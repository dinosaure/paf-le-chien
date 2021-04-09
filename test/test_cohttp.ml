open Lwt.Infix

let ( <.> ) f g x = f (g x)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()

let () = Logs.set_reporter (reporter Fmt.stderr)

let () = Logs.set_level ~all:true (Some Logs.Debug)

let () = Mirage_crypto_rng_unix.initialize ()

module P = Paf_mirage.Make (Time) (Tcpip_stack_socket.V4V6)

let unix_stack () =
  Tcpip_stack_socket.V4V6.UDP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun udpv4 ->
  Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun tcpv4 -> Tcpip_stack_socket.V4V6.connect udpv4 tcpv4

let error_handler (_ip, _port) ?request:_ _error _respond = ()

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Cstruct.of_bytes rs

let tls =
  let cert = load_file "server.pem" in
  let key = load_file "server.key" in
  match
    (X509.Certificate.decode_pem_multiple cert, X509.Private_key.decode_pem key)
  with
  | Ok certs, Ok (`RSA key) ->
      Tls.Config.server ~certificates:(`Single (certs, `RSA key)) ()
  | _ -> invalid_arg "Invalid certificate or key"

let sleep = Lwt_unix.sleep <.> Int64.to_float

let run_http_and_https_server ~request_handler stop =
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun stack ->
  P.init ~port:9090 stack >>= fun socket0 ->
  P.init ~port:3434 stack >>= fun socket1 ->
  let http = P.http_service ~error_handler request_handler in
  let https = P.https_service ~tls ~error_handler request_handler in
  let (`Initialized fiber0) = P.serve ~stop http socket0 in
  let (`Initialized fiber1) = P.serve ~stop https socket1 in
  Logs.debug (fun m -> m "Server initialised.") ;
  Lwt.async (fun () -> Lwt.join [ fiber0; fiber1 ]) ;
  Lwt.return_unit

let resolver domain_name =
  match Unix.gethostbyname (Domain_name.to_string domain_name) with
  | { Unix.h_addr_list; _ } ->
      if Array.length h_addr_list > 0
      then Lwt.return_some (Ipaddr_unix.of_inet_addr h_addr_list.(0))
      else Lwt.return_none
  | exception _ -> Lwt.return_none

let tcp_connect scheme stack ipaddr port =
  match scheme with
  | `HTTP -> Lwt.return_some (stack, ipaddr, port)
  | _ -> Lwt.return_none

let tls_connect scheme domain_name cfg stack ipaddr port =
  match scheme with
  | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port)
  | _ -> Lwt.return_none

let null =
  let authenticator ?ip:_ ~host:_ _ = Ok None in
  Tls.Config.client ~authenticator ()

module Client = Paf_cohttp

let stack = Mimic.make ~name:"stack"

let ctx =
  let tls = Mimic.make ~name:"tls" in
  Mimic.empty
  |> Mimic.(
       fold P.tcp_edn
         Fun.
           [
             req Paf_cohttp.scheme;
             req stack;
             req Paf_cohttp.ipaddr;
             dft Paf_cohttp.port 9090;
           ]
         ~k:tcp_connect)
  |> Mimic.(
       fold P.tls_edn
         Fun.
           [
             req Paf_cohttp.scheme;
             opt Paf_cohttp.domain_name;
             dft tls null;
             req stack;
             req Paf_cohttp.ipaddr;
             dft Paf_cohttp.port 3434;
           ]
         ~k:tls_connect)
  |> Mimic.(
       fold Paf_cohttp.ipaddr Fun.[ req Paf_cohttp.domain_name ] ~k:resolver)

let body_to_string body =
  let buf = Buffer.create 0x100 in
  let th, wk = Lwt.wait () in
  let on_eof () =
    Lwt.wakeup_later wk (Buffer.contents buf) ;
    Httpaf.Body.Reader.close body in
  let rec on_read str ~off ~len =
    let str = Bigstringaf.substring str ~off ~len in
    Logs.debug (fun m -> m "Received %S." str) ;
    Buffer.add_string buf str ;
    Httpaf.Body.Reader.schedule_read body ~on_eof ~on_read in
  Logs.debug (fun m -> m "Start to receive the body.") ;
  Httpaf.Body.Reader.schedule_read body ~on_eof ~on_read ;
  th

let query_to_assoc str =
  let lst =
    Astring.String.fields ~is_sep:(function '&' -> true | _ -> false) str in
  let f str =
    match Astring.String.cut ~sep:"=" str with
    | Some (k, v) -> (k, v)
    | None -> (str, "") in
  List.map f lst

let request_handler (ip, port) reqd =
  let open Httpaf in
  let req = Reqd.request reqd in
  Logs.debug (fun m ->
      m "Got a connection from %a:%d %s." Ipaddr.pp ip port req.Request.target) ;
  let body = Reqd.request_body reqd in
  match req.Request.target with
  | "/" ->
      let contents = "Hello World!" in
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int (String.length contents)) ] in
      let resp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd resp contents ;
      Lwt.async @@ fun () ->
      body_to_string body >>= fun _ ->
      Logs.debug (fun m -> m "Body drained.") ;
      Lwt.return_unit
  | "/repeat" ->
      Lwt.async @@ fun () ->
      body_to_string body >>= fun str ->
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int (String.length str)) ] in
      let resp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd resp str ;
      Lwt.return_unit
  | target ->
  match Astring.String.cut ~sep:"?" target with
  | Some ("/query", query) ->
      let lst = query_to_assoc query in
      let buf = Buffer.create 0x100 in
      let ppf = Format.formatter_of_buffer buf in
      Fmt.pf ppf "%a%!"
        Fmt.(list ~sep:(any ";") (pair ~sep:(any "=") string string))
        lst ;
      let contents = Buffer.contents buf in
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int (String.length contents)) ] in
      let resp = Response.create ~headers `OK in
      Reqd.respond_with_string reqd resp contents ;
      Lwt.async @@ fun () ->
      body_to_string body >>= fun _ -> Lwt.return_unit
  | _ ->
      Reqd.report_exn reqd Not_found ;
      let contents = "Invalid request." in
      let headers =
        Headers.of_list
          [ ("content-length", string_of_int (String.length contents)) ] in
      let resp = Response.create ~headers `Bad_request in
      Reqd.respond_with_string reqd resp contents

let test01 =
  Alcotest_lwt.test_case "simple-http" `Quick @@ fun _sw () ->
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  Client.get ~ctx (Uri.of_string "http://localhost:9090/")
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Alcotest.(check string) "contents" str "Hello World!" ;
  Lwt.return_unit

let test02 =
  Alcotest_lwt.test_case "repeat" `Quick @@ fun _sw () ->
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  let body = Cohttp_lwt.Body.of_string "Hello!" in
  Client.post ~ctx ~body (Uri.of_string "http://localhost:9090/repeat")
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Alcotest.(check string) "contents" str "Hello!" ;
  Lwt.return_unit

let test03 =
  Alcotest_lwt.test_case "simple-https" `Quick @@ fun _sw () ->
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  Client.get ~ctx (Uri.of_string "https://localhost:3434/")
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Alcotest.(check string) "contents" str "Hello World!" ;
  Lwt.return_unit

let test04 =
  Alcotest_lwt.test_case "repeat (https)" `Quick @@ fun _sw () ->
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  let body = Cohttp_lwt.Body.of_string "Secret Hello!" in
  Client.post ~ctx ~body (Uri.of_string "https://localhost:3434/repeat")
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Alcotest.(check string) "contents" str "Secret Hello!" ;
  Lwt.return_unit

let test05 =
  Alcotest_lwt.test_case "queries" `Quick @@ fun _sw () ->
  unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  Client.get ~ctx (Uri.of_string "https://localhost:3434/query?foo=a&bar=b")
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Alcotest.(check string) "contents" str "foo=a;bar=b" ;
  Lwt.return_unit

let test () =
  Alcotest_lwt.run "smart"
    [ ("cohttp", [ test01; test02; test03; test04; test05 ]) ]

let () =
  let fiber =
    Lwt_switch.with_switch @@ fun stop ->
    run_http_and_https_server ~request_handler stop >>= test >>= fun () ->
    Lwt_switch.turn_off stop in
  Lwt_main.run fiber
