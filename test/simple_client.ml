let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string) (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report } 

let () = Mirage_crypto_rng_unix.initialize ()
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

module Paf = Paf.Make(Time)(Tcpip_stack_socket)

open Lwt.Infix

let ( >>? ) x f = x >>= function
  | Ok x -> f x
  | Error err -> Lwt.return_error err

let ( <.> ) f g = fun x -> f (g x)

let response_handler th_err ~f _ response body =
  let buf = Buffer.create 0x100 in
  let th, wk = Lwt.wait () in
  let on_eof () =
    Format.eprintf "[#] on eof.\n%!" ;
    Httpaf.Body.close_reader body ;
    Lwt.wakeup_later wk () in
  let rec on_read payload ~off ~len =
    Format.eprintf "[#] on read (%d byte(s)).\n%!" len ;
    Buffer.add_string buf (Bigstringaf.substring payload ~off ~len) ;
    Httpaf.Body.schedule_read body ~on_eof ~on_read in
  Httpaf.Body.schedule_read body ~on_eof ~on_read ;
  Lwt.async @@ fun () ->
  Lwt.pick
    [ (th >|= fun () -> `Done)
    ; th_err ] >>= function
  | `Done -> f response (Buffer.contents buf)
  | _ -> Lwt.return_unit

let failf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let error_handler wk _ _ err =
  Lwt.wakeup_later wk (err :> [ `Body of string | `Done | Httpaf.Client_connection.error ]);
  Format.eprintf "Got an error while sending request.\n%!" ;
  match err with
  | `Exn (Paf.Send_error err)
  | `Exn (Paf.Recv_error err)
  | `Exn (Paf.Close_error err) ->
    failf "Impossible to start a transmission: %s" err
  | `Invalid_response_body_length _ ->
    failf "Invalid response body-length"
  | `Malformed_response _ ->
    failf "Malformed response"
  | `Exn _exn -> ()

let http_resolver stack ?(port= 80) domain_name =
  Lwt_unix.gethostbyname (Domain_name.to_string domain_name) >>= function
  | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
    Lwt.return_some { Conduit_mirage_tcp.stack
                    ; keepalive= None
                    ; nodelay= false
                    ; ip= Ipaddr_unix.V4.of_inet_addr_exn h_addr_list.(0)
                    ; port }
  | _ -> Lwt.return_none

let anchors = []

let authenticator _expect ~host crts =
  let crts = X509.Validation.valid_cas crts in
  match X509.Validation.verify_chain
         ~host ~time:(fun () -> Some (Ptime_clock.now ()))
         ~anchors crts with
  | Ok crt -> Ok (Some ([], crt))
  | Error _ -> Ok None

let https_resolver stack ?(port= 443) domain_name =
  http_resolver stack domain_name >>= function
  | Some config ->
    let tls_config = Tls.Config.client ~authenticator:(authenticator domain_name) () in
    Lwt.return_some ({ config with port }, tls_config)
  | None -> Lwt.return_none

let stack ip =
  Tcpip_stack_socket.UDPV4.connect (Some ip) >>= fun udpv4 ->
  Tcpip_stack_socket.TCPV4.connect (Some ip) >>= fun tcpv4 ->
  Tcpip_stack_socket.connect [ ip ] udpv4 tcpv4

let run uri =
  stack Ipaddr.V4.localhost >>= fun stack ->
  let th, wk = Lwt.wait () in
  let port = Uri.port uri in
  let f _ body = Lwt.wakeup_later wk body ; Lwt.return () in
  let th_err, (wk_err : [ `Body of string | `Done | Httpaf.Client_connection.error ] Lwt.u) = Lwt.wait () in
  match Uri.scheme uri, Uri.host uri with
  | Some "https", Some hostname ->
    let headers = Httpaf.Headers.of_list [ "Host", hostname ] in
    let hostname = Domain_name.(host_exn <.> of_string_exn) hostname in
    let request = Httpaf.Request.create ~headers `GET (Uri.path uri) in
    let response_handler = response_handler th_err ~f in
    let resolvers = Conduit_mirage.register_resolver ~key:Paf.tls_endpoint (https_resolver ?port stack) Conduit.empty in
    Paf.request ~key:Paf.tls_endpoint
      ~resolvers
      ~error_handler:(error_handler wk_err)
      ~response_handler
      hostname request >>? fun body ->
    Httpaf.Body.close_writer body ;
    ( Lwt.pick [ (th >|= fun body -> `Body body)
               ; th_err ] >>= function
      | `Body body -> Lwt.return_ok body
      | _ -> Lwt.return_error (`Msg "Got an error while sending request") )
  | Some "http", Some hostname ->
    let headers = Httpaf.Headers.of_list [ "Host", hostname ] in
    let hostname = Domain_name.(host_exn <.> of_string_exn) hostname in
    let request = Httpaf.Request.create ~headers `GET (Uri.path uri) in
    let response_handler = response_handler th_err ~f in
    let resolvers = Conduit_mirage.register_resolver ~key:Paf.TCP.endpoint (http_resolver ?port stack) Conduit.empty in
    Paf.request ~key:Paf.TCP.endpoint
      ~resolvers
      ~error_handler:(error_handler wk_err)
      ~response_handler
      hostname request >>? fun body ->
    Httpaf.Body.close_writer body ;
    ( Lwt.pick [ (th >|= fun body -> `Body body)
               ; th_err ] >>= function
      | `Body body -> Lwt.return_ok body
      | _ -> Lwt.return_error (`Msg "Got an error while sending request") )
  | _, _ -> failwith "Invalid uri: %a" Uri.pp uri

let () = match Sys.argv with
  | [| _; uri; |] ->
    ( match Lwt_main.run (run (Uri.of_string uri)) with
    | Ok body -> print_string body
    | Error err -> Format.eprintf "%s: %a\n%!" Sys.argv.(0) Conduit_mirage.pp_error err )
  | _ ->
    Format.eprintf "%s <uri>\n%!" Sys.argv.(0)
