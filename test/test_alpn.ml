open Lwt.Infix

let ( <.> ) f g x = f (g x)

let ( >>? ) = Lwt_result.bind

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
      Tls.Config.server ~alpn_protocols:[ "http/1.1"; "h2" ]
        ~certificates:(`Single (certs, `RSA key))
        ()
  | _ -> invalid_arg "Invalid certificate or key"

let error_handler _edn ?request:_ _err _respond = ()

let alpn_of_tls_connection (_, flow) =
  match P.TLS.epoch flow with
  | Ok { Tls.Core.alpn_protocol; _ } ->
      Fmt.epr ">>> alpn_protocol (server side): %a.\n%!"
        Fmt.(option string)
        alpn_protocol ;
      alpn_protocol
  | Error _ -> None

let peer_of_tls_connection ((ipaddr, port), _) =
  Fmt.str "%a:%d" Ipaddr.pp ipaddr port

let injection =
  let module R = (val Mimic.repr P.tls_protocol) in
  fun (_, flow) -> R.T flow

let port =
  let v = ref 9999 in
  fun () ->
    incr v ;
    !v

let service ~request_handler () =
  let info =
    {
      Alpn.alpn = alpn_of_tls_connection;
      Alpn.peer = peer_of_tls_connection;
      Alpn.injection;
    } in
  let accept t =
    P.accept t >>= function
    | Error _ as err -> Lwt.return err
    | Ok flow -> (
        let edn = Tcpip_stack_socket.V4V6.TCP.dst flow in
        P.TLS.server_of_flow tls flow >>= function
        | Ok flow -> Lwt.return_ok (edn, flow)
        | Error err ->
            Lwt.return_error (`Msg (Fmt.str "%a" P.TLS.pp_write_error err)))
  and close = P.close in
  Alpn.service info ~error_handler ~request_handler accept close

module R = (val Mimic.repr P.tls_protocol)

let error_handler () _resp = ()

let client ~ctx ~response_handler req =
  Mimic.resolve ctx >>= function
  | Error err -> Alcotest.failf "%a" Mimic.pp_error err
  | Ok (R.T v as flow) -> (
      let alpn =
        match P.TLS.epoch v with
        | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
        | Error _ -> None in
      Alpn.run ~sleep:Time.sleep_ns ?alpn ~error_handler ~response_handler ()
        req flow
      >>= function
      | Ok body -> Lwt.return body
      | Error err -> Alcotest.failf "%a" Mimic.pp_error err)
  | Ok flow -> (
      Alpn.run ~sleep:Time.sleep_ns ~error_handler ~response_handler () req flow
      >>= function
      | Ok body -> Lwt.return body
      | Error err -> Alcotest.failf "%a" Mimic.pp_error err)

let ctx_with_tls stack ~port tls =
  let ipaddr = Ipaddr_unix.of_inet_addr Unix.inet_addr_loopback in
  Mimic.add P.tls_edn (None, tls, stack, ipaddr, port) Mimic.empty

let authenticator ?ip:_ ~host:_ _ = Ok None

type version = HTTP_1_1 | HTTP_2_0

let test01 =
  Alcotest_lwt.test_case "http/1.1" `Quick @@ fun _sw () ->
  let port = port () in
  let stop = Lwt_switch.create () in
  let th, wk = Lwt.wait () in
  let request, wk_request = Lwt.wait () in
  let request_handler _edn : Alpn.reqd -> unit = function
    | Alpn.Reqd_HTTP_1_1 _reqd ->
        Lwt.wakeup_later wk_request HTTP_1_1 ;
        Lwt.wakeup_later wk ()
    | Alpn.Reqd_HTTP_2_0 _reqd ->
        Lwt.wakeup_later wk_request HTTP_2_0 ;
        Lwt.wakeup_later wk () in
  let response_handler () : Alpn.response -> Alpn.body -> unit = fun _ _ -> () in
  let service = service ~request_handler () in
  let tls = Tls.Config.client ~authenticator ~alpn_protocols:[ "http/1.1" ] () in
  let req = `V1 (Httpaf.Request.create `GET "/") in
  Lwt.both
    ( unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun stack ->
      P.init ~port stack >>= fun t ->
      P.serve ~stop service t |> fun (`Initialized th) ->
      let ctx = ctx_with_tls stack ~port tls in
      Lwt.both (client ~ctx ~response_handler req) th )
    (th >>= fun () -> Lwt_switch.turn_off stop)
  >>= fun ((body, ()), ()) ->
  request >>= fun request ->
  match (request, body) with
  | HTTP_1_1, Alpn.Body_HTTP_1_1 _ ->
      Alcotest.(check pass) "http/1.1" () () ;
      Lwt.return_unit
  | _ -> Alcotest.failf "Unexpected version of HTTP"

let test02 =
  Alcotest_lwt.test_case "h2" `Quick @@ fun _sw () ->
  let port = port () in
  let stop = Lwt_switch.create () in
  let th, wk = Lwt.wait () in
  let request, wk_request = Lwt.wait () in
  let request_handler _edn : Alpn.reqd -> unit = function
    | Alpn.Reqd_HTTP_1_1 _ ->
        Lwt.wakeup_later wk_request HTTP_1_1 ;
        Lwt.wakeup_later wk ()
    | Alpn.Reqd_HTTP_2_0 _ ->
        Lwt.wakeup_later wk_request HTTP_2_0 ;
        Lwt.wakeup_later wk () in
  let response_handler () : Alpn.response -> Alpn.body -> unit = fun _ _ -> () in
  let service = service ~request_handler () in
  let tls = Tls.Config.client ~authenticator ~alpn_protocols:[ "h2" ] () in
  let req = `V2 (H2.Request.create ~scheme:"https" `GET "/") in
  Lwt.both
    ( unix_stack () >|= Tcpip_stack_socket.V4V6.tcp >>= fun stack ->
      P.init ~port stack >>= fun t ->
      P.serve ~stop service t |> fun (`Initialized th) ->
      let ctx = ctx_with_tls stack ~port tls in
      Lwt.both (client ~ctx ~response_handler req) th )
    (th >>= fun () -> Lwt_switch.turn_off stop)
  >>= fun ((body, ()), ()) ->
  request >>= fun request ->
  match (request, body) with
  | HTTP_2_0, Alpn.Body_HTTP_2_0 _ ->
      Alcotest.(check pass) "h2" () () ;
      Lwt.return_unit
  | _ -> Alcotest.failf "Unexpected version of HTTP"

let test () = Alcotest_lwt.run "alpn" [ ("alpn", [ test01; test02 ]) ]

let () = Lwt_main.run (test ())
