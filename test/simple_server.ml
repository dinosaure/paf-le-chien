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

let sigpipe = 13

let () = Mirage_crypto_rng_unix.initialize ()

(*
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
*)

let () = Sys.set_signal sigpipe Sys.Signal_ignore

let src = Logs.Src.create "simple-server"

module Log = (val Logs.src_log src : Logs.LOG)

module P = Paf_mirage.Make (Time) (Tcpip_stack_socket.V4V6)
module Ke = Ke.Rke

let getline queue =
  let exists ~predicate queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.iter
      (fun chr ->
        if predicate chr then res := !pos ;
        incr pos)
      queue ;
    if !res = -1 then None else Some !res in
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
  match exists ~predicate:(( = ) '\n') queue with
  | Some pos ->
      let tmp = Bytes.create pos in
      Ke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
      Ke.N.shift_exn queue (pos + 1) ;
      Some (Bytes.unsafe_to_string tmp)
  | None -> None

let http_large filename (_ip, _port) ic oc =
  let open Httpaf in
  Body.Reader.close ic ;
  let ic = open_in filename in
  let tp = Bytes.create 0x1000 in
  let rec go () =
    match input ic tp 0 (Bytes.length tp) with
    | 0 -> Body.Writer.close oc
    | len ->
        Body.Writer.write_string oc (Bytes.sub_string tp 0 len) ;
        go ()
    | exception End_of_file -> Body.Writer.close oc in
  go () ;
  close_in ic

let http_ping_pong (_ip, _port) ic oc =
  let open Httpaf in
  let open Lwt.Infix in
  let closed = ref false and queue = Ke.create ~capacity:0x1000 Bigarray.char in

  let blit src src_off dst dst_off len =
    Bigstringaf.blit src ~src_off dst ~dst_off ~len in
  let on_eof () = closed := true in
  let rec on_read buf ~off ~len =
    Ke.N.push queue ~blit ~length:Bigstringaf.length buf ~off ~len ;
    Body.Reader.schedule_read ic ~on_eof ~on_read in
  Body.Reader.schedule_read ic ~on_eof ~on_read ;
  let rec go () =
    match (!closed, getline queue) with
    | false, None -> Lwt.pause () >>= go
    | false, Some "ping" ->
        Body.Writer.write_string oc "pong\n" ;
        go ()
    | false, Some "pong" ->
        Body.Writer.write_string oc "ping\n" ;
        go ()
    | false, Some _line ->
        Body.Writer.close oc ;
        Lwt.return_unit
    | true, _ ->
        Body.Writer.close oc ;
        Lwt.return_unit in
  Lwt.async go

let request_handler large (ip, port) reqd =
  let open Httpaf in
  let request = Reqd.request reqd in
  match request.Request.target with
  | "/" ->
      let headers = Headers.of_list [ ("transfer-encoding", "chunked") ] in
      let response = Response.create ~headers `OK in
      let oc = Reqd.respond_with_streaming reqd response in
      http_ping_pong (ip, port) (Reqd.request_body reqd) oc
  | "/ping" ->
      let headers = Headers.of_list [ ("content-length", "4") ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response "pong"
  | "/pong" ->
      let headers = Headers.of_list [ ("content-length", "4") ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response "ping"
  | "/large" ->
      let headers = Headers.of_list [ ("transfer-encoding", "chunked") ] in
      let response = Response.create ~headers `OK in
      let oc = Reqd.respond_with_streaming reqd response in
      http_large large (ip, port) (Reqd.request_body reqd) oc
  | _ -> assert false

let error_handler _ ?request:_ error _respond =
  match error with
  | `Exn _exn -> Printexc.print_backtrace stderr
  | `Bad_gateway -> Fmt.epr "Got a bad gateway error.\n%!"
  | `Bad_request -> Fmt.epr "Got a bad request error.\n%!"
  | `Internal_server_error -> Fmt.epr "Got an internal server error.\n%!"

let ( <.> ) f g x = f (g x)

open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

let fd_8080 = Unix.openfile "lock.8080" Unix.[ O_CREAT; O_RDWR ] 0o644

let () = at_exit (fun () -> try Unix.close fd_8080 with _exn -> ())

let fd_4343 = Unix.openfile "lock.4343" Unix.[ O_CREAT; O_RDWR ] 0o644

let () = at_exit (fun () -> try Unix.close fd_4343 with _exn -> ())

let unlock fd = Unix.lockf fd Unix.F_ULOCK 0

let server_http large stack =
  P.init ~port:8080 stack >>= fun service ->
  let http = P.http_service ~error_handler (request_handler large) in
  let (`Initialized th) = P.serve http service in
  unlock fd_8080 ;
  Log.debug (fun m -> m "HTTP server initialized.") ;
  th

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Cstruct.of_bytes rs

let server_https cert key large stack =
  let cert = load_file cert in
  let key = load_file key in
  match
    (X509.Certificate.decode_pem_multiple cert, X509.Private_key.decode_pem key)
  with
  | Ok certs, Ok (`RSA key) ->
      let tls = Tls.Config.server ~certificates:(`Single (certs, `RSA key)) () in
      P.init ~port:4343 stack >>= fun service ->
      let https = P.https_service ~tls ~error_handler (request_handler large) in
      let (`Initialized th) = P.serve https service in
      unlock fd_4343 ;
      th
  | _ -> invalid_arg "Invalid certificate or key"

let stack =
  Tcpip_stack_socket.V4V6.UDP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun udpv4 ->
  Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun tcpv4 -> Tcpip_stack_socket.V4V6.connect udpv4 tcpv4

let run_http large = stack >|= Tcpip_stack_socket.V4V6.tcp >>= server_http large

let run_https cert key large =
  stack >|= Tcpip_stack_socket.V4V6.tcp >>= server_https cert key large

let () =
  match Sys.argv with
  | [| _; "--with-tls"; cert; key; large |] ->
      Lwt_main.run (run_https cert key large)
  | [| _; large |] -> Lwt_main.run (run_http large)
  | _ -> Fmt.epr "%s [--with-tls cert key] large\n%!" Sys.argv.(0)
