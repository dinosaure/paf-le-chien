(*
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)
*)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

module Paf = Paf.Make (Tcpip_stack_socket.V4V6)
open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let ( <.> ) f g x = f (g x)

let response_handler th_err ~f _ response body =
  let buf = Buffer.create 0x100 in
  let th, wk = Lwt.wait () in
  let on_eof () =
    Httpaf.Body.close_reader body ;
    Lwt.wakeup_later wk () in
  let rec on_read payload ~off ~len =
    Buffer.add_string buf (Bigstringaf.substring payload ~off ~len) ;
    Httpaf.Body.schedule_read body ~on_eof ~on_read in
  Httpaf.Body.schedule_read body ~on_eof ~on_read ;
  Lwt.async @@ fun () ->
  Lwt.pick [ (th >|= fun () -> `Done); th_err ] >>= function
  | `Done -> f response (Buffer.contents buf)
  | _ ->
      Httpaf.Body.close_reader body ;
      Lwt.return_unit

let failf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let error_handler wk _ _ err =
  Lwt.wakeup_later wk
    (err :> [ `Body of string | `Done | Httpaf.Client_connection.error ]) ;
  match err with
  | `Exn (Paf.Error err) ->
      failf "Impossible to start a transmission: %a" Mimic.pp_error err
  | `Invalid_response_body_length _ -> failf "Invalid response body-length"
  | `Malformed_response _ -> failf "Malformed response"
  | `Exn exn -> raise exn

let anchors = []

let null =
  let authenticator ~host:_ _ = Ok None in
  Tls.Config.client ~authenticator ()

let v =
  Tcpip_stack_socket.V4V6.UDP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun udpv4 ->
  Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None
  >>= fun tcpv4 -> Tcpip_stack_socket.V4V6.connect udpv4 tcpv4

let stack = Mimic.make ~name:"stack"

let ipaddr = Mimic.make ~name:"ipaddr"

let port = Mimic.make ~name:"port"

let domain_name = Mimic.make ~name:"domain-name"

let scheme = Mimic.make ~name:"scheme"

let tls = Mimic.make ~name:"tls"

let tcp_connect scheme stack ipaddr port =
  match scheme with
  | `HTTPS -> Lwt.return_some (stack, ipaddr, port)
  | `HTTP -> Lwt.return_none

let dns_resolve domain_name =
  match Unix.gethostbyname (Domain_name.to_string domain_name) with
  | { Unix.h_addr_list; _ } ->
      if Array.length h_addr_list > 0
      then Lwt.return_some (Ipaddr_unix.of_inet_addr h_addr_list.(0))
      else Lwt.return_none
  | exception _ -> Lwt.return_none

let tls_connect scheme domain_name cfg stack ipaddr port =
  match scheme with
  | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port)
  | `HTTP -> Lwt.return_none

let ctx =
  Mimic.empty
  |> Mimic.(
       fold Paf.tcp_edn
         Fun.[ req scheme; req stack; req ipaddr; dft port 80 ]
         ~k:tcp_connect)
  |> Mimic.(
       fold Paf.tls_edn
         Fun.
           [
             req scheme;
             opt domain_name;
             dft tls null;
             req stack;
             req ipaddr;
             dft port 443;
           ]
         ~k:tls_connect)
  |> Mimic.(fold ipaddr Fun.[ req domain_name ] ~k:dns_resolve)

let run uri =
  let th, wk = Lwt.wait () in
  let f _ body =
    Lwt.wakeup_later wk body ;
    Lwt.return () in
  let ( th_err,
        (wk_err :
          [ `Body of string | `Done | Httpaf.Client_connection.error ] Lwt.u) )
      =
    Lwt.wait () in
  let ctx =
    match Uri.scheme uri with
    | Some "http" -> Mimic.add scheme `HTTP ctx
    | Some "https" -> Mimic.add scheme `HTTPS ctx
    | _ -> ctx in
  let ctx, hostname =
    match Uri.host uri with
    | None -> (ctx, None)
    | Some host ->
    match
      ( Ipaddr.of_string host,
        Rresult.(Domain_name.of_string host >>= Domain_name.host) )
    with
    | Ok v0, Ok v1 ->
        (ctx |> Mimic.add ipaddr v0 |> Mimic.add domain_name v1, Some host)
    | Ok v, _ -> (ctx |> Mimic.add ipaddr v, Some host)
    | _, Ok v -> (ctx |> Mimic.add domain_name v, Some host)
    | _ -> (ctx, Some host) in
  let ctx =
    match Uri.port uri with Some v -> Mimic.add port v ctx | None -> ctx in
  let headers =
    Option.fold ~none:Httpaf.Headers.empty
      ~some:(fun hostname -> Httpaf.Headers.of_list [ ("Host", hostname) ])
      hostname in
  let request = Httpaf.Request.create ~headers `GET (Uri.path uri) in
  let response_handler = response_handler th_err ~f in
  v >>= fun v ->
  let ctx = Mimic.add stack v ctx in
  Paf.request
    ~sleep:(Lwt_unix.sleep <.> Int64.to_float)
    ~ctx ~error_handler:(error_handler wk_err) ~response_handler request
  >>? fun body ->
  Httpaf.Body.close_writer body ;
  Lwt.pick [ (th >|= fun body -> `Body body); th_err ] >>= function
  | `Body body -> Lwt.return_ok body
  | _ ->
      Httpaf.Body.close_writer body ;
      Lwt.return_error (`Msg "Got an error while sending request")
