let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue int)
        (Unix.getpid ()) Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

(*
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)
*)

let failf fmt = Format.kasprintf failwith fmt

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let src = Logs.Src.create "simple-client"

module Log = (val Logs.src_log src : Logs.LOG)

module P = Paf_mirage.Make (Time) (Tcpip_stack_socket.V4V6)
open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let ( <.> ) f g x = f (g x)

let response_handler th_err ~(f : Httpaf.Response.t -> string -> unit Lwt.t) _ :
    Alpn.response -> Alpn.body -> unit =
 fun resp body ->
  match (resp, body) with
  | Alpn.Response_HTTP_2_0 _, _ -> failf "Invalid protocol H2"
  | ( Alpn.Response_HTTP_1_1 response,
      Alpn.Body_HTTP_1_1 (Alpn.Rd, Alpn.Body_rd body) ) -> (
      let buf = Buffer.create 0x100 in
      let th, wk = Lwt.wait () in
      let on_eof () =
        Httpaf.Body.Reader.close body ;
        Lwt.wakeup_later wk () in
      let rec on_read payload ~off ~len =
        Buffer.add_string buf (Bigstringaf.substring payload ~off ~len) ;
        Httpaf.Body.Reader.schedule_read body ~on_eof ~on_read in
      Httpaf.Body.Reader.schedule_read body ~on_eof ~on_read ;
      Lwt.async @@ fun () ->
      Lwt.pick [ (th >|= fun () -> `Done); th_err ] >>= function
      | `Done -> f response (Buffer.contents buf)
      | _ ->
          Httpaf.Body.Reader.close body ;
          Lwt.return_unit)
  | _ -> assert false

let failf fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let error_handler wk _ (err : Alpn.client_error) =
  Lwt.wakeup_later wk (err :> [ `Body of string | `Done | Alpn.client_error ]) ;
  match err with
  | `Invalid_response_body_length_v1 _ | `Invalid_response_body_length_v2 _ ->
      failf "Invalid response body-length"
  | `Malformed_response _ -> failf "Malformed response"
  | `Exn exn -> raise exn
  | `Protocol_error (_error_code, _msg) -> failf "Protocol error"

let anchors = []

let null =
  let authenticator ?ip:_ ~host:_ _ = Ok None in
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
  | `HTTP -> Lwt.return_some (stack, ipaddr, port)
  | `HTTPS -> Lwt.return_none

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
       fold P.tcp_edn
         Fun.[ req scheme; req stack; req ipaddr; dft port 80 ]
         ~k:tcp_connect)
  |> Mimic.(
       fold P.tls_edn
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
    Lwt.return_unit in
  let th_err, (wk_err : [ `Body of string | `Done | Alpn.client_error ] Lwt.u) =
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
        Result.bind (Domain_name.of_string host) Domain_name.host )
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
  let ctx = Mimic.add stack (Tcpip_stack_socket.V4V6.tcp v) ctx in
  P.run ~ctx ~error_handler:(error_handler wk_err) ~response_handler
    (`V1 request)
  >>= function
  | Error err ->
      Log.err (fun m -> m "Got an error: %a." Mimic.pp_error err) ;
      Lwt.return_error err
  | Ok (Alpn.Body_HTTP_2_0 _) -> Lwt.return_error (`Msg "Invalid protocol (H2)")
  | Ok (Alpn.Body_HTTP_1_1 (Alpn.Wr, Alpn.Body_wr body)) -> (
      Httpaf.Body.Writer.close body ;
      Lwt.pick [ (th >|= fun body -> `Body body); th_err ] >>= function
      | `Body body -> Lwt.return_ok body
      | _ ->
          Httpaf.Body.Writer.close body ;
          Lwt.return_error (`Msg "Got an error while sending request"))
  | _ -> assert false
