let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue int)
        (Stdlib.Domain.self () :> int)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()

module TCP = struct
  type flow = Miou_unix.file_descr
  type error = Unix.error * string * string
  type write_error = [ `Closed | `Unix of Unix.error * string * string ]

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let pp_write_error ppf = function
    | `Closed -> Fmt.string ppf "Connection closed by peer"
    | `Unix (err, f, v) -> Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let read flow =
    let buf = Bytes.create 0x1000 in
    match Miou_unix.read flow buf ~off:0 ~len:(Bytes.length buf) with
    | 0 -> Ok `End_of_input
    | len -> Ok (`Data (Cstruct.of_string (Bytes.sub_string buf 0 len)))
    | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)

  let write flow ({ Cstruct.len; _ } as cs) =
    let str = Cstruct.to_string cs in
    try Ok (Miou_unix.write flow str ~off:0 ~len)
    with Unix.Unix_error (err, f, v) -> Error (`Unix (err, f, v))

  let writev flow css =
    let rec go = function
      | [] -> Ok ()
      | x :: r ->
      match write flow x with Ok () -> go r | Error _ as err -> err in
    go css

  let close = Miou_unix.close

  type endpoint = Unix.sockaddr

  let connect sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let socket =
      match domain with
      | Unix.PF_INET -> Miou_unix.tcpv4 ()
      | Unix.PF_INET6 -> Miou_unix.tcpv6 ()
      | Unix.PF_UNIX -> Fmt.invalid_arg "Invalid sockaddr" in
    try
      Miou_unix.connect socket sockaddr ;
      Ok socket
    with Unix.Unix_error (err, f, v) ->
      Miou_unix.close socket ;
      Error (`Unix (err, f, v))
end

module TLS = struct
  include Tls_miou.Make (TCP)

  type endpoint =
    [ `host ] Domain_name.t option * Tls.Config.client * TCP.endpoint

  let connect (host, config, sockaddr) =
    match TCP.connect sockaddr with
    | Ok flow -> client_of_flow ?host config flow
    | Error err -> Error (`Write err)
end

let tcp_endpoint, tcp_protocol = Mimice.register ~name:"tcp/ip" (module TCP)
let tls_endpoint, tls_protocol = Mimice.register ~name:"tls" (module TLS)

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

module TCP_repr = (val Mimice.repr tcp_protocol)
module TLS_repr = (val Mimice.repr tls_protocol)

exception HTTP_1_1_error of Httpaf.Client_connection.error

let pp_http_1_1_error ppf = function
  | `Malformed_response str -> Fmt.pf ppf "Malformed response: %s" str
  | `Invalid_response_body_length resp ->
      Fmt.pf ppf "Invalid response body length: @[<hov>%a@]"
        Httpaf.Response.pp_hum resp
  | `Exn exn -> Fmt.pf ppf "Got an exception: %S" (Printexc.to_string exn)

let () =
  Printexc.register_printer @@ function
  | HTTP_1_1_error err -> Some (Fmt.str "%a" pp_http_1_1_error err)
  | _ -> None

let decode_host_port ~default str =
  let ( >>= ) = Result.bind in
  match
    (Ipaddr.with_port_of_string ~default str, String.split_on_char ':' str)
  with
  | Ok (ipaddr, port), _ -> Ok (`Inet_addr ipaddr, port)
  | _, [] -> assert false
  | _, [ domain_name ] ->
      Domain_name.of_string domain_name >>= Domain_name.host
      >>= fun domain_name -> Ok (`Domain_name domain_name, default)
  | _, [ domain_name; port ] -> (
      Domain_name.of_string domain_name >>= Domain_name.host
      >>= fun domain_name ->
      try Ok (`Domain_name domain_name, int_of_string port)
      with _ -> Error (`Msg "Invalid port"))
  | _ -> Error (`Msg "Invalid host")

let decode_uri uri =
  (* proto :// user : pass @ host : port / path *)
  let ( >>= ) = Result.bind in
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
      (if String.equal proto "http:"
      then Ok ("http", false)
      else if String.equal proto "https:"
      then Ok ("https", true)
      else Error (`Msg "Unknown protocol"))
      >>= fun (scheme, is_tls) ->
      let decode_user_pass up =
        match String.split_on_char ':' up with
        | [ user; pass ] -> Ok (user, pass)
        | _ -> Error (`Msg "Couldn't decode user and password") in
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      let default_port = if is_tls then 443 else 80 in
      decode_host_port ~default:default_port host_port >>= fun (host, port) ->
      Ok (is_tls, scheme, user_pass, host, port, "/" ^ String.concat "/" path)
  | _ -> Error (`Msg "Couldn't decode URI on top")

let http_domain_name : [ `host ] Domain_name.t Mimice.value =
  Mimice.make ~name:"HTTP domain-name"

let http_port : int Mimice.value = Mimice.make ~name:"HTTP port"
let https : bool Mimice.value = Mimice.make ~name:"HTTPS"

let http_sockaddr : Unix.sockaddr Mimice.value =
  Mimice.make ~name:"HTTP sockaddr"

let ctx =
  let k0 sockaddr is_tls = if is_tls then None else Some sockaddr in
  let k1 domain_name is_tls port =
    let port =
      match port with Some port -> port | None -> if is_tls then 443 else 80
    in
    match Unix.gethostbyname (Domain_name.to_string domain_name) with
    | (exception _) | { Unix.h_addr_list = [||]; _ } -> None
    | { Unix.h_addr_list; _ } -> Some (Unix.ADDR_INET (h_addr_list.(0), port))
  in
  let k2 domain_name sockaddr is_tls =
    let authenticator = Result.get_ok (Ca_certs.authenticator ()) in
    let cfg = Tls.Config.client ~authenticator () in
    if is_tls then Some (domain_name, cfg, sockaddr) else None in
  Mimice.empty
  |> Mimice.fold tcp_endpoint Mimice.Fun.[ req http_sockaddr; req https ] ~k:k0
  |> Mimice.fold http_sockaddr
       Mimice.Fun.[ req http_domain_name; req https; opt http_port ]
       ~k:k1
  |> Mimice.fold tls_endpoint
       Mimice.Fun.[ opt http_domain_name; req http_sockaddr; req https ]
       ~k:k2

let ctx_from_uri uri ctx =
  match decode_uri uri with
  | Ok (is_tls, _scheme, _user_pass, domain_name, port, _path) ->
      let ctx =
        match domain_name with
        | `Domain_name domain_name ->
            Mimice.add http_domain_name domain_name ctx
        | `Inet_addr ipaddr ->
            let sockaddr =
              Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port) in
            Mimice.add http_sockaddr sockaddr ctx in
      let ctx = Mimice.add http_port port ctx in
      let ctx = Mimice.add https is_tls ctx in
      ctx
  | Error _ -> ctx

let run ~ctx f acc request =
  match (Mimice.resolve ctx, request) with
  | (Error _ as err), _ -> err
  | Ok flow, `V1 request ->
      let acc = ref acc in
      let response = ref None in
      let error = ref None in
      let response_handler resp body =
        let rec on_eof () = ()
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f !acc str ;
          Httpaf.Body.schedule_read body ~on_read ~on_eof in
        response := Some resp ;
        Httpaf.Body.schedule_read body ~on_read ~on_eof in
      let give =
        match flow with
        | TCP_repr.T socket -> [ Miou_unix.owner socket ]
        | TLS_repr.T socket -> [ Miou_unix.owner socket.TLS.flow ]
        | _ -> [] in
      let disown = function
        | TCP_repr.T socket -> Miou_unix.disown socket
        | TLS_repr.T socket -> Miou_unix.disown socket.TLS.flow
        | _ -> () in
      let error_handler err = error := Some err in
      let stream, conn =
        Httpaf.Client_connection.request ?config:None request ~error_handler
          ~response_handler in
      let prm =
        Miou.call_cc ~give @@ fun () ->
        Pafe.run ~give ~disown (module Httpaf_Client_connection) conn flow ;
        (* NOTE(dinosaure): we do [yield ()] to give a chance for the
           cancellation to operate if we got an error. Indeed, if we call
           [error_handler], we do the cancellation of this promise and we
           should not do the [Option.get] then. *)
        Miou.yield () ;
        try (Option.get !response, !acc)
        with _ -> raise (HTTP_1_1_error (Option.get !error)) in
      Ok (prm, stream)
  | _ -> assert false

let ( let* ) x f = Result.bind x f

let do_request () =
  let ctx = ctx_from_uri "https://google.com/" ctx in
  let request =
    let request = Httpaf.Request.create `GET "/" in
    `V1 request in
  let f buf str =
    Buffer.add_string buf str ;
    buf in
  let* prm, _stream = run ~ctx f (Buffer.create 0x100) request in
  let* response, buf =
    Result.map_error (fun exn -> `HTTP exn) (Miou.await prm) in
  let str = Buffer.contents buf in
  Ok (response, str)

let () =
  Miou_unix.run @@ fun () ->
  let rng = Rng.initialize (module Mirage_crypto_rng.Fortuna) in
  let () =
    match do_request () with
    | Ok (_response, str) ->
        Fmt.pr "@[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) str
    | Error (#Mimice.error as err) -> Fmt.epr "%a\n%!" Mimice.pp_error err
    | Error (`HTTP exn) -> Fmt.epr "exception: %S\n%!" (Printexc.to_string exn)
  in
  Miou.cancel rng
