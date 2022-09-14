let http_scheme = Mimic.make ~name:"http-scheme"
let http_port = Mimic.make ~name:"http-port"
let http_hostname = Mimic.make ~name:"http-hostname"
let tls_config = Mimic.make ~name:"tls-config"

open Lwt.Infix

module type S = sig
  val connect : Mimic.ctx -> Mimic.ctx Lwt.t
  val alpn_protocol : Mimic.flow -> string option
  val authenticator : (X509.Authenticator.t, [> `Msg of string ]) result
end

module Make
  (Pclock : Mirage_clock.PCLOCK)
  (TCP : Tcpip.Tcp.S)
  (Happy_eyeballs : Mimic_happy_eyeballs.S with type flow = TCP.flow) : S = struct
  module TCP = struct
    include TCP
    type endpoint = Happy_eyeballs.t * string * int
    type nonrec write_error =
      [ `Write of write_error | `Connect of string | `Closed ]
    let pp_write_error ppf = function
      | `Connect err -> Fmt.string ppf err
      | `Write err -> pp_write_error ppf err
      | `Closed as err -> pp_write_error ppf err

    let write flow cs =
      let open Lwt.Infix in
      write flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect (happy_eyeballs, hostname, port) =
      Happy_eyeballs.resolve happy_eyeballs hostname [ port ] >>= function
      | Error (`Msg err) -> Lwt.return_error (`Connect err)
      | Ok ((_ipaddr, _port), flow) -> Lwt.return_ok flow
  end

  let tcp_edn, _tcp_protocol = Mimic.register ~name:"tcp" (module TCP)

  module TLS = struct
    type endpoint = Happy_eyeballs.t * Tls.Config.client * string * int

    include Tls_mirage.Make (TCP)

    let connect (happy_eyeballs, cfg, hostname, port) =
      let peer_name =
        Result.(to_option (bind (Domain_name.of_string hostname) Domain_name.host)) in
      Happy_eyeballs.resolve happy_eyeballs hostname [ port ] >>= function
      | Ok ((_ipaddr, _port), flow) -> client_of_flow cfg ?host:peer_name flow
      | Error (`Msg err) -> Lwt.return_error (`Write (`Connect err))
  end

  let tls_edn, tls_protocol =
    Mimic.register ~name:"tls" (module TLS)

  let connect ctx =
    let k0 happy_eyeballs http_scheme http_hostname http_port = match http_scheme with
      | "http" -> Lwt.return_some (happy_eyeballs, http_hostname, http_port)
      | _      -> Lwt.return_none in
    let k1 happy_eyeballs http_scheme http_hostname http_port tls_config = match http_scheme with
      | "https" -> Lwt.return_some (happy_eyeballs, tls_config, http_hostname, http_port)
      | _       -> Lwt.return_none in
    let ctx = Mimic.fold tcp_edn
      Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs
                ; req http_scheme; req http_hostname; dft http_port 80 ]
      ~k:k0 ctx in
    let ctx = Mimic.fold tls_edn
      Mimic.Fun.[ req Happy_eyeballs.happy_eyeballs
                ; req http_scheme; req http_hostname; dft http_port 443
                ; req tls_config ]
      ~k:k1 ctx in
    Lwt.return ctx

  let alpn_protocol flow =
    let module M = (val (Mimic.repr tls_protocol)) in
    match flow with
    | M.T flow ->
      ( match TLS.epoch flow with
      | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
      | Error _ -> None )
    | _ -> None

  let authenticator =
    let module V = Ca_certs_nss.Make (Pclock) in
    V.authenticator ()
end

module Version = Httpaf.Version
module Status = H2.Status
module Headers = H2.Headers

type response =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }

module HTTP_1_1 = struct
  include Httpaf.Client_connection
  let yield_reader _ = assert false
  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

let add_authentication ~add headers = function
  | None -> headers
  | Some (user, pass) ->
    let data = Base64.encode_string (user ^ ":" ^ pass) in
    add headers "authorization" ("Basic " ^ data)

let prepare_http_1_1_headers headers host user_pass body_length =
  let headers = Httpaf.Headers.of_list headers in
  let add = Httpaf.Headers.add_unless_exists in
  let headers = add headers "user-agent" ("http-mirage-client/%%VERSION%%") in
  let headers = add headers "host" host in
  let headers = add headers "connection" "close" in
  let headers = match body_length with
    | None -> headers
    | Some v -> add headers "content-length" (string_of_int v) in
  add_authentication ~add headers user_pass

let single_http_1_1_request ?config flow user_pass host meth path headers body =
  let body_length = Option.map String.length body in
  let headers = prepare_http_1_1_headers headers host user_pass body_length in
  let req = Httpaf.Request.create ~headers meth path in
  let finished, notify_finished = Lwt.wait () in
  let wakeup = let w = ref false in
    fun v -> if not !w then Lwt.wakeup_later notify_finished v ; w := true in
  let response_handler response body =
    let buf = Buffer.create 0x100 in
    let rec on_eof () =
      let response =
        { version= response.Httpaf.Response.version
        ; status = (response.Httpaf.Response.status :> H2.Status.t)
        ; reason = response.Httpaf.Response.reason
        ; headers= H2.Headers.of_list (Httpaf.Headers.to_list response.Httpaf.Response.headers) } in
      wakeup (Ok (response, Some (Buffer.contents buf)))
    and on_read ba ~off ~len =
      Buffer.add_string buf (Bigstringaf.substring ~off ~len ba) ;
      Httpaf.Body.schedule_read body ~on_read ~on_eof in
    let on_eof () =
      let response =
        { version= response.Httpaf.Response.version
        ; status = (response.Httpaf.Response.status :> H2.Status.t)
        ; reason = response.Httpaf.Response.reason
        ; headers= H2.Headers.of_list (Httpaf.Headers.to_list response.Httpaf.Response.headers) } in
      wakeup (Ok (response, None)) in
    Httpaf.Body.schedule_read body ~on_read ~on_eof in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("Malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg ("Invalid response body length"))
      | `Exn e -> Error (`Msg ("Exception here: " ^ Printexc.to_string e)) in
    wakeup err in
  let request_body, conn = Httpaf.Client_connection.request ?config req ~error_handler
    ~response_handler in
  Lwt.async (fun () -> Paf.run (module HTTP_1_1) conn flow) ;
  Option.iter (Httpaf.Body.write_string request_body) body ;
  Httpaf.Body.close_writer request_body ;
  finished

let prepare_h2_headers headers host user_pass body_length =
  let headers = H2.Headers.of_list headers in
  let add hdr = H2.Headers.add_unless_exists hdr ?sensitive:None in
  let headers = add headers ":authority" host in
  let headers = add headers "content-length" (string_of_int (Option.value ~default:0 body_length)) in
  add_authentication ~add headers user_pass

let single_h2_request ?config ~scheme flow user_pass host meth path headers body =
  let body_length = Option.map String.length body in
  let headers = prepare_h2_headers headers host user_pass body_length in
  let req = H2.Request.create ~scheme ~headers meth path in
  let finished, notify_finished = Lwt.wait () in
  let wakeup = let w = ref false in
    fun v -> if not !w then Lwt.wakeup_later notify_finished v ; w := true in
  let response_handler response response_body =
    let buf = Buffer.create 0x100 in
    let rec on_eof () =
      let response =
        { version= { major= 2; minor= 0; }
        ; status = response.H2.Response.status
        ; reason = ""
        ; headers= response.H2.Response.headers } in
      wakeup (Ok (response, Some (Buffer.contents buf)))
    and on_read ba ~off ~len =
      Buffer.add_string buf (Bigstringaf.substring ~off ~len ba) ;
      H2.Body.Reader.schedule_read response_body
        ~on_read ~on_eof in
    let on_eof () =
      let response =
        { version= { major= 2; minor= 0; }
        ; status = response.H2.Response.status
        ; reason = ""
        ; headers= response.H2.Response.headers } in
      wakeup (Ok (response, None)) in
    H2.Body.Reader.schedule_read response_body
      ~on_read ~on_eof in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("Malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg "Invalid response body length")
      | `Protocol_error (err, msg) ->
        let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
        Format.kfprintf kerr Format.str_formatter "%a: %s" H2.Error_code.pp_hum err msg
      | `Exn e -> Error (`Msg ("Exception here: " ^ Printexc.to_string e)) in
    wakeup err in
  let conn = H2.Client_connection.create ?config ?push_handler:None
    ~error_handler in
  let request_body = H2.Client_connection.request conn req ~error_handler ~response_handler in
  Lwt.async (fun () -> Paf.run (module H2.Client_connection) conn flow) ;
  Option.iter (H2.Body.Writer.write_string request_body) body ;
  H2.Body.Writer.close request_body ;
  finished >|= fun v ->
  H2.Client_connection.shutdown conn ; v

let decode_uri ~ctx uri =
  let ( >>= ) = Result.bind in
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
    ( if String.equal proto "http:"
      then Ok ("http", Mimic.add http_scheme "http" ctx)
      else if String.equal proto "https:"
      then Ok ("https", Mimic.add http_scheme "https" ctx)
      else Error (`Msg "Couldn't decode user and password") ) >>= fun (scheme, ctx) ->
    let decode_user_pass up = match String.split_on_char ':' up with
      | [ user; pass; ] -> Ok (user, pass)
      | _ -> Error (`Msg "Couldn't decode user and password") in
    ( match String.split_on_char '@' user_pass_host_port with
    | [ host_port ] -> Ok (None, host_port)
    | [ user_pass; host_port ] ->
      decode_user_pass user_pass >>= fun up ->
      Ok (Some up, host_port)
    | _ -> Error (`Msg "Couldn't decode URI") ) >>= fun (user_pass, host_port) ->
    ( match String.split_on_char ':' host_port with
    | [] -> Error (`Msg "Empty host & port")
    | [ hostname ] -> Ok (hostname, Mimic.add http_hostname hostname ctx)
    | hd :: tl ->
      let port, hostname = match List.rev (hd :: tl) with
        | hd :: tl -> hd, String.concat ":" (List.rev tl)
        | _ -> assert false in
      ( try Ok (hostname, Mimic.add http_hostname hostname (Mimic.add http_port (int_of_string port) ctx))
        with Failure _ -> Error (`Msg "Couldn't decode port") ) ) >>= fun (hostname, ctx) ->
    Ok (ctx, scheme, hostname, user_pass, "/" ^ String.concat "/" path)
  | _ -> Error (`Msg "Couldn't decode URI on top")

let ( >>? ) = Lwt_result.bind

let alpn_protocol_of_string = function
  | "http/1.1" -> Some `HTTP_1_1
  | "h2" -> Some `H2
  | _ -> None

let single_request ~ctx ~alpn_protocol ?config cfg ~meth ~headers ?body uri =
  Lwt.return (decode_uri ~ctx uri) >>? fun (ctx, scheme, host, user_pass, path) ->
  let ctx = match Lazy.force cfg with
    | Ok (`Custom cfg) -> Mimic.add tls_config cfg ctx
    | Ok (`Default cfg) ->
      ( match Result.bind (Domain_name.of_string host) Domain_name.host with
      | Ok peer -> Mimic.add tls_config (Tls.Config.peer cfg peer) ctx
      | Error _ -> Mimic.add tls_config cfg ctx )
    | Error _ -> ctx in
  Mimic.resolve ctx >>? fun flow ->
  match Option.bind (alpn_protocol flow) alpn_protocol_of_string, config with
  | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
    single_http_1_1_request ~config flow user_pass host meth path headers body
  | (Some `HTTP_1_1 | None), None ->
    single_http_1_1_request flow user_pass host meth path headers body
  | (Some `H2 | None), Some (`H2 config) ->
    single_h2_request ~config ~scheme flow user_pass host meth path headers body
  | Some `H2, None ->
    single_h2_request ~scheme flow user_pass host meth path headers body
  | Some `H2, (Some (`HTTP_1_1 _)) ->
    single_h2_request ~scheme flow user_pass host meth path headers body
  | Some `HTTP_1_1, Some (`H2 _) ->
    single_http_1_1_request flow user_pass host meth path headers body

let tls_config ?tls_config ?config authenticator =
  lazy ( match tls_config with
  | Some cfg -> Ok (`Custom cfg)
  | None ->
    let alpn_protocols = match config with
      | None -> [ "h2"; "http/1.1" ]
      | Some (`H2 _) -> [ "h2" ]
      | Some (`HTTP_1_1 _) -> [ "http/1.1" ] in
    Result.map (fun authenticator -> `Default (Tls.Config.client ~alpn_protocols ~authenticator ())) authenticator )

let resolve_location ~uri ~location =
  match String.split_on_char '/' location with
  | "http:" :: "" :: _ -> Ok location
  | "https:" :: "" :: _ -> Ok location
  | "" :: "" :: _ ->
    let schema = String.sub uri 0 (String.index uri '/') in
    Ok (schema ^ location)
  | "" :: _ ->
    (match String.split_on_char '/' uri with
     | schema :: "" :: user_pass_host_port :: _ ->
       Ok (String.concat "/" [schema ; "" ; user_pass_host_port ^ location])
     | _ -> Error (`Msg ("expected an absolute uri, got: " ^ uri)))
  | _ -> Error (`Msg ("unknown location (relative path): " ^ location))

let one_request
  ?config
  ?tls_config:cfg
  ~ctx
  ~alpn_protocol
  ~authenticator
  ?(meth= `GET)
  ?(headers= [])
  ?body
  ?(max_redirect= 5)
  ?(follow_redirect= true) uri =
  let tls_config = tls_config ?tls_config:cfg ?config authenticator in
  if not follow_redirect
  then single_request ~ctx ~alpn_protocol ?config tls_config ~meth ~headers ?body uri
  else
    let rec follow_redirect count uri =
      if count = 0 then Lwt.return_error (`Msg "Redirect limit exceeded")
      else
        single_request ~ctx ~alpn_protocol ?config tls_config ~meth ~headers ?body uri
        >>? fun (resp, body) ->
        if Status.is_redirection resp.status then
          ( match Headers.get resp.headers "location" with
          | Some location ->
            Lwt.return (resolve_location ~uri ~location) >>? fun uri ->
            follow_redirect (pred count) uri
          | None ->
            Lwt.return_ok (resp, body) )
        else
          Lwt.return_ok (resp, body) in
    follow_redirect max_redirect uri
