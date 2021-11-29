(* (c) Hannes Menhert *)

let ( <.> ) f g x = f (g x)

type configuration = {
  email : Emile.mailbox option;
  certificate_seed : string option;
  certificate_key_type : X509.Key_type.t;
  certificate_key_bits : int option;
  hostname : [ `host ] Domain_name.t;
  account_seed : string option;
  account_key_type : X509.Key_type.t;
  account_key_bits : int option;
}

let scheme = Mimic.make ~name:"paf-le-scheme"

let port = Mimic.make ~name:"paf-le-port"

let domain_name = Mimic.make ~name:"paf-le-domain-name"

let ipaddr = Mimic.make ~name:"paf-le-ipaddr"

let sleep = Mimic.make ~name:"paf-le-sleep"

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

let with_uri uri ctx =
  let scheme_v =
    match Uri.scheme uri with
    | Some "http" -> Some `HTTP
    | Some "https" -> Some `HTTPS
    | _ -> None in
  let port_v =
    match (Uri.port uri, scheme_v) with
    | Some port, _ -> Some port
    | None, Some `HTTP -> Some 80
    | None, Some `HTTPS -> Some 443
    | _ -> None in
  let domain_name_v, ipaddr_v =
    match Uri.host uri with
    | Some v -> (
        match
          ( Result.bind (Domain_name.of_string v) Domain_name.host,
            Ipaddr.of_string v )
        with
        | _, Ok v -> (None, Some v)
        | Ok v, _ -> (Some v, None)
        | _ -> (None, None))
    | _ -> (None, None) in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add scheme v ctx) scheme_v in
  let ctx = Option.fold ~none:ctx ~some:(fun v -> Mimic.add port v ctx) port_v in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add ipaddr v ctx) ipaddr_v in
  let ctx =
    Option.fold ~none:ctx
      ~some:(fun v -> Mimic.add domain_name v ctx)
      domain_name_v in
  ctx

let with_host headers uri =
  let hostname = Uri.host_with_default ~default:"localhost" uri in
  let hostname =
    match Uri.port uri with
    | Some port -> Fmt.str "%s:%d" hostname port
    | None -> hostname in
  Httpaf.Headers.add_unless_exists headers "host" hostname

let with_transfer_encoding ~chunked (meth : [ `GET | `HEAD | `POST ]) body
    headers =
  match (meth, chunked, body, Httpaf.Headers.get headers "content-length") with
  | `GET, _, _, _ -> headers
  | _, (None | Some false), _, Some _ -> headers
  | _, Some true, _, (Some _ | None) | _, None, `Stream _, None ->
      (* XXX(dinosaure): I'm not sure that the [Some _] was right. *)
      Httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked"
  | _, (None | Some false), `Empty, None ->
      Httpaf.Headers.add_unless_exists headers "content-length" "0"
  | _, (None | Some false), `String str, None ->
      Httpaf.Headers.add_unless_exists headers "content-length"
        (string_of_int (String.length str))
  | _, (None | Some false), `Strings sstr, None ->
      let len = List.fold_right (( + ) <.> String.length) sstr 0 in
      Httpaf.Headers.add_unless_exists headers "content-length"
        (string_of_int len)
  | _, Some false, `Stream _, None ->
      invalid_arg "Impossible to transfer a stream with a content-length value"

module HTTP : Letsencrypt__HTTP_client.S with type ctx = Mimic.ctx (* FIXME *) =
struct
  type ctx = Mimic.ctx

  module Headers = struct
    include Httpaf.Headers

    let init_with field value = of_list [ (field, value) ]

    let get_location hdrs = Option.map Uri.of_string (get hdrs "location")
  end

  module Body = struct
    type t =
      [ `Stream of string Lwt_stream.t
      | `Empty
      | `String of string
      | `Strings of string list ]

    let of_string str = `String str

    let to_string = function
      | `Stream t ->
          let open Lwt.Infix in
          Lwt_stream.to_list t >|= String.concat ""
      | `String str -> Lwt.return str
      | `Empty -> Lwt.return ""
      | `Strings sstr -> Lwt.return (String.concat "" sstr)
  end

  module Response = struct
    include Httpaf.Response

    let status resp = Httpaf.Status.to_code resp.Httpaf.Response.status

    let headers resp = resp.Httpaf.Response.headers
  end

  let error_handler mvar err = Lwt.async @@ fun () -> Lwt_mvar.put mvar err

  let response_handler mvar pusher resp body =
    let on_eof () = pusher None in
    let rec on_read buf ~off ~len =
      let str = Bigstringaf.substring buf ~off ~len in
      pusher (Some str) ;
      Httpaf.Body.Reader.schedule_read ~on_eof ~on_read body in
    Httpaf.Body.Reader.schedule_read ~on_eof ~on_read body ;
    Lwt.async @@ fun () -> Lwt_mvar.put mvar resp

  let rec unroll body stream =
    let open Lwt.Infix in
    Lwt_stream.get stream >>= function
    | Some str ->
        Httpaf.Body.Writer.write_string body str ;
        unroll body stream
    | None ->
        Httpaf.Body.Writer.close body ;
        Lwt.return_unit

  let transmit cohttp_body httpaf_body =
    match cohttp_body with
    | `Empty -> Httpaf.Body.Writer.close httpaf_body
    | `String str ->
        Httpaf.Body.Writer.write_string httpaf_body str ;
        Httpaf.Body.Writer.close httpaf_body
    | `Strings sstr ->
        List.iter (Httpaf.Body.Writer.write_string httpaf_body) sstr ;
        Httpaf.Body.Writer.close httpaf_body
    | `Stream stream -> Lwt.async @@ fun () -> unroll httpaf_body stream

  exception Invalid_response_body_length of Httpaf.Response.t

  exception Malformed_response of string

  let call ?(ctx = Mimic.empty) ?(headers = Httpaf.Headers.empty)
      ?(body = `Empty) ?chunked (meth : [ `GET | `HEAD | `POST ]) uri =
    let ctx = with_uri uri ctx in
    let sleep =
      match Mimic.get sleep ctx with
      | Some sleep -> sleep
      | None -> fun _ -> Lwt.return_unit
      (* TODO *) in
    let headers = with_host headers uri in
    let headers = with_transfer_encoding ~chunked meth body headers in
    let req =
      Httpaf.Request.create ~headers
        (meth :> Httpaf.Method.t)
        (Uri.path_and_query uri) in
    let stream, pusher = Lwt_stream.create () in
    let mvar_res = Lwt_mvar.create_empty () in
    let mvar_err = Lwt_mvar.create_empty () in
    let open Lwt.Infix in
    Mimic.resolve ctx >>= function
    | Error (#Mimic.error as err) ->
        Lwt.fail (Failure (Fmt.str "%a" Mimic.pp_error err))
    | Ok flow -> (
        let error_handler = error_handler mvar_err in
        let response_handler = response_handler mvar_res pusher in
        let conn = Httpaf.Client_connection.create ?config:None in
        let httpaf_body =
          Httpaf.Client_connection.request conn ~error_handler ~response_handler
            req in
        Lwt.async (fun () ->
            Paf.run ~sleep (module Httpaf_Client_connection) conn flow) ;
        transmit body httpaf_body ;
        Lwt.pick
          [
            (Lwt_mvar.take mvar_res >|= fun res -> `Response res);
            (Lwt_mvar.take mvar_err >|= fun err -> `Error err);
          ]
        >>= function
        | `Error (`Exn exn) -> Mimic.close flow >>= fun () -> Lwt.fail exn
        | `Error (`Invalid_response_body_length resp) ->
            Mimic.close flow >>= fun () ->
            Lwt.fail (Invalid_response_body_length resp)
        | `Error (`Malformed_response err) ->
            Mimic.close flow >>= fun () -> Lwt.fail (Malformed_response err)
        | `Response resp -> Lwt.return (resp, `Stream stream))

  open Lwt.Infix

  let head ?ctx ?headers uri = call ?ctx ?headers `HEAD uri >|= fst

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `POST uri
end

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  type nonrec configuration = configuration = {
    email : Emile.mailbox option;
    certificate_seed : string option;
    certificate_key_type : X509.Key_type.t;
    certificate_key_bits : int option;
    hostname : [ `host ] Domain_name.t;
    account_seed : string option;
    account_key_type : X509.Key_type.t;
    account_key_bits : int option;
  }

  module Acme = Letsencrypt.Client.Make (HTTP)

  module Log = (val let src = Logs.Src.create "letsencrypt" in
                    Logs.src_log src : Logs.LOG)

  let gen_key ?seed ?bits key_type =
    let seed = Option.map Cstruct.of_string seed in
    X509.Private_key.generate ?seed ?bits key_type

  let csr key host =
    let host = Domain_name.to_string host in
    let cn =
      X509.
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN host)) ]
    in
    X509.Signing_request.create cn key

  let prefix = (".well-known", "acme-challenge")

  let tokens = Hashtbl.create 1

  let solver _host ~prefix:_ ~token ~content =
    Hashtbl.replace tokens token content ;
    Lwt.return (Ok ())

  let request_handler (ipaddr, port) reqd =
    Log.debug (fun m ->
        m "Let's encrypt request handler for %a:%d" Ipaddr.pp ipaddr port) ;
    let req = Httpaf.Reqd.request reqd in
    match String.split_on_char '/' req.Httpaf.Request.target with
    | [ p1; p2; token ]
      when String.equal p1 (fst prefix) && String.equal p2 (snd prefix) -> (
        match Hashtbl.find_opt tokens token with
        | Some data ->
            let headers =
              Httpaf.Headers.of_list
                [
                  ("content-type", "application/octet-stream");
                  ("content-length", string_of_int (String.length data));
                ] in
            let resp = Httpaf.Response.create ~headers `OK in
            Httpaf.Reqd.respond_with_string reqd resp data
        | None ->
            let headers = Httpaf.Headers.of_list [ ("connection", "close") ] in
            let resp = Httpaf.Response.create ~headers `Not_found in
            Httpaf.Reqd.respond_with_string reqd resp "")
    | _ ->
        let headers = Httpaf.Headers.of_list [ ("connection", "close") ] in
        let resp = Httpaf.Response.create ~headers `Not_found in
        Httpaf.Reqd.respond_with_string reqd resp ""

  let provision_certificate ?(production = false) cfg ctx =
    let ( >>? ) = Lwt_result.bind in
    let endpoint =
      if production
      then Letsencrypt.letsencrypt_production_url
      else Letsencrypt.letsencrypt_staging_url in
    let priv =
      gen_key ?seed:cfg.certificate_seed ?bits:cfg.certificate_key_bits
        cfg.certificate_key_type in
    match csr priv cfg.hostname with
    | Error _ as err -> Lwt.return err
    | Ok csr ->
        let account_key =
          gen_key ?seed:cfg.account_seed ?bits:cfg.account_key_bits
            cfg.account_key_type in
        Acme.initialise ~ctx ~endpoint
          ?email:(Option.map Emile.to_string cfg.email)
          account_key
        >>? fun le ->
        let sleep sec = Time.sleep_ns (Duration.of_sec sec) in
        let solver = Letsencrypt.Client.http_solver solver in
        Acme.sign_certificate ~ctx solver le sleep csr >>? fun certs ->
        Lwt.return_ok (`Single (certs, priv))

  open Lwt.Infix

  module TCP = struct
    include Stack.TCP

    type endpoint = Stack.TCP.t * Ipaddr.t * int

    let pp_write_error ppf = function
      | `Error err -> pp_error ppf err
      | `Write_error err -> pp_write_error ppf err
      | `Closed -> pp_write_error ppf `Closed

    let write stack cs =
      write stack cs >|= Result.map_error (fun err -> `Write_error err)

    let writev stack css =
      writev stack css >|= Result.map_error (fun err -> `Write_error err)

    type nonrec write_error =
      [ `Error of error | `Write_error of write_error | `Closed ]

    let connect (stack, ipaddr, port) =
      create_connection stack (ipaddr, port) >>= function
      | Ok flow -> Lwt.return_ok flow
      | Error err -> Lwt.return_error (`Error err)
  end

  module TLS = struct
    include Tls_mirage.Make (Stack.TCP)

    type endpoint =
      Stack.TCP.t
      * Tls.Config.client
      * [ `host ] Domain_name.t option
      * Ipaddr.t
      * int

    let connect (stack, cfg, host, ipaddr, port) =
      Stack.TCP.create_connection stack (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow -> client_of_flow ?host cfg flow
  end

  let ctx ~gethostbyname ~authenticator dns stackv4v6 =
    let tcp_edn, _tcp_protocol =
      Mimic.register ~name:"letsencrypt-tcp" (module TCP) in
    let tls_edn, _tls_protocol =
      Mimic.register ~name:"letsencrypt-tls" (module TLS) in

    let k0 scheme stack ipaddr port =
      match scheme with
      | `HTTP -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none in
    let k1 scheme stack tls domain_name ipaddr port =
      match scheme with
      | `HTTPS -> Lwt.return_some (stack, tls, domain_name, ipaddr, port)
      | _ -> Lwt.return_none in
    let k2 domain_name =
      gethostbyname dns domain_name >>= function
      | Ok v -> Lwt.return_some v
      | Error _ -> Lwt.return_none in
    let open Mimic in
    let stack = Mimic.make ~name:"letsencrypt-stack" in
    let tls = Mimic.make ~name:"letsencrypt-tls" in

    Mimic.empty
    |> Mimic.add sleep Time.sleep_ns
    |> Mimic.add stack (Stack.tcp stackv4v6)
    |> Mimic.fold tcp_edn
         Fun.[ req scheme; req stack; req ipaddr; dft port 80 ]
         ~k:k0
    |> Mimic.fold tls_edn
         Fun.
           [
             req scheme;
             req stack;
             dft tls (Tls.Config.client ~authenticator ());
             opt domain_name;
             req ipaddr;
             dft port 443;
           ]
         ~k:k1
    |> Mimic.fold ipaddr Fun.[ req domain_name ] ~k:k2

  let with_uri = with_uri
end
