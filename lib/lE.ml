(* (c) Hannes Menhert *)

type configuration = {
  email : Emile.mailbox option;
  seed : string option;
  certificate_seed : string option;
  hostname : [ `host ] Domain_name.t;
}

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  type nonrec configuration = configuration = {
    email : Emile.mailbox option;
    seed : string option;
    certificate_seed : string option;
    hostname : [ `host ] Domain_name.t;
  }

  module Acme = Letsencrypt.Client.Make (Paf_cohttp)

  module Log = (val let src = Logs.Src.create "letsencrypt" in
                    Logs.src_log src : Logs.LOG)

  let gen_rsa ?seed () =
    let g =
      match seed with
      | None -> None
      | Some seed ->
          let seed = Cstruct.of_string seed in
          Some Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ?g ~bits:4096 ()

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
    match
      Astring.String.cuts ~sep:"/" ~empty:false req.Httpaf.Request.target
    with
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
    let priv = `RSA (gen_rsa ?seed:cfg.seed ()) in
    match csr priv cfg.hostname with
    | Error _ as err -> Lwt.return err
    | Ok csr ->
        Acme.initialise ~ctx ~endpoint
          ?email:(Option.map Emile.to_string cfg.email)
          (gen_rsa ?seed:cfg.seed ())
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
      write stack cs >|= Rresult.R.reword_error (fun err -> `Write_error err)

    let writev stack css =
      writev stack css >|= Rresult.R.reword_error (fun err -> `Write_error err)

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

    let connect (stack, cfg, domain_name, ipaddr, port) =
      let host = Option.map Domain_name.to_string domain_name in
      Stack.TCP.create_connection stack (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow -> client_of_flow ?host cfg flow
  end

  include Paf_cohttp

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
end
