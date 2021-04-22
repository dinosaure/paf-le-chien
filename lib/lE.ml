(* (c) Hannes Menhert *)

module Make (Time : Mirage_time.S) = struct
  module Acme = Letsencrypt.Client.Make (Paf_cohttp)

  module Log = (val let src = Logs.Src.create "letsencrypt" in
                    Logs.src_log src : Logs.LOG)

  type configuration = {
    email : Emile.mailbox option;
    seed : string option;
    certificate_seed : string option;
    hostname : [ `host ] Domain_name.t;
  }

  let gen_rsa ?seed () =
    let g =
      match seed with
      | None -> None
      | Some seed ->
          let seed = Cstruct.of_string seed in
          Some Mirage_crypto_rng.(create ~seed (module Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ?g ~bits:4096 ()

  let csr hostname key =
    let hostname = Domain_name.to_string hostname in
    let cn =
      X509.
        [
          Distinguished_name.(
            Relative_distinguished_name.singleton (CN hostname));
        ] in
    X509.Signing_request.create cn (`RSA key)

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
        match Hashtbl.find tokens token with
        | data ->
            let headers =
              Httpaf.Headers.of_list
                [
                  ("content-type", "application/octet-stream");
                  ("content-length", string_of_int (String.length data));
                ] in
            let resp = Httpaf.Response.create ~headers `OK in
            Httpaf.Reqd.respond_with_string reqd resp data
        | exception Not_found ->
            let headers = Httpaf.Headers.of_list [ ("connection", "close") ] in
            let resp = Httpaf.Response.create ~headers `Not_found in
            Httpaf.Reqd.respond_with_string reqd resp "")
    | _ ->
        let headers = Httpaf.Headers.of_list [ ("connection", "close") ] in
        let resp = Httpaf.Response.create ~headers `Not_found in
        Httpaf.Reqd.respond_with_string reqd resp ""

  let provision_certificate ?(production = false) cfg ctx =
    let open Lwt_result.Infix in
    let endpoint =
      if production
      then Letsencrypt.letsencrypt_production_url
      else Letsencrypt.letsencrypt_staging_url in
    Acme.initialise ~ctx ~endpoint
      ?email:(Option.map Emile.to_string cfg.email)
      (gen_rsa ?seed:cfg.seed ())
    >>= fun le ->
    let sleep sec = Time.sleep_ns (Duration.of_sec sec) in
    let priv = gen_rsa ?seed:cfg.certificate_seed () in
    Lwt.return (csr cfg.hostname priv) >>= fun csr ->
    let solver = Letsencrypt.Client.http_solver solver in
    Acme.sign_certificate ~ctx solver le sleep csr >|= fun certs ->
    `Single (certs, `RSA priv)

  include Paf_cohttp
end
