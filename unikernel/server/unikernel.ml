open Lwt.Infix

let ( <.> ) f g = fun x -> f (g x)
let ( >>? ) = Lwt_result.bind

let app =
  let open Rock in
  let handler _req =
    let resp = Response.make ~status:`OK ~body:(Body.of_string "Hello Rock!") () in
    Lwt.return resp in
  App.create ~handler ()

module Make
  (_ : Mirage_console.S)
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Paf : Paf_mirage.S with type stack = Stack.TCP.t
                       and type ipaddr = Ipaddr.t) = struct
  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  module NSS = Ca_certs_nss.Make(Pclock)
  module Letsencrypt = LE.Make(Time)(Stack)

  let authenticator = Result.get_ok (NSS.authenticator ())

  let error_handler _ ?request:_ _ _ = ()

  let get_certificate ?(production= false) cfg stackv4v6 =
    Paf.init ~port:80 (Stack.tcp stackv4v6) >>= fun t ->
    let service =
      Paf.http_service ~error_handler
        (fun _flow -> Letsencrypt.request_handler)
    in
    Lwt_switch.with_switch @@ fun stop ->
    let `Initialized th = Paf.serve ~stop service t in
    let gethostbyname dns domain_name =
      DNS.gethostbyname dns domain_name >>? fun ipv4 ->
      Lwt.return_ok (Ipaddr.V4 ipv4) in
    let ctx = Letsencrypt.ctx
      ~gethostbyname
      ~authenticator
      (DNS.create stackv4v6) stackv4v6 in
    let fiber =
      Letsencrypt.provision_certificate ~production cfg ctx >>= fun certificates ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return certificates in
    Lwt.both th fiber >>= function
    | (_, Ok certificates) -> Lwt.return certificates
    | (_, Error (`Msg err)) -> failwith err

  type kind =
    | HTTP
    | HTTPS of Letsencrypt.configuration

  let rock stackv4v6 v t ~request_handler ~error_handler = match v with
    | HTTP ->
      let service = Paf.http_service ~error_handler:(fun _ -> error_handler)
        (fun _flow _dst -> request_handler) in
      let `Initialized th = Paf.serve service t in th
    | HTTPS cfg ->
      get_certificate ~production:(Key_gen.production ()) cfg stackv4v6 >>= fun certificates ->
      let tls = Tls.Config.server ~certificates () in
      let service = Paf.https_service ~tls ~error_handler:(fun _ -> error_handler)
        (fun _flow _dst -> request_handler) in
      let `Initialized th = Paf.serve service t in th

  let host v =
    Result.bind (Domain_name.of_string v) Domain_name.host

  let cfg ?email ?hostname ?seed ?certificate_seed = function
    | false -> HTTP
    | true -> match hostname with
      | Some hostname ->
        HTTPS { LE.hostname
              ; email
              ; account_seed= seed
              ; account_key_type= `ED25519
              ; account_key_bits= None
              ; certificate_seed
              ; certificate_key_type= `ED25519
              ; certificate_key_bits= None }
      | None -> failwith "Missing hostname"

  let start _console _random _time _mclock _pclock stackv4v6 service =
    let email = Option.bind (Key_gen.email ()) (Result.to_option <.> Emile.of_string) in
    let hostname = Option.bind (Key_gen.hostname ()) (Result.to_option <.> host) in
    let cfg = cfg ?email
                  ?hostname
                  ?seed:(Key_gen.account_seed ())
                  ?certificate_seed:(Key_gen.cert_seed ())
              (Key_gen.https ()) in
    Rock.Server_connection.run (rock stackv4v6 cfg service) app
end
