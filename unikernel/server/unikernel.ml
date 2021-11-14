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
  (Stack : Mirage_stack.V4V6) = struct
  module DNS = Dns_client_mirage.Make(Random)(Time)(Mclock)(Stack)
  module Paf = Paf_mirage.Make(Time)(Stack)
  module Nss = Ca_certs_nss.Make(Pclock)
  module Letsencrypt = LE.Make(Time)(Stack)

  let authenticator = Result.get_ok (Nss.authenticator ())

  let error_handler _ ?request:_ _ _ = ()

  let get_certificate ?(production= false) cfg stackv4v6 =
    Paf.init ~port:80 stackv4v6 >>= fun t ->
    let service = Paf.http_service ~error_handler Letsencrypt.request_handler in
    Lwt_switch.with_switch @@ fun stop ->
    let `Initialized th = Paf.serve ~stop service t in
    let ctx = Letsencrypt.ctx
      ~gethostbyname:(fun dns domain_name -> DNS.gethostbyname dns domain_name >>? fun ipv4 -> Lwt.return_ok (Ipaddr.V4 ipv4))
      ~authenticator
      (DNS.create stackv4v6) stackv4v6 in
    let fiber =
      Letsencrypt.provision_certificate ~production cfg ctx >>= fun certificates ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return certificates in
    Lwt.both th fiber >>= function
    | (_, Ok certificates) -> Lwt.return certificates
    | (_, Error (`Msg err)) -> failwith err

  type kind =
    | HTTP of int
    | HTTPS of (int * Letsencrypt.configuration)

  let rock stackv4v6 v ~request_handler ~error_handler = match v with
    | HTTP port ->
      Paf.init ~port stackv4v6 >>= fun t ->
      let service = Paf.http_service ~error_handler:(fun _ -> error_handler)
        (fun _ -> request_handler) in
      let `Initialized th = Paf.serve service t in th
    | HTTPS (port, cfg) ->
      get_certificate ~production:(Key_gen.production ()) cfg stackv4v6 >>= fun certificates ->
      let tls = Tls.Config.server ~certificates () in
      Paf.init ~port stackv4v6 >>= fun t ->
      let service = Paf.https_service ~tls ~error_handler:(fun _ -> error_handler)
        (fun _ -> request_handler) in
      let `Initialized th = Paf.serve service t in th

  let host v =
    Result.bind (Domain_name.of_string v) Domain_name.host

  let cfg ?port ?email ?hostname ?seed ?certificate_seed = function
    | false -> HTTP (Option.value ~default:80 port)
    | true -> match hostname with
      | Some hostname ->
        HTTPS (Option.value ~default:443 port, { Letsencrypt.hostname; email; seed; certificate_seed; })
      | None -> failwith "Missing hostname"

  let start _console _random _time _mclock _pclock stackv4v6 =
    let email = Option.bind (Key_gen.email ()) (Result.to_option <.> Emile.of_string) in
    let hostname = Option.bind (Key_gen.hostname ()) (Result.to_option <.> host) in
    let cfg = cfg ?port:(Key_gen.port ())
                  ?email
                  ?hostname
                  ?seed:(Key_gen.account_seed ())
                  ?certificate_seed:(Key_gen.cert_seed ())
              (Key_gen.https ()) in
    Rock.Server_connection.run (rock stackv4v6 cfg) app
end
