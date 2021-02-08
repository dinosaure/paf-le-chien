open Lwt.Infix

let ( <.> ) f g = fun x -> f (g x)

let app =
  let open Rock in
  let handler _req =
    let resp = Response.make ~status:`OK ~body:(Body.of_string "Hello Rock!") () in
    Lwt.return resp in
  App.create ~handler ()

module Make
  (Console : Mirage_console.S)
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (StackV4 : Mirage_stack.V4)
  (Stack : Mirage_stack.V4V6) = struct
  module Resolver = Dns_client_mirage.Make(Random)(Time)(Mclock)(StackV4)
  module Paf = Paf.Make(Time)(Stack)
  module Letsencrypt = LE.Make(Time)(Paf)
  module Nss = Ca_certs_nss.Make(Pclock)

  let root =
    let authenticator = Rresult.R.failwith_error_msg (Nss.authenticator ()) in
    Tls.Config.client ~authenticator ()

  let tcp_connect scheme stack ipaddr port = match scheme with
    | `HTTP -> Lwt.return_some (stack, ipaddr, port)
    | _ -> Lwt.return_none

  let tls_connect scheme domain_name cfg stack ipaddr port = match scheme with
    | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port)
    | _ -> Lwt.return_none

  let dns_resolver dns domain_name =
    Resolver.gethostbyname dns domain_name >>= function
    | Ok ipv4 -> Lwt.return_some (Ipaddr.V4 ipv4)
    | _ -> Lwt.return_none

  let error_handler _ ?request:_ _ _ = ()

  let get_certificate ?(production= false) cfg stackv4 stackv4v6 =
    let dns   = Mimic.make ~name:"dns" in
    let stack = Mimic.make ~name:"stack" in
    let tls   = Mimic.make ~name:"tls" in

    let ctx =
      let open Letsencrypt in
      Mimic.empty
      |> Mimic.(fold Paf.tcp_edn Fun.[ req scheme; req stack; req ipaddr; dft port 80; ] ~k:tcp_connect)
      |> Mimic.(fold Paf.tls_edn Fun.[ req scheme; opt domain_name; dft tls root; req stack; req ipaddr; dft port 443; ]
                ~k:tls_connect)
      |> Mimic.(fold ipaddr Fun.[ req dns; req domain_name; ] ~k:dns_resolver)
      |> Mimic.add dns (Resolver.create stackv4)
      |> Mimic.add stack stackv4v6 in
    Paf.init ~port:80 stackv4v6 >>= fun service ->
    Lwt_switch.with_switch @@ fun stop ->
    let `Initialized th = Paf.http ~stop ~request_handler:Letsencrypt.request_handler ~error_handler service in
    let fiber =
      Letsencrypt.provision_certificate ~production cfg ctx >>= fun res ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return res in
    Lwt.both th fiber >>= function
    | (_, Ok tls) -> Lwt.return tls
    | (_, Error (`Msg err)) -> failwith err

  type kind =
    | HTTP of int
    | HTTPS of (int * Letsencrypt.configuration)

  let rock stackv4 stackv4v6 v ~request_handler ~error_handler = match v with
    | HTTP port ->
      Paf.init ~port stackv4v6 >>= fun service ->
      let `Initialized th = Paf.http
        ~error_handler:(fun _ -> error_handler)
        ~request_handler:(fun _ -> request_handler)
        service in th
    | HTTPS (port, cfg) ->
      get_certificate ~production:(Key_gen.production ()) cfg stackv4 stackv4v6 >>= fun certificates ->
      let tls = Tls.Config.server ~certificates () in
      Paf.init ~port stackv4v6 >>= fun service ->
      let `Initialized th = Paf.https ~tls
        ~error_handler:(fun _ -> error_handler)
        ~request_handler:(fun _ -> request_handler)
        service in th

  let host v = let open Rresult in
    Domain_name.of_string v >>= Domain_name.host

  let cfg ?port ?email ?hostname ?seed ?certificate_seed = function
    | false -> HTTP (Option.value ~default:80 port)
    | true -> match hostname with
      | Some hostname ->
        HTTPS (Option.value ~default:443 port, { Letsencrypt.hostname; email; seed; certificate_seed; })
      | None -> failwith "Missing hostname"

  let start console _random _time _mclock _pclock stackv4 stackv4v6 =
    let email = Option.bind (Key_gen.email ()) (Rresult.R.to_option <.> Emile.of_string) in
    let hostname = Option.bind (Key_gen.hostname ()) (Rresult.R.(to_option <.> host)) in
    let cfg = cfg ?port:(Key_gen.port ())
                  ?email
                  ?hostname
                  ?seed:(Key_gen.account_seed ())
                  ?certificate_seed:(Key_gen.cert_seed ())
              (Key_gen.https ()) in
    Rock.Server_connection.run (rock stackv4 stackv4v6 cfg) app
end
