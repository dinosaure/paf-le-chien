[@@@warning "-45"]

module type DNS = sig
  type t

  module Transport : Dns_client.S
    with type io_addr = Ipaddr.t * int
     and type +'a io = 'a Lwt.t

  val nameserver : t -> Transport.ns_addr
  val getaddrinfo : t -> ?nameserver:Transport.ns_addr -> 'response Dns.Rr_map.key -> 'a Domain_name.t ->
    ('response, [> `Msg of string ]) result Lwt.t
  val gethostbyname : t -> ?nameserver:Transport.ns_addr -> [ `host ] Domain_name.t ->
    (Ipaddr.V4.t, [> `Msg of string ]) result Lwt.t
  val gethostbyname6 : t -> ?nameserver:Transport.ns_addr -> [ `host ] Domain_name.t ->
    (Ipaddr.V6.t, [> `Msg of string ]) result Lwt.t
  val get_resource_record : t -> ?nameserver:Transport.ns_addr ->
    'response Dns.Rr_map.key -> 'a Domain_name.t ->
    ('response,
     [> `Msg of string
     | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
     | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]) result Lwt.t
end

open Lwt.Infix

module Make
  (Console : Mirage_console.S)
  (Time : Mirage_time.S)
  (Pclock : Mirage_clock.PCLOCK)
  (_ : Mirage_stack.V4V6)
  (Dns : DNS) (* XXX(dinosaure): ask @hannesm to provide a signature. *)
  (Paf : Paf_mirage.S) = struct
  module Client = Paf_cohttp
  module Nss = Ca_certs_nss.Make(Pclock)

  let authenticator = Result.get_ok (Nss.authenticator ())
  let default_tls_cfg = Tls.Config.client ~authenticator ()

  let stack = Mimic.make ~name:"stack"
  let tls = Mimic.make ~name:"tls"

  let with_stack v ctx = Mimic.add stack v ctx

  let with_tcp ctx =
    let k scheme stack ipaddr port = match scheme with
      | `HTTP -> Lwt.return_some (stack, ipaddr, port) | _ -> Lwt.return_none in
    Mimic.(fold Paf.tcp_edn Fun.[ req Client.scheme
                                ; req stack
                                ; req Client.ipaddr
                                ; dft Client.port 80 ] ~k ctx)

  let with_tls ctx =
    let k scheme domain_name cfg stack ipaddr port = match scheme with
      | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port) | _ -> Lwt.return_none in
    Mimic.(fold Paf.tls_edn Fun.[ req Client.scheme
                                ; opt Client.domain_name
                                ; dft tls default_tls_cfg
                                ; req stack
                                ; req Client.ipaddr
                                ; dft Client.port 443 ] ~k ctx)

  let dns = Mimic.make ~name:"dns"

  let with_dns v ctx = Mimic.add dns v ctx
  let with_sleep ctx = Mimic.add Paf_cohttp.sleep Time.sleep_ns ctx

  let with_resolv ctx =
    let k dns domain_name =
      Dns.gethostbyname dns domain_name >>= function
      | Ok ipv4 -> Lwt.return_some (Ipaddr.V4 ipv4)
      | _ -> Lwt.return_none in
    Mimic.(fold Client.ipaddr Fun.[ req dns; req Client.domain_name ] ~k ctx)

  let log console fmt = Fmt.kstr (Console.log console) fmt

  let start console _time _pclock stack dns _paf_cohttp =
    let uri = Uri.of_string (Key_gen.uri ()) in
    let ctx =
      Mimic.empty
      |> with_sleep
      |> with_tcp         (* stack -> ipaddr -> port => (stack * ipaddr * port) *)
      |> with_tls         (* domain_name -> tls -> stack -> ipaddr -> port => (domain_name * tls * stack * ipaddr * port) *)
      |> with_resolv      (* domain_name => ipaddr *)
      |> with_stack stack (* stack *)
      |> with_dns dns     (* dns *) in
    Client.get ~ctx uri >>= fun (_resp, body) ->
    Cohttp_lwt.Body.to_string body >>= fun str ->
    log console "%S\n%!" str
end
