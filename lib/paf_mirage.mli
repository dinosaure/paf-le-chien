module type S = sig
  type stack

  module TCP : Mirage_flow.S

  module TLS : module type of Tls_mirage.Make (TCP)

  type t

  type dst = Ipaddr.t * int

  val init : port:int -> stack -> t Lwt.t

  val accept : t -> (TCP.flow, [> `Closed ]) result Lwt.t

  val close : t -> unit Lwt.t

  val http_service :
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val https_service :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]

  val tcp_protocol : (stack * Ipaddr.t * int, TCP.flow) Mimic.protocol

  val tcp_edn : (stack * Ipaddr.t * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option
    * Tls.Config.client
    * stack
    * Ipaddr.t
    * int)
    Mimic.value

  val tls_protocol :
    ( [ `host ] Domain_name.t option * Tls.Config.client * stack * Ipaddr.t * int,
      TLS.flow )
    Mimic.protocol

  val run :
    ctx:Mimic.ctx ->
    error_handler:(dst option -> Alpn.client_error -> unit) ->
    response_handler:(dst option -> [ `read ] Alpn.resp_handler -> unit) ->
    [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
    ([ `write ] Alpn.body, [> Mimic.error ]) result Lwt.t
end

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) :
  S with type stack = Stack.t and type TCP.flow = Stack.TCP.flow
