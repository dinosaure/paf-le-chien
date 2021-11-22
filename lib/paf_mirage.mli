module type S = sig
  type stack
  (** The type of the TCP/IP stack. *)

  module TCP : sig
    include Mirage_flow.S

    val dst : flow -> Ipaddr.t * int
  end

  module TLS : module type of Tls_mirage.Make (TCP)

  type t
  (** The type of the {i socket} bound on a specific port (via {!init}). *)

  type dst = Ipaddr.t * int

  val init : port:int -> stack -> t Lwt.t
  (** [init ~port stack] bounds the given [stack] to a specific port and return
      the main socket {!t}. *)

  val accept : t -> (TCP.flow, [> `Closed ]) result Lwt.t
  (** [accept t] waits an incoming connection and return a {i socket} connected
      to a peer. *)

  val close : t -> unit Lwt.t
  (** [close t] closes the main {e socket}. *)

  val http_service :
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service
  (** [http_service ~error_handler request_handler] makes an HTTP/AF service
      where any HTTP/1.1 requests are handled by [request_handler]. The returned
      service is not yet launched (see {!serve}). *)

  val https_service :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service
  (** [https_service ~tls ~error_handler request_handler] makes an HTTP/AF
      service over TLS (from the given TLS configuration). Then, HTTP/1.1
      requests are handled by [request_handler]. The returned service is not yet
      launched (see {!serve}). *)

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]
  (** [serve ?stop service] returns an initialized promise of the given service
      [service]. [stop] can be used to stop the service. *)

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
    response_handler:(dst option -> Alpn.response -> Alpn.body -> unit) ->
    [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
    (Alpn.body, [> Mimic.error ]) result Lwt.t
end

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) :
  S with type stack = Stack.TCP.t and type TCP.flow = Stack.TCP.flow
