module type S = sig
  type stack
  (** The type of the TCP/IP stack. *)

  type ipaddr
  (** The type of the IP address. *)

  (** {2 Protocols.}

      From the given stack, [Paf_mirage] constructs protocols needed for HTTP:

      - A simple TCP/IP protocol
      - A TCP/IP protocol wrapped into TLS {i via} [ocaml-tls]

      We expose these protocols in the sense of [mimic]. They are registered
      globally with [mimic] and are usable {i via} [mimic] (see
      {!Mimic.resolve}) as long as the given [ctx] contains {!val:tcp_edn}
      and/or {!val:tls_edn}. Such way to instance {i something} which represents
      these protocols and usable as a {!Mirage_flow.S} are useful for the
      client-side, see {!val:run}.

      We expose 2 new functions: [no_close]/[to_close]. In a specific context
      such as the proxy, the handler should notify us to fakely close the
      underlying connection. Indeed, [Paf] will try to close your connection as
      soon as the HTTP transmission is finished. However, in the case of a
      proxy, the connection must remains then. {!val:TCP.no_close} sets the
      [flow] so that the next call to {!val:TCP.close} is ignored.
      {!val:to_close} resets the [flow] to the basic behavior - we will really
      close the given [flow]. *)

  module TCP : sig
    include Mirage_flow.S

    val dst : flow -> ipaddr * int
    val no_close : flow -> unit
    val to_close : flow -> unit
  end

  module TLS : sig
    type error =
      [ `Tls_alert of Tls.Packet.alert_type
      | `Tls_failure of Tls.Engine.failure
      | `Read of TCP.error
      | `Write of TCP.write_error ]

    type write_error = [ `Closed | error ]

    include
      Mirage_flow.S with type error := error and type write_error := write_error

    val no_close : flow -> unit
    val to_close : flow -> unit
    val epoch : flow -> (Tls.Core.epoch_data, unit) result

    val reneg :
      ?authenticator:X509.Authenticator.t ->
      ?acceptable_cas:X509.Distinguished_name.t list ->
      ?cert:Tls.Config.own_cert ->
      ?drop:bool ->
      flow ->
      (unit, [ write_error | `Msg of string ]) result Lwt.t

    val key_update :
      ?request:bool ->
      flow ->
      (unit, [ write_error | `Msg of string ]) result Lwt.t

    val server_of_flow :
      Tls.Config.server -> TCP.flow -> (flow, write_error) result Lwt.t

    val client_of_flow :
      Tls.Config.client ->
      ?host:[ `host ] Domain_name.t ->
      TCP.flow ->
      (flow, write_error) result Lwt.t
  end

  val tcp_protocol : (stack * ipaddr * int, TCP.flow) Mimic.protocol
  val tcp_edn : (stack * ipaddr * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option * Tls.Config.client * stack * ipaddr * int)
    Mimic.value

  val tls_protocol :
    ( [ `host ] Domain_name.t option * Tls.Config.client * stack * ipaddr * int,
      TLS.flow )
    Mimic.protocol

  (** {2 Server implementation.} *)

  type t
  (** The type of the {i socket} bound on a specific port (via {!init}). *)

  type dst = ipaddr * int

  val init : port:int -> stack -> t Lwt.t
  (** [init ~port stack] bounds the given [stack] to a specific port and return
      the main socket {!t}. *)

  val accept : t -> (TCP.flow, [> `Closed ]) result Lwt.t
  (** [accept t] waits an incoming connection and return a {i socket} connected
      to a peer. *)

  val close : t -> unit Lwt.t
  (** [close t] closes the main {e socket}. *)

  (** {3 HTTP/1.1 servers.}

      The user is able to launch a simple HTTP/1.1 server with TLS or not.
      Below, you can see a simple example:

      {[
        let run ~error_handler ~request_handler =
          Paf_mirage.init ~port:8080 stack >>= fun t ->
          Paf_mirage.http_service ~error_handler request_handler
          >>= fun service ->
          let (`Initialized th) = Paf_mirage.serve service t in
          th
      ]} *)

  val http_service :
    ?config:H1.Config.t ->
    error_handler:(dst -> H1.Server_connection.error_handler) ->
    (TCP.flow -> dst -> H1.Server_connection.request_handler) ->
    t Paf.service
  (** [http_service ~error_handler request_handler] makes an HTTP/AF service
      where any HTTP/1.1 requests are handled by [request_handler]. The returned
      service is not yet launched (see {!serve}). *)

  val https_service :
    tls:Tls.Config.server ->
    ?config:H1.Config.t ->
    error_handler:(dst -> H1.Server_connection.error_handler) ->
    (TLS.flow -> dst -> H1.Server_connection.request_handler) ->
    t Paf.service
  (** [https_service ~tls ~error_handler request_handler] makes an HTTP/AF
      service over TLS (from the given TLS configuration). Then, HTTP/1.1
      requests are handled by [request_handler]. The returned service is not yet
      launched (see {!serve}). *)

  (** {3 HTTP/1.1 & H2 over TLS server.}

      It's possible to make am ALPN server. It's an HTTP server which can handle

      - HTTP/1.1 requests
      - and H2 requests

      The choice is made by the ALPN challenge on the TLS layer where the client
      can send which protocol he/she wants to use. Therefore, the server must
      handle these two cases. *)

  val alpn_service :
    tls:Tls.Config.server ->
    ?config:H1.Config.t * H2.Config.t ->
    (TLS.flow, dst) Alpn.server_handler ->
    t Paf.service
  (** [alpn_service ~tls handler] makes an H2/HTTP/AF service over TLS (from the
      given TLS configuration). An HTTP request (version 1.1 or 2) is handled
      then by [handler]. The returned service is not yet launched (see
      {!val:serve} to launch it). *)

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]
  (** [serve ?stop service] returns an initialized promise of the given service
      [service]. [stop] can be used to stop the service. *)
end

module Make (Stack : Tcpip.Tcp.S) :
  S with type stack = Stack.t and type ipaddr = Stack.ipaddr

(** {2 Client implementation.}

    The client implementation of [Paf_mirage] does not strictly need a
    {i functor}. Indeed, the client was made in the sense of [mimic]. The user
    should provide a {!Mimic.ctx} which generate a {!paf_transmission}. By this
    way, the {!run} function is able to introspect the used protocol (regardless
    its implementation) and do the ALPN challenge with the server. *)

type transmission = [ `Clear | `TLS of string option ]

val paf_transmission : transmission Mimic.value

val run :
  ctx:Mimic.ctx ->
  (Ipaddr.t * int) option Alpn.client_handler ->
  [ `V1 of H1.Request.t | `V2 of H2.Request.t ] ->
  (Alpn.alpn_response, [> Mimic.error ]) result Lwt.t
(** [run ~ctx handler req] sends an HTTP request (H2 or HTTP/1.1) to a peer
    which can be reached {i via} the given Mimic's [ctx]. If the connection is
    recognized as a {!tls_protocol}, we proceed an ALPN challenge between what
    the user chosen and what the peer can handle. Otherwise, we send a simple
    HTTP/1.1 request or a [h2c] request. *)
