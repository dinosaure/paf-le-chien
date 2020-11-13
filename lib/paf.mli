module Conduit_mirage_tls :
    module type of Conduit_tls.Make (Lwt) (Conduit_mirage)

module Make (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) : sig
  module TCP : module type of Conduit_mirage_tcp.Make (StackV4)

  val tls_protocol :
    ( (StackV4.t, Ipaddr.V4.t) Conduit_mirage_tcp.endpoint * Tls.Config.client,
      TCP.protocol Conduit_mirage_tls.protocol_with_tls )
    Conduit_mirage.protocol

  val tls_service :
    ( StackV4.t Conduit_mirage_tcp.configuration * Tls.Config.server,
      TCP.service Conduit_mirage_tls.service_with_tls,
      TCP.protocol Conduit_mirage_tls.protocol_with_tls )
    Conduit_mirage.Service.service

  exception Send_error of string

  exception Recv_error of string

  exception Close_error of string

  val http :
    ?config:Httpaf.Config.t ->
    error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:
      (Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler) ->
    TCP.service ->
    (unit, [> Conduit_mirage.error ]) result Lwt.t

  val https :
    ?config:Httpaf.Config.t ->
    error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:
      (Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler) ->
    TCP.service Conduit_mirage_tls.service_with_tls ->
    (unit, [> Conduit_mirage.error ]) result Lwt.t

  val request :
    ?config:Httpaf.Config.t ->
    resolvers:Conduit.resolvers ->
    error_handler:
      (Conduit_mirage.flow ->
      (Ipaddr.V4.t * int) option ->
      Httpaf.Client_connection.error_handler) ->
    response_handler:
      ((Ipaddr.V4.t * int) option -> Httpaf.Client_connection.response_handler) ->
    Conduit.Endpoint.t ->
    Httpaf.Request.t ->
    ([ `write ] Httpaf.Body.t, [> Conduit_mirage.error ]) result Lwt.t
end
