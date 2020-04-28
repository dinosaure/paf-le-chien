open Conduit_mirage
open Conduit_mirage_tls

module Make (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) : sig
  module TCP : module type of Conduit_mirage_tcp.Make(StackV4)

  exception Send_error  of string
  exception Recv_error  of string
  exception Close_error of string

  val tls_endpoint : (TCP.endpoint * Tls.Config.client) key
  val tls_configuration : (TCP.configuration * Tls.Config.server) key

  val tls_protocol : TCP.protocol protocol_with_tls Witness.protocol
  val tls_service : (TCP.service service_with_tls * TCP.protocol protocol_with_tls) Witness.service

  val http
    :  ?config:Httpaf.Config.t
    -> error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler)
    -> request_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler)
    -> TCP.service -> (unit, [> Conduit_mirage.error ]) result Lwt.t

  val https
    :  ?config:Httpaf.Config.t
    -> error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler)
    -> request_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler)
    -> TCP.service Conduit_mirage_tls.service_with_tls
    -> (unit, [> Conduit_mirage.error ]) result Lwt.t

  val request
    :  ?key:'a key
    -> ?config:Httpaf.Config.t
    -> resolvers:Conduit.resolvers
    -> error_handler:(Conduit_mirage.flow -> (Ipaddr.V4.t * int) option -> Httpaf.Client_connection.error_handler)
    -> response_handler:((Ipaddr.V4.t * int) option -> Httpaf.Client_connection.response_handler)
    -> [ `raw ] Domain_name.t
    -> Httpaf.Request.t
    -> ([ `write ] Httpaf.Body.t, [> Conduit_mirage.error ]) result Lwt.t
end
