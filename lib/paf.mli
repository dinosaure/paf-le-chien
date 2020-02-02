open Tuyau_mirage

module Make (StackV4 : Mirage_stack.V4) : sig
  module TCP : module type of Tuyau_mirage_tcp.Make(StackV4)

  val tls_endpoint : (TCP.endpoint * Tls.Config.client) key
  val tls_configuration : (TCP.configuration * Tls.Config.server) key

  val http
    :  ?config:Httpaf.Config.t
    -> error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler)
    -> request_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler)
    -> TCP.service -> (unit, [> Tuyau_mirage.error ]) result Lwt.t

  val https
    :  ?config:Httpaf.Config.t
    -> error_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.error_handler)
    -> request_handler:(Ipaddr.V4.t * int -> Httpaf.Server_connection.request_handler)
    -> TCP.service Tuyau_mirage_tls.service_with_tls
    -> (unit, [> Tuyau_mirage.error ]) result Lwt.t
end
