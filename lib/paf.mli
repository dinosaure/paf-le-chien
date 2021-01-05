(** MirageOS compatible layer of HTTP/AF. *)

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) : sig
  exception Error of Mimic.error

  val tcp_edn : (Stack.t * Ipaddr.t * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option
    * Tls.Config.client
    * Stack.t
    * Ipaddr.t
    * int)
    Mimic.value

  type service

  val init : port:int -> Stack.t -> service Lwt.t

  val http :
    ?config:Httpaf.Config.t ->
    error_handler:(Ipaddr.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:(Ipaddr.t * int -> Httpaf.Server_connection.request_handler) ->
    service ->
    [ `Initialized of unit Lwt.t ]

  val https :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    error_handler:(Ipaddr.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:(Ipaddr.t * int -> Httpaf.Server_connection.request_handler) ->
    service ->
    [ `Initialized of unit Lwt.t ]

  val request :
    ?config:Httpaf.Config.t ->
    ctx:Mimic.ctx ->
    error_handler:
      (Mimic.flow ->
      (Ipaddr.t * int) option ->
      Httpaf.Client_connection.error_handler) ->
    response_handler:
      ((Ipaddr.t * int) option -> Httpaf.Client_connection.response_handler) ->
    Httpaf.Request.t ->
    ([ `write ] Httpaf.Body.t, [> Mimic.error ]) result Lwt.t
end
