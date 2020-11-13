(** MirageOS compatible layer of HTTP/AF. *)

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

  (** {3 Exceptions.}

      On the error handler, the user can get such exception which comes from the
      protocol implementation (TLS or not). *)

  exception Send_error of string

  exception Recv_error of string

  exception Close_error of string

  (** {3 Server part.}

      A server expects the initialised {i socket} given by
      [Conduit.Service.init 'cfg ~service:TCP.service] (for a simple HTTP
      server) or [Conduit.Service.init 'cfg ~service:tls_service] (for a HTTPS
      server).

      It requires values to create a connection handler that will service
      individual requests with [request_handler]. It returns an {i infinite}
      {i lwt} promise (cancellable). *)

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

  (** {3 Client part.}

      To send one (and unique) request, the user must give:

      - a Conduit [resolvers]
      - a representation of the {i endpoint}
      - a response handler
      - the HTTP request

      It returns the {i body} of the request where the user can write on it.

      The given [resolvers] and the {i endpoint} can be made by an [Uri.t] such
      as:

      {[
        let alpn uri =
          match (Uri.scheme uri, Uri.host uri) with
          | Some "http", Some domain_name ->
              let edn = Conduit.Endpoint.of_string domain_name in
              let port = Option.value ~default:80 (Uri.port uri) in
              let resolvers =
                Conduit.add TCP.protocol (resolv ~port) Conduit.empty in
              (edn, resolvers)
          | Some "https", Some domain_name ->
              let edn = Conduit.Endpoint.of_string domain_name in
              let port = Option.value ~default:443 (Uri.port uri) in
              let resolvers =
                Conduit.add tls_protocol (tls_resolv ~port) Conduit.empty in
              (edn, resolvers)
          | _ -> invalid_arg "Invalid URI: %a" Uri.pp uri
      ]} *)

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
