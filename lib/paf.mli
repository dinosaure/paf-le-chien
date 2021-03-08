(** MirageOS compatible layer of HTTP/AF. *)

module type S = sig
  exception Error of Mimic.error

  type stack

  type service
  (** The type of services. *)

  val init : port:int -> stack -> service Lwt.t
  (** [init ~port stack] returns a {!service} bound on [port] with [stack]. *)

  val http :
    sleep:(int64 -> unit Lwt.t) ->
    ?config:Httpaf.Config.t ->
    ?stop:Lwt_switch.t ->
    error_handler:(Ipaddr.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:(Ipaddr.t * int -> Httpaf.Server_connection.request_handler) ->
    service ->
    [ `Initialized of unit Lwt.t ]
  (** [http ?config ?stop ~error_handler ~request_handler service] promises a
      service loop computation that is ready to receive connections. The inner
      promise is then determined once the service loop has ended - by default,
      only when an error occurs.

      If passed, [stop] is a switch that terminates the service loop, for
      example to limit execution time to 10 seconds:

      {[
        let* service = init ~port:80 stack in
        let stop = Lwt_switch.create () in
        let `Initialized server = http ~stop ... service in
        Lwt.both (Lwt_unix.sleep 10. >>= fun () -> Lwt_switch.turn_off stop) server
      ]}

      This is useful when subsequent actions are reliant on the service loop
      having begin, such as when testing with a client-server pair:

      {[
        let* service = init ~port:80 stack in
        let `Initialized server = http ... service in
        Lwt.both server (client >|= signal_stop)
      ]} *)

  val https :
    sleep:(int64 -> unit Lwt.t) ->
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    ?stop:Lwt_switch.t ->
    error_handler:(Ipaddr.t * int -> Httpaf.Server_connection.error_handler) ->
    request_handler:(Ipaddr.t * int -> Httpaf.Server_connection.request_handler) ->
    service ->
    [ `Initialized of unit Lwt.t ]
  (** Same as {!http}, but requires a TLS certificate [tls]. *)

  val tcp_edn : (stack * Ipaddr.t * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option
    * Tls.Config.client
    * stack
    * Ipaddr.t
    * int)
    Mimic.value

  val request :
    sleep:(int64 -> unit Lwt.t) ->
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
  (** [request ?config ~ctx ~error_handler ~response_handler req] returns a
      {i open} HTTP body according to the given context [ctx] and the request
      [req].

      To be able to start a simple HTTP connection, you must:

      - know the target IP address
      - create the client {i stack}
      - know the target port (default to [80])

      These values must exist into the given [ctx] to, at least, be able to
      start a TCP/IP connection:

      {[
        let ctx =
          Mimic.add Paf.tcp_edn
            (stack, ipaddr_of_google, 80)
            Mimic.empty Paf.request ~ctx ~error_handler ~response_handler req
      ]}

      The user is able to fill the [ctx] with some clever processes such as a
      DNS resolver:

      {[
        let domain_name = Mimic.make ~name:"domain-name"

        let ipaddr = Mimic.make ~name:"ipaddr"

        let resolver domain_name =
          match Unix.gethostbyname (Domain_name.to_string domain_name) with
          | { Unix.h_addr_list; _ } ->
              if Array.length h_addr_list > 0
              then Lwt.return_some (Ipaddr_unix.of_inet_addr h_addr_list.(0))
              else Lwt.return_none
          | exception _ -> Lwt.return_none

        let stack = Mimic.make ~name:"stack"

        let port = Mimic.make ~name:"port"

        let connect stack ipaddr port = Lwt.return_some (stack, ipaddr, port)

        let ctx =
          let open Mimic in
          fold ipaddr Fun.[ req domain_name ] ~k:resolver Mimic.empty
          |> fold Paf.tcp_edn
               Fun.[ req stack; req ipaddr; dft port 80 ]
               ~k:connect
      ]}

      If [mimic] is able to create a {!tcp_edn} value from the [ctx] and these
      functions, the user will be able to start a TCP/IP connection. [ipaddr]
      will come from a given [domain_name] and if the [port] is missing, we use
      [80] as the default value. The [stack] still is required.

      For a more user-friendly interface, you should take a look into
      [paf.cohttp]. *)
end

module Make (Stack : Mirage_stack.V4V6) : S with type stack = Stack.t

(** {2 Unfunctorized API.}

    Due to the design of Mimic and the ability to inject a {i flow} under the
    [Mimic.flow], a {i unfunctorized} API exists to starting an HTTP server or
    an HTTP client.

    These processes can work, as below, with a TCP/IP stack (with or without
    TLS) or something else. See Mimic for more details. *)

type flow = Mimic.flow

exception Error of Mimic.error

val create_server_connection_handler :
  sleep:(int64 -> unit Lwt.t) ->
  ?config:Httpaf.Config.t ->
  request_handler:('endpoint -> Httpaf.Server_connection.request_handler) ->
  error_handler:('endpoint -> Httpaf.Server_connection.error_handler) ->
  'endpoint ->
  flow ->
  unit Lwt.t

val request :
  sleep:(int64 -> unit Lwt.t) ->
  ?config:Httpaf.Config.t ->
  flow ->
  'endpoint ->
  Httpaf.Request.t ->
  error_handler:(flow -> 'endpoint -> Httpaf.Client_connection.error_handler) ->
  response_handler:('endpoint -> Httpaf.Client_connection.response_handler) ->
  [ `write ] Httpaf.Body.t
