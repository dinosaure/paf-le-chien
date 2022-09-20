(** ALPN support.

    [Alpn] depend on [http/af] & [h2] and choose them because they share the
    same {!Paf.RUNTIME} interface. [Alpn] does not require [ocaml-tls] so it's
    possible to use OpenSSL. It requires, at least:

    - Something to extract ALPN result from the TLS {i flow}
    - Something to represent as the string the peer (useful for over-framework)
    - An injection function (available from [mimic])

    In other words, [Alpn] did the only choice to trust on [http/af] & [h2] to
    handle HTTP/1.0, HTTP/1.1 and H2 protocols. *)

module type REQD = sig
  type t
  type request
  type response

  module Body : sig
    type ro
    type wo
  end

  val request : t -> request
  val request_body : t -> Body.ro
  val response : t -> response option
  val response_exn : t -> response
  val respond_with_string : t -> response -> string -> unit
  val respond_with_bigstring : t -> response -> Bigstringaf.t -> unit

  val respond_with_streaming :
    t -> ?flush_headers_immediately:bool -> response -> Body.wo

  val report_exn : t -> exn -> unit
  val try_with : t -> (unit -> unit) -> (unit, exn) result
end

type http_1_1_protocol =
  (module REQD
     with type t = Httpaf.Reqd.t
      and type request = Httpaf.Request.t
      and type response = Httpaf.Response.t
      and type Body.ro = [ `read ] Httpaf.Body.t
      and type Body.wo = [ `write ] Httpaf.Body.t)

type h2_protocol =
  (module REQD
     with type t = H2.Reqd.t
      and type request = H2.Request.t
      and type response = H2.Response.t
      and type Body.ro = H2.Body.Reader.t
      and type Body.wo = H2.Body.Writer.t)

type ('reqd, 'headers, 'request, 'response, 'ro, 'wo) protocol =
  | HTTP_1_1 :
      http_1_1_protocol
      -> ( Httpaf.Reqd.t,
           Httpaf.Headers.t,
           Httpaf.Request.t,
           Httpaf.Response.t,
           [ `read ] Httpaf.Body.t,
           [ `write ] Httpaf.Body.t )
         protocol
  | H2 :
      h2_protocol
      -> ( H2.Reqd.t,
           H2.Headers.t,
           H2.Request.t,
           H2.Response.t,
           H2.Body.Reader.t,
           H2.Body.Writer.t )
         protocol

val http_1_1 :
  ( Httpaf.Reqd.t,
    Httpaf.Headers.t,
    Httpaf.Request.t,
    Httpaf.Response.t,
    [ `read ] Httpaf.Body.t,
    [ `write ] Httpaf.Body.t )
  protocol

val h2 :
  ( H2.Reqd.t,
    H2.Headers.t,
    H2.Request.t,
    H2.Response.t,
    H2.Body.Reader.t,
    H2.Body.Writer.t )
  protocol

type server_error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]
(** Type of server errors. *)

type ('flow, 'edn) info = {
  alpn : 'flow -> string option;
  peer : 'flow -> 'edn;
  injection : 'flow -> Mimic.flow;
}
(** The type of information from a ['flow]:

    - [alpn] is a function which is able to extract the result of the
      negotiation between the client & the server about which protocol we need
      to use.
    - [peer] returns a [string] representation of the given ['flow] to help to
      print out some logs about this client.
    - [injection] is the function which wraps the given ['flow] to a
      [Mimic.flow].

    For the last function, it can be done if you already registered the protocol
    with [mimic]. In that case, the second value given by [Mimic.register] helps
    you to {i inject} your flow as a [Mimic.flow]:

    {[
      let _, protocol = Mimic.register ~name:"my-protocol" (module My_protocol)

      let injection (flow : My_protocol.flow) : Mimic.flow =
        let module R = (val Mimic.repr protocol) in
        R.T flow
    ]} *)

type ('flow, 'edn) server_handler = {
  error :
    'reqd 'headers 'request 'response 'ro 'wo.
    'edn ->
    ('reqd, 'headers, 'request, 'response, 'ro, 'wo) protocol ->
    ?request:'request ->
    server_error ->
    ('headers -> 'wo) ->
    unit;
  request :
    'reqd 'headers 'request 'response 'ro 'wo.
    'flow ->
    'edn ->
    'reqd ->
    ('reqd, 'headers, 'request, 'response, 'ro, 'wo) protocol ->
    unit;
}
(** The type of handler. To be able to handle http/1.1 and h2 requests with the
    same function, we have chosen to use record with universally quantified
    types. Such design requires some constraints: 1) [error] and [request]
    should be defined at top 2) if they requires extra informations (such as the
    path of file, a value to connect to a database, etc.), they can be used into
    handlers but the record must contains non-curried version of these handlers.
    3) you must use type annotation due to the GADT {!type:protocol}

    For instance, we have a value [db] is required by our request handler. You
    can describe your handler by this way:

    {[
      let error_handler
        : type reqd headers request response ro wo.
          _ -> (reqd, headers, request, response, ro, wo) Alpn.protocol ->
          ?request:request -> _ -> (headers -> wo) -> unit
        = fun edn protocol ?request error respond ->
          match protocol with
          | Alpn.HTTP_1_1 _ ->
            (* everything is specialized to the [Httpaf] module. You can use
               [?request] as an [Httpaf.Request.t option] without type error. *)
          | Alpn.H2 _ ->
            (* everything is specialized to the [H2] module. *)

      let request_handler
        : type reqd headers request response ro wo.
          Database.t -> _ -> _ -> reqd ->
          (reqd, headers, request, response, ro, wo) Alpn.protocol -> unit
        = fun db flow edn reqd -> function
        | Alpn.HTTP_1_1 _ -> ...
        | Alpn.H2 _ -> ...

      let handler db =
        { error= (fun edn protocol ?request error respond ->
                error_handler edn protocol ?request error respond)
        ; request= (fun flow edn reqd protocol ->
                request_handler db flow end reqd protocol) }
    ]} *)

val service :
  ('flow, 'edn) info ->
  (Mimic.flow, 'edn) server_handler ->
  ('socket -> ('flow, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
  ('t -> ('socket, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
  ('t -> unit Lwt.t) ->
  't Paf.service
(** [service info handler connect accept close] creates a new
    {!type:Paf.service} over the {i socket} ['flow]. From the given
    implementation of [accept] and [close], we are able to instantiate the
    {i main loop}. Then, from the given [info], we extract informations such the
    application layer protocol and choose which protocol we will use. Currently,
    if [info.alpn] returns:

    - [Some "http/1.0" | Some "http/1.1" | None], we launch an [http/af] service
    - [Some "h2"], we launch an [h2] service

    The user is able to identify which protocol we launched by
    {!type:server_handler}. The returned service can be run with {!Paf.serve}.
    Here is an example with [Lwt_unix.file_descr] and the TCP/IP transmission
    protocol (without ALPN negotiation):

    {[
      let _, protocol
        : Unix.sockaddr Mimic.value
          * (Unix.sockaddr, Lwt_unix.file_descr) Mimic.protocol
        = Mimic.register ~name:"lwt-tcp" (module TCP)

      let accept t =
        Lwt.catch begin fun () ->
          Lwt_unix.accept >>= fun (socket, _) ->
          Lwt.return_ok socket
        end @@ function
        | Unix.Unix_error (err, f, v) ->
          Lwt.return_error (`Unix (err, f, v))
        | exn -> raise exn

      let info =
        let module R = (val Mimic.register protocol) in
        { Alpn.alpn= const None
        ; Alpn.peer= (fun socket ->
          sockaddr_to_string (Lwt_unix.getpeername socket))
        ; Alpn.injection=
          (fun socket -> R.T socket) }

      let service = Alpn.service info handler
        accept Lwt_unix.close

      let fiber =
        let t = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Lwt_unix.bind t (Unix.ADDR_INET (Unix.inet_addr_loopback, 8080))
        >>= fun () ->
        let `Initialized th = Paf.serve service t in th

      let () = Lwt_main.run fiber
    ]} *)

type client_error =
  [ `Exn of exn
  | `Malformed_response of string
  | `Invalid_response_body_length_v1 of Httpaf.Response.t
  | `Invalid_response_body_length_v2 of H2.Response.t
  | `Protocol_error of H2.Error_code.t * string ]
(** Type of client errors. *)

type 'edn client_handler = {
  error :
    'reqd 'headers 'request 'response 'ro 'wo.
    'edn ->
    ('reqd, 'headers, 'request, 'response, 'ro, 'wo) protocol ->
    client_error ->
    unit;
  response :
    'reqd 'headers 'request 'response 'ro 'wo.
    Mimic.flow ->
    'edn ->
    'response ->
    'ro ->
    ('reqd, 'headers, 'request, 'response, 'ro, 'wo) protocol ->
    unit;
}
(** The type of client handler. As {!type:server_handler}, we have chosen to use
    a record with universally quantified types. Please follow the explanation
    given about {!type:server_handler} to understand how to use it. *)

type alpn_response =
  | Response_HTTP_1_1 :
      ([ `write ] Httpaf.Body.t * Httpaf.Client_connection.t)
      -> alpn_response
  | Response_H2 : H2.Body.Writer.t * H2.Client_connection.t -> alpn_response

val run :
  ?alpn:string ->
  'edn client_handler ->
  'edn ->
  [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
  Mimic.flow ->
  (alpn_response, [> `Msg of string ]) result Lwt.t
(** [run ?alpn ~client_handler edn req flow] tries communicate to [edn] via
    [flow] with a certain protocol according to the given [alpn] value and the
    given request. It returns the body of the request to allow the user to write
    on it (and communicate then with the server).

    [run] does only the ALPN dispatch. It does not instantiate the connection
    and it does not try to upgrade the protocol. It just choose the right HTTP
    protocol according to:

    - the given [alpn] value
    - the given [request] (if you want to communicate via HTTP/1.1 or H2)

    Here is an example with [mimic]:

    {[
      let run uri request =
        let ctx = ctx_of_uri uri in
        (* See Mimic for more details. *)
        Mimic.resolve ctx >>= function
        | Error _ as err -> Lwt.return err
        | Ok flow -> run ?alpn:None handler uri request flow
    ]} *)
