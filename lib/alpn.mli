(** ALPN support.

    [Alpn] depend on [http/af] & [h2] and choose them because they share the
    same {!Paf.RUNTIME} interface. [Alpn] does not require [ocaml-tls] so it's
    possible to use OpenSSL. It requires, at least:

    - Something to extract ALPN result from the TLS {i flow}
    - Something to represent as the string the peer (useful for over-framework)
    - An injection function (available from [mimic])

    In other words, [Alpn] did the only choice to trust on [http/af] & [h2] to
    handle HTTP/1.0, HTTP/1.1 and H2 protocols. *)

type 'c capability = Rd : [ `read ] capability | Wr : [ `write ] capability

type body =
  | Body_HTTP_1_1 : 'c capability * 'c httpaf_body -> body
  | Body_HTTP_2_0 : 'c capability * 'c H2.Body.t -> body

and _ httpaf_body =
  | Body_wr : Httpaf.Body.Writer.t -> [ `write ] httpaf_body
  | Body_rd : Httpaf.Body.Reader.t -> [ `read ] httpaf_body

type response =
  | Response_HTTP_1_1 of Httpaf.Response.t
  | Response_HTTP_2_0 of H2.Response.t

type request =
  | Request_HTTP_1_1 of Httpaf.Request.t
  | Request_HTTP_2_0 of H2.Request.t

type reqd = Reqd_HTTP_1_1 of Httpaf.Reqd.t | Reqd_HTTP_2_0 of H2.Reqd.t

type headers =
  | Headers_HTTP_1_1 of Httpaf.Headers.t
  | Headers_HTTP_2_0 of H2.Headers.t

type server_error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

type 'flow info = {
  alpn : 'flow -> string option;
  peer : 'flow -> string;
  injection : 'flow -> Mimic.flow;
}
(** The type of information from a ['flow]:

    - [alpn] is a fonction which is able to extract the result of the
      negotiation between the client & the server about which protocol we need
      to start
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

val service :
  'flow info ->
  error_handler:
    (string -> ?request:request -> server_error -> (headers -> body) -> unit) ->
  request_handler:(string -> reqd -> unit) ->
  ('t -> ('flow, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
  ('t -> unit Lwt.t) ->
  't Paf.service
(** [service info ~error_handler ~request_handler accept close] creates a new
    {!type:Paf.service} over the {i socket} ['t]. From the given implementation
    of [accept] and [close], we are able to instantiate the {i main loop}. Then,
    from the given [info], we extract informations such the application layer
    protocol and choose which protocol we will use. Currently, if [info.alpn]
    returns:

    - [Some "http/1.0" | Some "http/1.1" | None], we launch an [http/af] service
    - [Some "h2"], we launch an [h2] service

    The user is able to identify which protocol we launched by {!resp_handler}.
    The returned service can be run with {!Paf.serve}. Here is an example with
    [Lwt_unix.file_descr] and the TCP/IP transmission protocol (without ALPN
    negotiation):

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

      let service = Alpn.service info
        ~error_handler
        ~request_handler
        accept Lwt_unix.close

      let fiber =
        let t = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Lwt_unix.bind t (Unix.ADDR_INET (Unix.inet_addr_loopback, 8080))
        >>= fun () ->
        let `Initialized th = Paf.serve
          ~sleep:(Lwt_unix.sleep <.> Int64.to_float)
          service t in th

      let () = Lwt_main.run fiber
    ]} *)

type client_error =
  [ `Exn of exn
  | `Malformed_response of string
  | `Invalid_response_body_length_v1 of Httpaf.Response.t
  | `Invalid_response_body_length_v2 of H2.Response.t
  | `Protocol_error of H2.Error_code.t * string ]

val error_handler_v1 :
  'edn ->
  ('edn -> client_error -> unit) ->
  Httpaf.Client_connection.error ->
  unit

val error_handler_v2 :
  'edn -> ('edn -> client_error -> unit) -> H2.Client_connection.error -> unit

val run :
  sleep:Paf.sleep ->
  ?alpn:string ->
  error_handler:('edn -> client_error -> unit) ->
  response_handler:('edn -> response -> body -> unit) ->
  'edn ->
  [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
  Mimic.flow ->
  (body, [> `Msg of string ]) result Lwt.t
(** [run ~sleep ?alpn ~error_handler ~response_handler edn req flow] tries
    communitate to [edn] via [flow] with a certain protocol according to the
    given [alpn] value and the given request. It returns the body of the request
    to allow the user to write on it (and communicate then with the server).

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
        | Ok flow ->
            run
              ~sleep:(Lwt_unix.sleep <.> Int64.to_float)
              ?alpn:None ~error_handler ~response_handler uri request flow
    ]} *)
