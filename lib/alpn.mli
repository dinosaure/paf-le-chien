(** ALPN support.

    [Alpn] depend on [http/af] & [h2] and choose them because they share the
    same {!Paf.RUNTIME} interface. [Alpn] does not require [ocaml-tls] so it's
    possible to use OpenSSL. It requires, at least:

    - Something to extract ALPN result from the TLS {i flow}
    - Something to represent as the string the peer (useful for over-framework)
    - An injection function (available from [mimic])

    In other words, [Alpn] did the only choice to trust on [http/af] & [h2] to
    handle HTTP/1.0, HTTP/1.1 and H2 protocols. *)

type ('reqd, 'hdr, 'req, 'resp, 'c, 'a) protocol =
  | HTTP_1_0
      : ( Httpaf.Reqd.t,
          Httpaf.Headers.t,
          Httpaf.Request.t,
          Httpaf.Response.t,
          'c,
          'c Httpaf.Body.t )
        protocol
  | HTTP_1_1
      : ( Httpaf.Reqd.t,
          Httpaf.Headers.t,
          Httpaf.Request.t,
          Httpaf.Response.t,
          'c,
          'c Httpaf.Body.t )
        protocol
  | HTTP_2_0
      : ( H2.Reqd.t,
          H2.Headers.t,
          H2.Request.t,
          H2.Response.t,
          'c,
          'c H2.Body.t )
        protocol  (** Type of protocols. *)

type 'c body = Body : (_, _, _, _, 'c, 'v) protocol * 'v -> 'c body

type request = Request : (_, _, 'r, _, _, _) protocol * 'r -> request

type response = Response : (_, _, _, 'r, _, _) protocol * 'r -> response

type headers = Headers : (_, 'hdr, _, _, _, _) protocol * 'hdr -> headers

type 'c resp_handler =
  | Resp_handler : (_, _, _, 'r, 'c, 'v) protocol * 'r * 'v -> 'c resp_handler

type 'c reqd_handler =
  | Reqd_handler : ('r, _, _, _, 'c, 'v) protocol * 'r -> 'c reqd_handler

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

val serve :
  'flow info ->
  error_handler:
    (string ->
    ?request:request ->
    server_error ->
    (headers -> [ `write ] body) ->
    unit) ->
  request_handler:(string -> [ `write ] reqd_handler -> unit) ->
  ('t -> ('flow, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
  ('t -> unit Lwt.t) ->
  't Paf.service
(** [serve info ~error_handler ~request_handler accept close] creates a new
    {!Paf.service} over the {i socket} ['t]. From the given implementation of
    [accept] and [close], we are able to instantiate the {i main loop}. Then,
    from the given [info], we extract informations such the application layer
    protocol and choose which protocol we will use. Currently, if [info.alpn]
    returns:

    - [Some "http/1.0" | Some "http/1.1" | None], we launch an [http/af] service
    - [Some "h2"], we launch an [h2] service

    The user is able to identify which protocol we launched by {!resd_handler}.
    The returned service can be run with {!Paf.serve}. *)

type client_error =
  [ `Exn of exn
  | `Malformed_response of string
  | `Invalid_response_body_length_v1 of Httpaf.Response.t
  | `Invalid_response_body_length_v2 of H2.Response.t
  | `Protocol_error of H2.Error_code.t * string ]

val run :
  sleep:Paf.sleep ->
  ?alpn:string ->
  error_handler:('edn -> client_error -> unit) ->
  response_handler:('edn -> [ `read ] resp_handler -> unit) ->
  'edn ->
  [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
  Mimic.flow ->
  ([ `write ] body, [> `Msg of string ]) result Lwt.t
(** [run ~sleep ?alpn ~error_handler ~response_handler edn req flow] tries
    communitate to [edn] via [flow] with a certain protocol according to the
    given [alpn] value and the given request. It returns the body of the request
    to allow the user to write on it (and communicate then with the server). *)
