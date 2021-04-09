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
        protocol

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

val response_handler_v1_0 :
  'edn ->
  ('edn -> [ `read ] resp_handler -> unit) ->
  Httpaf.Response.t ->
  [ `read ] Httpaf.Body.t ->
  unit

val response_handler_v1_1 :
  'edn ->
  ('edn -> [ `read ] resp_handler -> unit) ->
  Httpaf.Response.t ->
  [ `read ] Httpaf.Body.t ->
  unit

val response_handler_v2_0 :
  'edn ->
  ('edn -> [ `read ] resp_handler -> unit) ->
  H2.Response.t ->
  [ `read ] H2.Body.t ->
  unit

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
  response_handler:('edn -> [ `read ] resp_handler -> unit) ->
  'edn ->
  [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
  Mimic.flow ->
  ([ `write ] body, [> `Msg of string ]) result Lwt.t
