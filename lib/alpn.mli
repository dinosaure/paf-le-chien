type ('r, 'c, 'a) protocol =
  | HTTP_1_0 : (Httpaf.Response.t, 'c, 'c Httpaf.Body.t) protocol
  | HTTP_1_1 : (Httpaf.Response.t, 'c, 'c Httpaf.Body.t) protocol
  | HTTP_2_0 : (H2.Response.t, 'c, 'c H2.Body.t) protocol

type response = Response : ('r, _, _) protocol * 'r -> response

type 'c body = Body : (_, 'c, 'v) protocol * 'v -> 'c body

type 'c handler = Handler : ('r, 'c, 'v) protocol * 'r * 'v -> 'c handler

type error =
  [ `Exn of exn
  | `Malformed_response of string
  | `Invalid_response_body_length_v1 of Httpaf.Response.t
  | `Invalid_response_body_length_v2 of H2.Response.t
  | `Protocol_error of H2.Error_code.t * string ]

val error_handler_v1 :
  'edn -> ('edn -> error -> unit) -> Httpaf.Client_connection.error -> unit

val error_handler_v2 :
  'edn -> ('edn -> error -> unit) -> H2.Client_connection.error -> unit

val response_handler_v1_0 :
  'edn ->
  ('edn -> [ `read ] handler -> unit) ->
  Httpaf.Response.t ->
  [ `read ] Httpaf.Body.t ->
  unit

val response_handler_v1_1 :
  'edn ->
  ('edn -> [ `read ] handler -> unit) ->
  Httpaf.Response.t ->
  [ `read ] Httpaf.Body.t ->
  unit

val response_handler_v2_0 :
  'edn ->
  ('edn -> [ `read ] handler -> unit) ->
  H2.Response.t ->
  [ `read ] H2.Body.t ->
  unit

val run :
  sleep:Paf.sleep ->
  ?alpn:string ->
  error_handler:('edn -> error -> unit) ->
  response_handler:('edn -> [ `read ] handler -> unit) ->
  'edn ->
  [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
  Mimic.flow ->
  ([ `write ] body, [> `Msg of string ]) result Lwt.t
