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

type common_error = [ `Exn of exn | `Malformed_response of string ]

let error_handler_v1 edn f = function
  | #common_error as err -> f edn err
  | `Invalid_response_body_length resp ->
      f edn (`Invalid_response_body_length_v1 resp)

let error_handler_v2 edn f = function
  | #common_error as err -> f edn err
  | `Protocol_error _ as err -> f edn err
  | `Invalid_response_body_length resp ->
      f edn (`Invalid_response_body_length_v2 resp)

let response_handler_v1_0 edn f resp body =
  f edn (Handler (HTTP_1_0, resp, body))

let response_handler_v1_1 edn f resp body =
  f edn (Handler (HTTP_1_1, resp, body))

let response_handler_v2_0 edn f resp body =
  f edn (Handler (HTTP_2_0, resp, body))

let run ~sleep ?alpn ~error_handler ~response_handler edn request flow =
  match (alpn, request) with
  | (Some "h2" | None), `V2 request ->
      let error_handler = error_handler_v2 edn error_handler in
      let response_handler = response_handler_v2_0 edn response_handler in
      let conn =
        H2.Client_connection.create ?config:None ?push_handler:None
          ~error_handler in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module H2.Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body (HTTP_2_0, body))
  | Some "http/1.0", `V1 request ->
      let conn = Httpaf.Client_connection.create ?config:None in
      let error_handler = error_handler_v1 edn error_handler in
      let response_handler = response_handler_v1_0 edn response_handler in
      let body =
        Httpaf.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module Httpaf.Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body (HTTP_1_0, body))
  | (Some "http/1.1" | None), `V1 request ->
      let conn = Httpaf.Client_connection.create ?config:None in
      let error_handler = error_handler_v1 edn error_handler in
      let response_handler = response_handler_v1_1 edn response_handler in
      let body =
        Httpaf.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module Httpaf.Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body (HTTP_1_1, body))
  | Some protocol, _ ->
      Lwt.return_error
        (Rresult.R.msgf "Invalid Application layer protocol: %S" protocol)
