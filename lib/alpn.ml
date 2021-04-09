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

let response_handler_v1_0 edn f resp body =
  f edn (Resp_handler (HTTP_1_0, resp, body))

let response_handler_v1_1 edn f resp body =
  f edn (Resp_handler (HTTP_1_1, resp, body))

let response_handler_v2_0 edn f resp body =
  f edn (Resp_handler (HTTP_2_0, resp, body))

let request_handler_v1 edn f reqd = f edn (Reqd_handler (HTTP_1_1, reqd))

let request_handler_v2 edn f reqd = f edn (Reqd_handler (HTTP_2_0, reqd))

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

type 'flow info = {
  alpn : 'flow -> string option;
  peer : 'flow -> string;
  injection : 'flow -> Mimic.flow;
}

type server_error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

let error_handler_v1 edn f ?request error
    (response : Httpaf.Headers.t -> [ `write ] Httpaf.Body.t) =
  let request = Option.map (fun req -> Request (HTTP_1_1, req)) request in
  let response = function
    | Headers (HTTP_1_1, headers) -> Body (HTTP_1_1, response headers)
    | _ -> assert false in
  f edn ?request (error :> server_error) response

let error_handler_v2 edn f ?request error
    (response : H2.Headers.t -> [ `write ] H2.Body.t) =
  let request = Option.map (fun req -> Request (HTTP_2_0, req)) request in
  let response = function
    | Headers (HTTP_2_0, headers) -> Body (HTTP_2_0, response headers)
    | _ -> assert false in
  f edn ?request (error :> server_error) response

let serve info ~error_handler ~request_handler accept close =
  let connection flow =
    match info.alpn flow with
    | Some "http/1.0" | Some "http/1.1" | None ->
        let edn = info.peer flow in
        let flow = info.injection flow in
        let error_handler = error_handler_v1 edn error_handler in
        let request_handler = request_handler_v1 edn request_handler in
        let conn =
          Httpaf.Server_connection.create ~error_handler request_handler in
        Lwt.return_ok
          (flow, Paf.Runtime ((module Httpaf.Server_connection), conn))
    | Some "h2" ->
        let edn = info.peer flow in
        let flow = info.injection flow in
        let error_handler = error_handler_v2 edn error_handler in
        let request_handler = request_handler_v2 edn request_handler in
        let conn = H2.Server_connection.create ~error_handler request_handler in
        Lwt.return_ok (flow, Paf.Runtime ((module H2.Server_connection), conn))
    | Some protocol ->
        Lwt.return_error (Rresult.R.msgf "Invalid protocol %S." protocol) in
  Paf.service connection accept close

type client_error =
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
      let error_handler = error_handler_v1 edn error_handler in
      let response_handler = response_handler_v1_0 edn response_handler in
      let body, conn =
        Httpaf.Client_connection.request request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module Httpaf_Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body (HTTP_1_0, body))
  | (Some "http/1.1" | None), `V1 request ->
      let error_handler = error_handler_v1 edn error_handler in
      let response_handler = response_handler_v1_1 edn response_handler in
      let body, conn =
        Httpaf.Client_connection.request request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module Httpaf_Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body (HTTP_1_1, body))
  | Some protocol, _ ->
      Lwt.return_error
        (Rresult.R.msgf "Invalid Application layer protocol: %S" protocol)
