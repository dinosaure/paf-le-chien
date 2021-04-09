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

let response_handler_v1_1 capability edn f resp body =
  f edn (Response_HTTP_1_1 resp) (Body_HTTP_1_1 (capability, body))

let response_handler_v2_0 capability edn f resp body =
  f edn (Response_HTTP_2_0 resp) (Body_HTTP_2_0 (capability, body))

let request_handler_v1 edn f reqd = f edn (Reqd_HTTP_1_1 reqd)

let request_handler_v2 edn f reqd = f edn (Reqd_HTTP_2_0 reqd)

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
    (response : Httpaf.Headers.t -> Httpaf.Body.Writer.t) =
  let request = Option.map (fun req -> Request_HTTP_1_1 req) request in
  let response = function
    | Headers_HTTP_1_1 headers -> Body_HTTP_1_1 (Wr, Body_wr (response headers))
    | _ -> assert false in
  f edn ?request (error :> server_error) response

let error_handler_v2 edn f ?request error
    (response : H2.Headers.t -> [ `write ] H2.Body.t) =
  let request = Option.map (fun req -> Request_HTTP_2_0 req) request in
  let response = function
    | Headers_HTTP_2_0 headers -> Body_HTTP_2_0 (Wr, response headers)
    | _ -> assert false in
  f edn ?request (error :> server_error) response

let service info ~error_handler ~request_handler accept close =
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
        Lwt.return_error (`Msg (Fmt.str "Invalid protocol %S." protocol)) in
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
      let response_handler = response_handler_v2_0 Rd edn response_handler in
      let conn =
        H2.Client_connection.create ?config:None ?push_handler:None
          ~error_handler in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module H2.Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body_HTTP_2_0 (Wr, body))
  | (Some "http/1.1" | None), `V1 request ->
      let error_handler = error_handler_v1 edn error_handler in
      let response_handler resp body =
        response_handler_v1_1 Rd edn response_handler resp (Body_rd body) in
      let conn = Httpaf.Client_connection.create ?config:None in
      let body =
        Httpaf.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () ->
          Paf.run (module Httpaf_Client_connection) ~sleep conn flow) ;
      Lwt.return_ok (Body_HTTP_1_1 (Wr, Body_wr body))
  | Some protocol, _ ->
      Lwt.return_error
        (`Msg (Fmt.str "Invalid Application layer protocol: %S" protocol))
