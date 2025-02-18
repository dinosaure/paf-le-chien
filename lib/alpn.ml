let src = Logs.Src.create "paf-alpn"

module Log = (val Logs.src_log src : Logs.LOG)

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
     with type t = H1.Reqd.t
      and type request = H1.Request.t
      and type response = H1.Response.t
      and type Body.ro = H1.Body.Reader.t
      and type Body.wo = H1.Body.Writer.t)

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
      -> ( H1.Reqd.t,
           H1.Headers.t,
           H1.Request.t,
           H1.Response.t,
           H1.Body.Reader.t,
           H1.Body.Writer.t )
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

let http_1_1 =
  let module M = struct
    include H1.Reqd

    type request = H1.Request.t
    type response = H1.Response.t

    module Body = struct
      type ro = H1.Body.Reader.t
      type wo = H1.Body.Writer.t
    end

    let respond_with_streaming t ?flush_headers_immediately response =
      respond_with_streaming t ?flush_headers_immediately response
  end in
  (module M : REQD
    with type t = H1.Reqd.t
     and type request = H1.Request.t
     and type response = H1.Response.t
     and type Body.ro = H1.Body.Reader.t
     and type Body.wo = H1.Body.Writer.t)

let h2 =
  let module M = struct
    include H2.Reqd

    type request = H2.Request.t
    type response = H2.Response.t

    module Body = struct
      type ro = H2.Body.Reader.t
      type wo = H2.Body.Writer.t
    end
  end in
  (module M : REQD
    with type t = H2.Reqd.t
     and type request = H2.Request.t
     and type response = H2.Response.t
     and type Body.ro = H2.Body.Reader.t
     and type Body.wo = H2.Body.Writer.t)

module H1_Client_connection = struct
  include H1.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield | `Upgrade ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t H2.IOVec.t list
         | `Yield
         | `Upgrade ])
end

type ('flow, 'edn) info = {
  alpn : 'flow -> string option;
  peer : 'flow -> 'edn;
  injection : 'flow -> Mimic.flow;
}

type server_error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

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

module H2_Server_connection = struct
  include H2.Server_connection

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t H2.IOVec.t list
         | `Yield
         | `Upgrade ])
end

let service :
    ('flow, 'edn) info ->
    (Mimic.flow, 'edn) server_handler ->
    ('socket -> ('flow, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
    ('t -> ('socket, ([> `Closed | `Msg of string ] as 'error)) result Lwt.t) ->
    ('t -> unit Lwt.t) ->
    't Paf.service =
 fun info handler connect accept close ->
  let connection flow =
    match info.alpn flow with
    | Some "http/1.0" | Some "http/1.1" | None ->
        let edn = info.peer flow in
        let flow = info.injection flow in
        let error_handler ?request error respond =
          handler.error edn (HTTP_1_1 http_1_1) ?request
            (error :> server_error)
            respond in
        let request_handler' reqd =
          handler.request flow edn reqd (HTTP_1_1 http_1_1) in
        let conn = H1.Server_connection.create ~error_handler request_handler' in
        Lwt.return_ok (flow, Paf.Runtime ((module H1.Server_connection), conn))
    | Some "h2" ->
        let edn = info.peer flow in
        let flow = info.injection flow in
        let error_handler ?request error respond =
          handler.error edn (H2 h2) ?request (error :> server_error) respond
        in
        let request_handler' reqd = handler.request flow edn reqd (H2 h2) in
        let conn = H2.Server_connection.create ~error_handler request_handler' in
        Lwt.return_ok (flow, Paf.Runtime ((module H2_Server_connection), conn))
    | Some protocol ->
        Lwt.return_error (`Msg (Fmt.str "Invalid protocol %S." protocol)) in
  Paf.service connection connect accept close

type client_error =
  [ `Exn of exn
  | `Malformed_response of string
  | `Invalid_response_body_length_v1 of H1.Response.t
  | `Invalid_response_body_length_v2 of H2.Response.t
  | `Protocol_error of H2.Error_code.t * string ]

type common_error = [ `Exn of exn | `Malformed_response of string ]

let to_client_error_v1 = function
  | `Invalid_response_body_length response ->
      `Invalid_response_body_length_v1 response
  | #common_error as err -> (err :> client_error)

let to_client_error_v2 = function
  | `Invalid_response_body_length response ->
      `Invalid_response_body_length_v2 response
  | (`Exn _ | `Malformed_response _ | `Protocol_error _) as err -> err

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

module H2_Client_connection = struct
  include H2.Client_connection

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t H2.IOVec.t list
         | `Yield
         | `Upgrade ])
end

type alpn_response =
  | Response_HTTP_1_1 :
      (H1.Body.Writer.t * H1.Client_connection.t)
      -> alpn_response
  | Response_H2 : H2.Body.Writer.t * H2.Client_connection.t -> alpn_response

let run ?alpn handler edn request flow =
  match (alpn, request) with
  | (Some "h2" | None), `V2 request ->
      let error_handler error =
        handler.error edn (H2 h2) (to_client_error_v2 error) in
      let response_handler response body =
        handler.response flow edn response body (H2 h2) in
      let conn =
        H2.Client_connection.create ?config:None ?push_handler:None
          ~error_handler () in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler in
      Lwt.async (fun () -> Paf.run (module H2_Client_connection) conn flow) ;
      Lwt.return_ok (Response_H2 (body, conn))
  | (Some "http/1.1" | None), `V1 request ->
      let error_handler error =
        handler.error edn (HTTP_1_1 http_1_1) (to_client_error_v1 error) in
      let response_handler response body =
        handler.response flow edn response body (HTTP_1_1 http_1_1) in
      let body, conn =
        H1.Client_connection.request request ~error_handler ~response_handler
      in
      Lwt.async (fun () -> Paf.run (module H1_Client_connection) conn flow) ;
      Lwt.return_ok (Response_HTTP_1_1 (body, conn))
  | Some protocol, _ ->
      Lwt.return_error
        (`Msg (Fmt.str "Invalid Application layer protocol: %S" protocol))

let http_1_1 = HTTP_1_1 http_1_1
let h2 = H2 h2
