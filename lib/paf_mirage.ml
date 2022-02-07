module type S = sig
  type stack

  type ipaddr

  module TCP : sig
    include Mirage_flow.S

    val dst : flow -> ipaddr * int
  end

  module TLS : module type of Tls_mirage.Make (TCP)

  val tcp_protocol : (stack * ipaddr * int, TCP.flow) Mimic.protocol

  val tcp_edn : (stack * ipaddr * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option * Tls.Config.client * stack * ipaddr * int)
    Mimic.value

  val tls_protocol :
    ( [ `host ] Domain_name.t option * Tls.Config.client * stack * ipaddr * int,
      TLS.flow )
    Mimic.protocol

  type t

  type dst = ipaddr * int

  val init : port:int -> stack -> t Lwt.t

  val accept : t -> (TCP.flow, [> `Closed ]) result Lwt.t

  val close : t -> unit Lwt.t

  val http_service :
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (TCP.flow -> dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val https_service :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (TLS.flow -> dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val alpn_service :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t * H2.Config.t ->
    error_handler:
      (dst ->
      ?request:Alpn.request ->
      Alpn.server_error ->
      (Alpn.headers -> Alpn.body) ->
      unit) ->
    (dst -> Alpn.reqd -> unit) ->
    t Paf.service

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]
end

module Make (Time : Mirage_time.S) (Stack : Tcpip.Tcp.S) :
  S
    with type stack = Stack.t
     and type TCP.flow = Stack.flow
     and type ipaddr = Stack.ipaddr = struct
  open Lwt.Infix

  type ipaddr = Stack.ipaddr

  type dst = ipaddr * int

  module TCP = struct
    let src = Logs.Src.create "paf-tcp"

    module Log = (val Logs.src_log src : Logs.LOG)

    include Stack

    type endpoint = Stack.t * Stack.ipaddr * int

    type nonrec write_error =
      [ `Write of write_error | `Connect of error | `Closed ]

    let pp_write_error ppf = function
      | `Write err | (`Closed as err) -> pp_write_error ppf err
      | `Connect err -> pp_error ppf err

    let write flow cs =
      write flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect (stack, ipaddr, port) =
      create_connection stack (ipaddr, port) >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Connect err)
  end

  module TLS = struct
    let src = Logs.Src.create "paf-tls"

    module Log = (val Logs.src_log src : Logs.LOG)

    include Tls_mirage.Make (TCP)

    type endpoint =
      [ `host ] Domain_name.t option
      * Tls.Config.client
      * Stack.t
      * Stack.ipaddr
      * int

    let connect (host, cfg, stack, ipaddr, port) =
      Stack.create_connection stack (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow -> client_of_flow cfg ?host flow
  end

  let src = Logs.Src.create "paf-layer"

  module Log = (val Logs.src_log src : Logs.LOG)

  type stack = Stack.t

  let tcp_edn, tcp_protocol = Mimic.register ~name:"tcp" (module TCP)

  let tls_edn, tls_protocol =
    Mimic.register ~priority:10 ~name:"tls" (module TLS)

  type t = {
    stack : Stack.t;
    queue : Stack.flow Queue.t;
    condition : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    mutable closed : bool;
  }

  let init ~port stack =
    let queue = Queue.create () in
    let condition = Lwt_condition.create () in
    let mutex = Lwt_mutex.create () in
    let listener flow =
      Lwt_mutex.lock mutex >>= fun () ->
      Queue.push flow queue ;
      Lwt_condition.signal condition () ;
      Lwt_mutex.unlock mutex ;
      Lwt.return () in
    Stack.listen ~port stack listener ;
    Lwt.return { stack; queue; condition; mutex; closed = false }

  let rec accept ({ queue; condition; mutex; _ } as t) =
    Lwt_mutex.lock mutex >>= fun () ->
    let rec await () =
      if Queue.is_empty queue && not t.closed
      then Lwt_condition.wait condition ~mutex >>= await
      else Lwt.return_unit in
    await () >>= fun () ->
    match Queue.pop queue with
    | flow ->
        Lwt_mutex.unlock mutex ;
        Lwt.return_ok flow
    | exception Queue.Empty ->
        if t.closed
        then (
          Lwt_mutex.unlock mutex ;
          Lwt.return_error `Closed)
        else (
          Lwt_mutex.unlock mutex ;
          accept t)

  let close ({ condition; _ } as t) =
    t.closed <- true ;
    (* Stack.disconnect stack >>= fun () -> *)
    Lwt_condition.signal condition () ;
    Lwt.return_unit

  let http_service ?config ~error_handler request_handler =
    let module R = (val Mimic.repr tcp_protocol) in
    let connection flow =
      let dst = Stack.dst flow in
      let error_handler = error_handler dst in
      let request_handler = request_handler flow dst in
      let conn =
        Httpaf.Server_connection.create ?config ~error_handler request_handler
      in
      Lwt.return_ok
        (R.T flow, Paf.Runtime ((module Httpaf.Server_connection), conn)) in
    Paf.service connection accept close

  let https_service ~tls ?config ~error_handler request_handler =
    let module R = (val Mimic.repr tls_protocol) in
    let accept t =
      accept t >>= function
      | Error _ as err -> Lwt.return err
      | Ok flow -> (
          let dst = Stack.dst flow in
          TLS.server_of_flow tls flow >>= function
          | Ok flow -> Lwt.return_ok (dst, flow)
          | Error `Closed ->
              (* XXX(dinosaure): be care! [`Closed] at this stage does not mean
               * that the bound socket is closed but the socket with the peer is
               * closed. *)
              Lwt.return_error (`Write `Closed)
          | Error err -> Stack.close flow >>= fun () -> Lwt.return_error err)
    in
    let connection (dst, flow) =
      let error_handler = error_handler dst in
      let request_handler = request_handler flow dst in
      let conn =
        Httpaf.Server_connection.create ?config ~error_handler request_handler
      in
      Lwt.return_ok
        (R.T flow, Paf.Runtime ((module Httpaf.Server_connection), conn)) in
    Paf.service connection accept close

  let alpn =
    let module R = (val Mimic.repr tls_protocol) in
    let alpn_of_tls_connection (_edn, flow) =
      match TLS.epoch flow with
      | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
      | Error _ -> None in
    let peer_of_tls_connection (edn, _flow) = edn in
    (* XXX(dinosaure): [TLS]/[ocaml-tls] should let us to project the underlying
     * [flow] and apply [TCP.dst] on it. *)
    let injection (_edn, flow) = R.T flow in
    {
      Alpn.alpn = alpn_of_tls_connection;
      Alpn.peer = peer_of_tls_connection;
      Alpn.injection;
    }

  let alpn_service ~tls ?config:(_ = (Httpaf.Config.default, H2.Config.default))
      ~error_handler request_handler =
    let accept t =
      accept t >>= function
      | Error _ as err -> Lwt.return err
      | Ok flow -> (
          let dst = Stack.dst flow in
          TLS.server_of_flow tls flow >>= function
          | Ok flow -> Lwt.return_ok (dst, flow)
          | Error `Closed ->
              Lwt.return_error
                (`Msg (Fmt.str "%a" TLS.pp_write_error (`Write `Closed)))
          | Error err ->
              Stack.close flow >>= fun () ->
              Lwt.return_error (`Msg (Fmt.str "%a" TLS.pp_write_error err)))
    in
    Alpn.service alpn ~error_handler ~request_handler accept close

  let serve ?stop service t = Paf.serve ~sleep:Time.sleep_ns ?stop service t
end

type transmission = [ `Clear | `TLS of string option ]

let paf_transmission : transmission Mimic.value =
  Mimic.make ~name:"paf-transmission"

open Lwt.Infix

let rec kind_of_flow : Mimic.edn list -> transmission option = function
  | Mimic.Edn (k, v) :: r -> (
      match Mimic.equal k paf_transmission with
      | Some Mimic.Refl -> Some v
      | None -> kind_of_flow r)
  | [] -> None

let ( >>? ) = Lwt_result.bind

let run ~sleep ~ctx ~error_handler ~response_handler request =
  Mimic.unfold ctx >>? fun ress ->
  Mimic.connect ress >>= fun res ->
  match (res, kind_of_flow ress) with
  | (Error _ as err), _ -> Lwt.return err
  | Ok flow, (Some `Clear | None) ->
      let alpn = match request with `V1 _ -> "http/1.1" | `V2 _ -> "h2c" in
      Alpn.run ~sleep ~alpn ~error_handler ~response_handler flow request flow
  | Ok flow, Some (`TLS alpn) ->
      Alpn.run ~sleep ?alpn ~error_handler ~response_handler flow request flow

module TCPV4V6 (Stack : Tcpip.Stack.V4V6) : sig
  include
    Tcpip.Tcp.S
      with type t = Stack.TCP.t
       and type ipaddr = Ipaddr.t
       and type flow = Stack.TCP.flow

  val connect : Stack.t -> t Lwt.t
end = struct
  include Stack.TCP

  let connect stackv4v6 = Lwt.return (Stack.tcp stackv4v6)
end
