module type S = sig
  type stack
  type ipaddr

  module TCP : sig
    include Mirage_flow.S

    val dst : flow -> ipaddr * int
    val no_close : flow -> unit
    val to_close : flow -> unit
  end

  module TLS : sig
    type error =
      [ `Tls_alert of Tls.Packet.alert_type
      | `Tls_failure of Tls.Engine.failure
      | `Read of TCP.error
      | `Write of TCP.write_error ]

    type write_error = [ `Closed | error ]

    include
      Mirage_flow.S with type error := error and type write_error := write_error

    val no_close : flow -> unit
    val to_close : flow -> unit
    val epoch : flow -> (Tls.Core.epoch_data, unit) result

    val reneg :
      ?authenticator:X509.Authenticator.t ->
      ?acceptable_cas:X509.Distinguished_name.t list ->
      ?cert:Tls.Config.own_cert ->
      ?drop:bool ->
      flow ->
      (unit, [ write_error | `Msg of string ]) result Lwt.t

    val key_update :
      ?request:bool ->
      flow ->
      (unit, [ write_error | `Msg of string ]) result Lwt.t

    val server_of_flow :
      Tls.Config.server -> TCP.flow -> (flow, write_error) result Lwt.t

    val client_of_flow :
      Tls.Config.client ->
      ?host:[ `host ] Domain_name.t ->
      TCP.flow ->
      (flow, write_error) result Lwt.t
  end

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
    (TLS.flow, dst) Alpn.server_handler ->
    t Paf.service

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]
end

module Make (Stack : Tcpip.Tcp.S) :
  S with type stack = Stack.t and type ipaddr = Stack.ipaddr = struct
  open Lwt.Infix

  type ipaddr = Stack.ipaddr
  type dst = ipaddr * int

  module TCP = struct
    let src = Logs.Src.create "paf-tcp"

    module Log = (val Logs.src_log src : Logs.LOG)
    include Stack

    type nonrec flow = { flow : flow; mutable no_close : bool }
    type endpoint = Stack.t * Stack.ipaddr * int

    type nonrec write_error =
      [ `Write of write_error | `Connect of error | `Closed ]

    let pp_write_error ppf = function
      | `Write err | (`Closed as err) -> pp_write_error ppf err
      | `Connect err -> pp_error ppf err

    let read flow = read flow.flow
    let dst flow = dst flow.flow

    let write flow cs =
      write flow.flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow.flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect (stack, ipaddr, port) =
      create_connection stack (ipaddr, port) >>= function
      | Ok flow -> Lwt.return_ok { flow; no_close = false }
      | Error err -> Lwt.return_error (`Connect err)

    let no_close flow = flow.no_close <- true
    let to_close flow = flow.no_close <- false

    let close flow =
      match flow.no_close with
      | true ->
          Log.debug (fun m -> m "Fakely close the connection.") ;
          Lwt.return_unit
      | false ->
          Log.debug (fun m -> m "Really close the connection.") ;
          close flow.flow

    let shutdown flow = shutdown flow.flow
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

    type nonrec flow = TCP.flow * flow

    let connect (host, cfg, stack, ipaddr, port) =
      Stack.create_connection stack (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow ->
          let open Lwt_result.Infix in
          let tcp_flow = { TCP.flow; TCP.no_close = false } in
          client_of_flow cfg ?host tcp_flow >>= fun tls_flow ->
          Lwt.return_ok (tcp_flow, tls_flow)

    let no_close (tcp_flow, _) = TCP.no_close tcp_flow
    let to_close (tcp_flow, _) = TCP.to_close tcp_flow
    let read (_, tls_flow) = read tls_flow
    let write (_, tls_flow) = write tls_flow
    let writev (_, tls_flow) = writev tls_flow
    let shutdown (_, tls_flow) = shutdown tls_flow
    let epoch (_, tls_flow) = epoch tls_flow

    let reneg ?authenticator ?acceptable_cas ?cert ?drop (_, tls_flow) =
      reneg ?authenticator ?acceptable_cas ?cert ?drop tls_flow

    let key_update ?request (_, tls_flow) = key_update ?request tls_flow

    let server_of_flow config tcp_flow =
      Lwt_result.Infix.(
        server_of_flow config tcp_flow >>= fun tls_flow ->
        Lwt.return_ok (tcp_flow, tls_flow))

    let client_of_flow config ?host tcp_flow =
      Lwt_result.Infix.(
        client_of_flow config ?host tcp_flow >>= fun tls_flow ->
        Lwt.return_ok (tcp_flow, tls_flow))

    let close (tcp_flow, tls_flow) =
      match tcp_flow.TCP.no_close with
      | true -> Lwt.return_unit
      | false -> close tls_flow
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
        Lwt.return_ok { TCP.flow; TCP.no_close = false }
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
      let dst = TCP.dst flow in
      let error_handler = error_handler dst in
      let request_handler' reqd = request_handler flow dst reqd in
      let conn =
        Httpaf.Server_connection.create ?config ~error_handler request_handler'
      in
      Lwt.return_ok
        (R.T flow, Paf.Runtime ((module Httpaf.Server_connection), conn)) in
    Paf.service connection Lwt.return_ok accept close

  let https_service ~tls ?config ~error_handler request_handler =
    let module R = (val Mimic.repr tls_protocol) in
    let handshake tcp_flow =
      let dst = TCP.dst tcp_flow in
      TLS.server_of_flow tls tcp_flow >>= function
      | Ok flow -> Lwt.return_ok (dst, flow)
      | Error `Closed ->
          (* XXX(dinosaure): be care! [`Closed] at this stage does not mean
           * that the bound socket is closed but the socket with the peer is
           * closed. *)
          Log.err (fun m -> m "The connection was closed by peer.") ;
          TCP.close tcp_flow >>= fun () -> Lwt.return_error `Closed
      | Error err ->
          Log.err (fun m -> m "Got a TLS error: %a." TLS.pp_write_error err) ;
          TCP.close tcp_flow >>= fun () -> Lwt.return_error err in
    let connection (dst, flow) =
      let error_handler = error_handler dst in
      let request_handler' reqd = request_handler flow dst reqd in
      let conn =
        Httpaf.Server_connection.create ?config ~error_handler request_handler'
      in
      Lwt.return_ok
        (R.T flow, Paf.Runtime ((module Httpaf.Server_connection), conn)) in
    Paf.service connection handshake accept close

  let alpn =
    let module R = (val Mimic.repr tls_protocol) in
    let alpn_of_tls_connection (_edn, flow) =
      match TLS.epoch flow with
      | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
      | Error _ -> None in
    let peer_of_tls_connection (edn, _flow) = edn in
    (* XXX(dinosaure): [TLS]/[ocaml-tls] should let us to project the underlying
     * [flow] and apply [TCP.dst] on it.
     * Actually, we did it with the [TLS] module. *)
    let injection (_edn, flow) = R.T flow in
    {
      Alpn.alpn = alpn_of_tls_connection;
      Alpn.peer = peer_of_tls_connection;
      Alpn.injection;
    }

  let alpn_service ~tls ?config:(_ = (Httpaf.Config.default, H2.Config.default))
      handler =
    let handshake tcp_flow =
      let dst = TCP.dst tcp_flow in
      TLS.server_of_flow tls tcp_flow >>= function
      | Ok flow -> Lwt.return_ok (dst, flow)
      | Error `Closed ->
          (* XXX(dinosaure): be care! [`Closed] at this stage does not mean
           * that the bound socket is closed but the socket with the peer is
           * closed. *)
          Log.err (fun m -> m "The connection was closed by peer.") ;
          Lwt.return_error (`Write `Closed)
      | Error err ->
          Log.err (fun m -> m "Got a TLS error: %a." TLS.pp_write_error err) ;
          TCP.close tcp_flow >>= fun () ->
          Lwt.return_error (err :> [ TLS.write_error | `Msg of string ]) in
    let module R = (val Mimic.repr tls_protocol) in
    let request flow edn reqd protocol =
      match flow with
      | R.T flow -> handler.Alpn.request flow edn reqd protocol
      | _ -> assert false
      (* XXX(dinosaure): this case should never occur. Indeed, the [injection]
         given to [Alpn.service] only create a [tls_protocol] flow. We just
         destruct it and give it to [request_handler]. *) in
    Alpn.service alpn { handler with request } handshake accept close

  let serve ?stop service t = Paf.serve ?stop service t
end

type transmission = [ `Clear | `TLS of string option ]

let paf_transmission : transmission Mimic.value =
  Mimic.make ~name:"paf-transmission"

let paf_endpoint : (Ipaddr.t * int) Mimic.value =
  Mimic.make ~name:"paf-endpoint"

open Lwt.Infix

let rec kind_of_flow : Mimic.edn list -> transmission option = function
  | Mimic.Edn (k, v) :: r -> (
      match Mimic.equal k paf_transmission with
      | Some Mimic.Refl -> Some v
      | None -> kind_of_flow r)
  | [] -> None

let rec endpoint_of_flow : Mimic.edn list -> (Ipaddr.t * int) option = function
  | Mimic.Edn (k, v) :: r -> (
      match Mimic.equal k paf_endpoint with
      | Some Mimic.Refl -> Some v
      | None -> endpoint_of_flow r)
  | [] -> None

let ( >>? ) = Lwt_result.bind

let run ~ctx handler request =
  Mimic.unfold ctx >>? fun ress ->
  Mimic.connect ress >>= fun res ->
  match (res, kind_of_flow ress) with
  | (Error _ as err), _ -> Lwt.return err
  | Ok flow, (Some `Clear | None) ->
      let edn = endpoint_of_flow ress in
      let alpn = match request with `V1 _ -> "http/1.1" | `V2 _ -> "h2c" in
      Alpn.run ~alpn handler edn request flow
  | Ok flow, Some (`TLS alpn) ->
      let edn = endpoint_of_flow ress in
      Alpn.run ?alpn handler edn request flow
