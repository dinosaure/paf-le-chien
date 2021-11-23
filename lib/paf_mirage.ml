module type S = sig
  type stack

  module TCP : sig
    include Mirage_flow.S

    val dst : flow -> Ipaddr.t * int
  end

  module TLS : module type of Tls_mirage.Make (TCP)

  type t

  type dst = Ipaddr.t * int

  val init : port:int -> stack -> t Lwt.t

  val accept : t -> (TCP.flow, [> `Closed ]) result Lwt.t

  val close : t -> unit Lwt.t

  val http_service :
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val https_service :
    tls:Tls.Config.server ->
    ?config:Httpaf.Config.t ->
    error_handler:(dst -> Httpaf.Server_connection.error_handler) ->
    (dst -> Httpaf.Server_connection.request_handler) ->
    t Paf.service

  val serve :
    ?stop:Lwt_switch.t -> 't Paf.service -> 't -> [ `Initialized of unit Lwt.t ]

  val tcp_protocol : (stack * Ipaddr.t * int, TCP.flow) Mimic.protocol

  val tcp_edn : (stack * Ipaddr.t * int) Mimic.value

  val tls_edn :
    ([ `host ] Domain_name.t option
    * Tls.Config.client
    * stack
    * Ipaddr.t
    * int)
    Mimic.value

  val tls_protocol :
    ( [ `host ] Domain_name.t option * Tls.Config.client * stack * Ipaddr.t * int,
      TLS.flow )
    Mimic.protocol

  val run :
    ctx:Mimic.ctx ->
    error_handler:(dst option -> Alpn.client_error -> unit) ->
    response_handler:(dst option -> Alpn.response -> Alpn.body -> unit) ->
    [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ] ->
    (Alpn.body, [> Mimic.error ]) result Lwt.t
end

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) :
  S with type stack = Stack.TCP.t and type TCP.flow = Stack.TCP.flow = struct
  open Lwt.Infix

  type dst = Ipaddr.t * int

  module TCP = struct
    let src = Logs.Src.create "paf-tcp"

    module Log = (val Logs.src_log src : Logs.LOG)

    include Stack.TCP

    type endpoint = Stack.TCP.t * Ipaddr.t * int

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
      Log.debug (fun m ->
          m "Initiate a TCP connection to: %a:%d." Ipaddr.pp ipaddr port) ;
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
      * Stack.TCP.t
      * Ipaddr.t
      * int

    let connect (host, cfg, stack, ipaddr, port) =
      Log.debug (fun m ->
          m "Initiate a TCP connection for TLS to: %a:%d." Ipaddr.pp ipaddr port) ;
      Stack.TCP.create_connection stack (ipaddr, port) >>= function
      | Error err ->
          Log.err (fun m ->
              m
                "Got an error when we try to connect to the server (TLS) to \
                 %a:%d: %a"
                Ipaddr.pp ipaddr port Stack.TCP.pp_error err) ;
          Lwt.return_error (`Read err)
      | Ok flow ->
          Log.debug (fun m ->
              m "Initiate a TLS connection to: %a:%d." Ipaddr.pp ipaddr port) ;
          client_of_flow cfg ?host flow
  end

  let src = Logs.Src.create "paf-layer"

  module Log = (val Logs.src_log src : Logs.LOG)

  type stack = Stack.TCP.t

  let tcp_edn, tcp_protocol = Mimic.register ~name:"tcp" (module TCP)

  let tls_edn, tls_protocol =
    Mimic.register ~priority:10 ~name:"tls" (module TLS)

  type t = {
    stack : Stack.TCP.t;
    queue : Stack.TCP.flow Queue.t;
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
    Stack.TCP.listen ~port stack listener ;
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
      let dst = Stack.TCP.dst flow in
      let error_handler = error_handler dst in
      let request_handler = request_handler dst in
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
          let ((ipaddr, port) as dst) = Stack.TCP.dst flow in
          TLS.server_of_flow tls flow >>= function
          | Ok flow -> Lwt.return_ok (dst, flow)
          | Error `Closed ->
              (* XXX(dinosaure): be care! [`Closed] at this stage does not mean
               * that the bound socket is closed but the socket with the peer is
               * closed. *)
              Lwt.return_error (`Write `Closed)
          | Error err ->
              Log.err (fun m ->
                  m
                    "Got an error when we try to connect to the client (TLS) \
                     to %a:%d: %a"
                    Ipaddr.pp ipaddr port TLS.pp_write_error err) ;
              Stack.TCP.close flow >>= fun () -> Lwt.return_error err) in
    let connection (dst, flow) =
      let error_handler = error_handler dst in
      let request_handler = request_handler dst in
      let conn =
        Httpaf.Server_connection.create ?config ~error_handler request_handler
      in
      Lwt.return_ok
        (R.T flow, Paf.Runtime ((module Httpaf.Server_connection), conn)) in
    Paf.service connection accept close

  let serve ?stop service t = Paf.serve ~sleep:Time.sleep_ns ?stop service t

  let alpn socket =
    match TLS.epoch socket with
    | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
    | _ -> None

  let run ~ctx ~error_handler ~response_handler request =
    let module TCP = (val Mimic.repr tcp_protocol) in
    let module TLS = (val Mimic.repr tls_protocol) in
    Mimic.resolve ctx >>= function
    | Error _ as err -> Lwt.return err
    | Ok (TCP.T socket as flow) ->
        let alpn = match request with `V1 _ -> "http/1.1" | `V2 _ -> "h2c" in
        let edn = Stack.TCP.dst socket in
        Alpn.run ~sleep:Time.sleep_ns ~alpn ~error_handler ~response_handler
          (Some edn) request flow
    | Ok (TLS.T socket as flow) ->
        let alpn = alpn socket in
        Alpn.run ~sleep:Time.sleep_ns ?alpn ~error_handler ~response_handler
          None request flow
    | Ok flow ->
        let alpn = match request with `V1 _ -> "http/1.1" | `V2 _ -> "h2c" in
        Alpn.run ~sleep:Time.sleep_ns ~alpn ~error_handler ~response_handler
          None request flow
end
