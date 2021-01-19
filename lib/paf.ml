module type HTTPAF = sig
  type flow

  exception Error of Mimic.error

  val create_server_connection_handler :
    ?config:Httpaf.Config.t ->
    request_handler:('endpoint -> Httpaf.Server_connection.request_handler) ->
    error_handler:('endpoint -> Httpaf.Server_connection.error_handler) ->
    'endpoint ->
    flow ->
    unit Lwt.t

  val request :
    ?config:Httpaf.Config.t ->
    flow ->
    'endpoint ->
    Httpaf.Request.t ->
    error_handler:(flow -> 'endpoint -> Httpaf.Client_connection.error_handler) ->
    response_handler:('endpoint -> Httpaf.Client_connection.response_handler) ->
    [ `write ] Httpaf.Body.t
end

module Httpaf (Time : Mirage_time.S) : HTTPAF with type flow = Mimic.flow =
struct
  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)

  type server = {
    flow : Mimic.flow;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  and flow = Mimic.flow

  exception Error of Mimic.error

  open Rresult
  open Lwt.Infix

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Mimic.close flow.flow)
    else Lwt.return ()

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let rec recv flow ~read ~read_eof =
    match Ke.Rke.N.peek flow.queue with
    | [] -> (
        if flow.rd_closed
        then
          let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return `Closed
        else
          Mimic.read flow.flow >>= function
          | Error (#Mimic.error as err) ->
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.fail (Error err)
          | Ok `Eof ->
              let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
              Log.debug (fun m -> m "[`read] Connection closed.") ;
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Ok (`Data v) ->
              let len = Cstruct.len v in
              Log.debug (fun m -> m "<- %d byte(s)" len) ;
              let _ =
                Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.len ~off:0 ~len v
              in
              recv flow ~read ~read_eof)
    | src :: _ ->
        let len = Bigstringaf.length src in
        let shift = read src ~off:0 ~len in
        Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
        Ke.Rke.N.shift_exn flow.queue shift ;
        if shift = 0 then Ke.Rke.compress flow.queue ;
        Lwt.return `Continue

  let sleep timeout =
    Time.sleep_ns timeout >>= fun () -> Lwt.return (Error `Closed)

  let writev ?(timeout = 5_000_000_000L) flow iovecs =
    let rec go acc = function
      | [] -> Lwt.return (`Ok acc)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Mimic.write flow.flow raw; sleep timeout ] >>= function
          | Ok () -> go (acc + len) rest
          | Error `Closed ->
              flow.wr_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Error _ -> assert false) in
    go 0 iovecs

  let send flow iovecs =
    if flow.wr_closed
    then safely_close flow >>= fun () -> Lwt.return `Closed
    else writev flow iovecs

  let close flow =
    if ((not flow.rd_closed) && flow.wr_closed)
       || (flow.rd_closed && not flow.wr_closed)
    then (
      flow.rd_closed <- true ;
      flow.wr_closed <- true ;
      Log.debug (fun m -> m "Properly close the connection.") ;
      Mimic.close flow.flow)
    else Lwt.return ()

  let create_server_connection_handler ?(config = Httpaf.Config.default)
      ~request_handler ~error_handler =
    let connection_handler edn flow =
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create ~config ~error_handler:(error_handler edn)
          (request_handler edn) in
      let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
      let flow = { flow; queue; rd_closed = false; wr_closed = false } in
      let rd_exit, notify_rd_exit = Lwt.task () in
      let wr_exit, notify_wr_exit = Lwt.task () in
      let rec rd_fiber () =
        let rec go () =
          match Server_connection.next_read_operation connection with
          | `Read ->
              Log.debug (fun m -> m "next read operation: `read") ;
              recv flow
                ~read:(Server_connection.read connection)
                ~read_eof:(Server_connection.read_eof connection)
              >>= fun _ -> go ()
          | `Yield ->
              Log.debug (fun m -> m "next read operation: `yield") ;
              Server_connection.yield_reader connection rd_fiber ;
              Lwt.return ()
          | `Close ->
              Log.debug (fun m -> m "next read operation: `close") ;
              Lwt.wakeup_later notify_rd_exit () ;
              flow.rd_closed <- true ;
              safely_close flow in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_exn connection exn ;
            Lwt.return ()) in
      let rec wr_fiber () =
        let rec go () =
          match Server_connection.next_write_operation connection with
          | `Write iovecs ->
              Log.debug (fun m -> m "next write operation: `write") ;
              send flow iovecs >>= fun res ->
              Server_connection.report_write_result connection res ;
              go ()
          | `Yield ->
              Log.debug (fun m -> m "next write operation: `yield") ;
              Server_connection.yield_writer connection wr_fiber ;
              Lwt.return ()
          | `Close _ ->
              Log.debug (fun m -> m "next write operation: `close") ;
              Lwt.wakeup_later notify_wr_exit () ;
              flow.wr_closed <- true ;
              safely_close flow in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_write_result connection `Closed ;
            Server_connection.report_exn connection exn ;
            Lwt.return ()) in
      rd_fiber () ;
      wr_fiber () ;
      let threads = [ rd_exit; wr_exit ] in
      Lwt.join threads >>= fun () ->
      Log.debug (fun m -> m "End of transmission.") ;
      close flow in
    connection_handler

  type client = {
    flow : Mimic.flow;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Mimic.close flow.flow)
    else Lwt.return ()

  let rec really_recv flow ~read ~read_eof =
    Mimic.read flow.flow >>= function
    | Error _err ->
        flow.rd_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed
    | Ok `Eof ->
        let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
        Log.debug (fun m -> m "[`read] Connection closed.") ;
        flow.rd_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed
    | Ok (`Data v) ->
        let len = Cstruct.len v in
        Log.debug (fun m -> m "<- %d byte(s)" len) ;
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.len ~off:0 ~len v ;
        recv flow ~read ~read_eof

  and recv flow ~read ~read_eof =
    match Ke.Rke.N.peek flow.queue with
    | [] ->
        if flow.rd_closed
        then
          let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return `Closed
        else really_recv flow ~read ~read_eof
    | src :: _ ->
        let len = Bigstringaf.length src in
        Log.debug (fun m -> m "transmit %d byte(s)" len) ;
        let shift = read src ~off:0 ~len in
        Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
        Ke.Rke.N.shift_exn flow.queue shift ;
        if shift = 0
        then (
          Ke.Rke.compress flow.queue ;
          really_recv flow ~read ~read_eof)
        else Lwt.return `Continue

  let drain flow ~read:_ ~read_eof =
    let go () =
      match Ke.Rke.N.peek flow.queue with
      | [] ->
          Log.debug (fun m -> m "[`drain] empty queue.") ;
          let _ = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return ()
      | src :: _ ->
          Log.debug (fun m -> m "[`drain] %d byte(s)." (Bigstringaf.length src)) ;
          let _ = read_eof src ~off:0 ~len:(Bigstringaf.length src) in
          Lwt.return () in
    Ke.Rke.compress flow.queue ;
    go ()

  let writev ?(timeout = 1000000000L) flow iovecs =
    let rec go acc = function
      | [] -> Lwt.return (`Ok acc)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Mimic.write flow.flow raw; sleep timeout ] >>= function
          | Ok () -> go (acc + len) rest
          | Error `Closed ->
              flow.wr_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Error _ -> assert false) in
    go 0 iovecs

  let close flow =
    if ((not flow.rd_closed) && flow.wr_closed)
       || (flow.rd_closed && not flow.wr_closed)
    then (
      flow.rd_closed <- true ;
      flow.wr_closed <- true ;
      Log.debug (fun m -> m "Properly close the connection.") ;
      Mimic.close flow.flow)
    else Lwt.return ()

  let request ?(config = Httpaf.Config.default) flow edn request ~error_handler
      ~response_handler =
    let module Client_connection = Httpaf.Client_connection in
    let request_body, connection =
      Client_connection.request ~config request
        ~error_handler:(error_handler flow edn)
        ~response_handler:(response_handler edn) in
    let queue = Ke.Rke.create ~capacity:config.read_buffer_size Bigarray.char in
    let flow = { flow; queue; rd_closed = false; wr_closed = false } in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rd_loop () =
      let rec go () =
        match Client_connection.next_read_operation connection with
        | `Read -> (
            Log.debug (fun m -> m "[`read] start to read.") ;
            let read = Client_connection.read connection in
            let read_eof = Client_connection.read_eof connection in
            recv flow ~read ~read_eof >>= function
            | `Closed ->
                Log.err (fun m -> m "[`read] connection was closed by peer.") ;
                Lwt.wakeup_later notify_read_loop_exited () ;
                flow.rd_closed <- true ;
                safely_close flow
            | _ -> go ())
        | `Close ->
            Log.debug (fun m -> m "[`read] close the connection.") ;
            let read = Client_connection.read connection in
            let read_eof = Client_connection.read_eof connection in
            drain flow ~read ~read_eof >>= fun () ->
            Lwt.wakeup_later notify_read_loop_exited () ;
            flow.rd_closed <- true ;
            safely_close flow in
      Lwt.async (fun () ->
          Lwt.catch go (fun exn ->
              Log.err (fun m ->
                  m "report a read error: %s" (Printexc.to_string exn)) ;
              Lwt.wakeup_later notify_read_loop_exited () ;
              Client_connection.report_exn connection exn ;
              Client_connection.shutdown connection ;
              Lwt.return ())) in
    let writev = writev flow in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec wr_loop () =
      let rec go () =
        match Client_connection.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "[`write] start to write.") ;
            writev iovecs >>= fun res ->
            Client_connection.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "[`write] yield.") ;
            Client_connection.yield_writer connection wr_loop ;
            Lwt.return ()
        | `Close _ ->
            Log.debug (fun m -> m "[`write] close.") ;
            Lwt.wakeup_later notify_write_loop_exited () ;
            Lwt.return () in

      Lwt.async (fun () ->
          Lwt.catch go (fun exn ->
              Log.err (fun m ->
                  m "report a write error: %s" (Printexc.to_string exn)) ;
              Client_connection.report_exn connection exn ;
              Lwt.return ())) in
    wr_loop () ;
    rd_loop () ;
    Lwt.async (fun () ->
        Lwt.join [ read_loop_exited; write_loop_exited ] >>= fun () ->
        Log.debug (fun m -> m "End of transmission.") ;
        close flow) ;
    request_body
end

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  open Lwt.Infix

  module TCP = struct
    let src = Logs.Src.create "paf-tls"

    module Log = (val Logs.src_log src : Logs.LOG)

    include Stack.TCP

    type endpoint = Stack.t * Ipaddr.t * int

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
      let t = Stack.tcp stack in
      Log.debug (fun m ->
          m "Initiate a TCP connection to: %a:%d." Ipaddr.pp ipaddr port) ;
      create_connection t (ipaddr, port) >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Connect err)
  end

  module TLS = struct
    let src = Logs.Src.create "paf-tls"

    module Log = (val Logs.src_log src : Logs.LOG)

    include Tls_mirage.Make (Stack.TCP)

    type endpoint =
      [ `host ] Domain_name.t option
      * Tls.Config.client
      * Stack.t
      * Ipaddr.t
      * int

    let connect (domain_name, cfg, stack, ipaddr, port) =
      let t = Stack.tcp stack in
      Log.debug (fun m ->
          m "Initiate a TCP connection for TLS to: %a:%d." Ipaddr.pp ipaddr port) ;
      Stack.TCP.create_connection t (ipaddr, port) >>= function
      | Error err -> Lwt.return_error (`Read err)
      | Ok flow ->
          Log.debug (fun m ->
              m "Initiate a TLS connection to: %a:%d." Ipaddr.pp ipaddr port) ;
          client_of_flow cfg
            ?host:(Option.map Domain_name.to_string domain_name)
            flow
  end

  let tcp_edn, tcp_protocol = Mimic.register ~name:"tcp" (module TCP)

  let tls_edn, tls_protocol =
    Mimic.register ~priority:10 ~name:"tls" (module TLS)

  module Httpaf = Httpaf (Time)
  include Httpaf

  type service = {
    stack : Stack.t;
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
    Stack.listen_tcp ~port stack listener ;
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

  let close ({ stack; mutex; _ } as t) =
    Lwt_mutex.with_lock mutex @@ fun () ->
    Stack.disconnect stack >>= fun () ->
    t.closed <- true ;
    Lwt.return_unit

  let ( >>? ) = Lwt_result.bind

  let serve_when_ready ?timeout ?stop ~handler service =
    let timeout () =
      match timeout with
      | None -> Lwt.wait () |> fst
      | Some t -> Time.sleep_ns t in
    `Initialized
      (let switched_off =
         let t, u = Lwt.wait () in
         Lwt_switch.add_hook stop (fun () ->
             Lwt.wakeup_later u (Ok `Stopped) ;
             Lwt.return_unit) ;
         t in
       let rec loop () =
         let accept =
           accept service >>? fun flow -> Lwt.return_ok (`Flow flow) in
         Lwt.pick [ accept; (timeout () >|= fun () -> Ok `Timeout) ]
         >>? function
         | `Flow flow ->
             Lwt.async (fun () -> handler flow) ;
             Lwt.pause () >>= loop
         | `Timeout -> Lwt.return_ok `Timeout in
       let stop_result =
         Lwt.pick [ switched_off; loop () ] >>= function
         | Ok (`Timeout | `Stopped) ->
             close service >>= fun () -> Lwt.return_ok ()
         | Error _ as err -> close service >>= fun () -> Lwt.return err in
       stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)

  module Tcp = (val Mimic.repr tcp_protocol)

  module Tls = (val Mimic.repr tls_protocol)

  let http ?config ?stop ~error_handler ~request_handler service =
    let handler socket =
      let ipaddr, port = Stack.TCP.dst socket in
      create_server_connection_handler ?config ~error_handler ~request_handler
        (ipaddr, port) (Tcp.T socket) in
    serve_when_ready ?stop ~handler service

  let https ~tls ?config ?stop ~error_handler ~request_handler service =
    let handler socket =
      let ipaddr, port = Stack.TCP.dst socket in
      TLS.server_of_flow tls socket >>= function
      | Ok state ->
          create_server_connection_handler ?config ~error_handler
            ~request_handler (ipaddr, port) (Tls.T state)
      | Error _ -> Lwt.return_unit in
    serve_when_ready ?stop ~handler service

  let request ?config ~ctx ~error_handler ~response_handler v =
    Mimic.resolve ctx >>= function
    | Error _ as err -> Lwt.return err
    | Ok (Tcp.T socket as flow) ->
        Logs.debug (fun m -> m "Connected to the peer.") ;
        let ipaddr, port = Stack.TCP.dst socket in
        Lwt.return_ok
          (request ?config flow
             (Some (ipaddr, port))
             v ~error_handler ~response_handler)
    | Ok flow ->
        Lwt.return_ok
          (request ?config flow None v ~error_handler ~response_handler)
end
