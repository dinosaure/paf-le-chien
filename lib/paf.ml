module Conduit_mirage_tls = Conduit_tls.Make (Lwt) (Conduit_mirage)

module type HTTPAF = sig
  type flow

  exception Send_error of string

  exception Recv_error of string

  exception Close_error of string

  val create_server_connection_handler :
    ?config:Httpaf.Config.t ->
    request_handler:('endpoint -> Httpaf.Server_connection.request_handler) ->
    error_handler:('endpoint -> Httpaf.Server_connection.error_handler) ->
    'endpoint ->
    flow ->
    unit Lwt.t

  val request :
    ?tls:bool ->
    ?config:Httpaf.Config.t ->
    flow ->
    'endpoint ->
    Httpaf.Request.t ->
    error_handler:(flow -> 'endpoint -> Httpaf.Client_connection.error_handler) ->
    response_handler:('endpoint -> Httpaf.Client_connection.response_handler) ->
    [ `write ] Httpaf.Body.t
end

module Httpaf
    (Conduit : Conduit.S
                 with type 'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (Time : Mirage_time.S) : HTTPAF with type flow = Conduit.flow = struct
  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)

  module We = Ke.Rke.Weighted

  type server = {
    flow : Conduit.flow;
    tmp : Cstruct.t;
    queue : (char, Bigarray.int8_unsigned_elt) We.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  and flow = Conduit.flow

  exception Recv_error of string

  exception Send_error of string

  exception Close_error of string

  open Lwt.Infix

  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Conduit.close flow.flow >>= function
      | Error `Not_found -> assert false
      | Error (`Msg err) ->
          Log.err (fun m -> m "Got an error when closing: %s" err) ;
          Lwt.fail (Close_error err)
      | Ok () -> Lwt.return ())
    else Lwt.return ()

  let rec recv flow ~read ~read_eof =
    match We.N.peek flow.queue with
    | [] -> (
        if flow.rd_closed
        then
          let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return `Closed
        else
          let len = min (We.available flow.queue) (Cstruct.len flow.tmp) in
          let raw = Cstruct.sub flow.tmp 0 len in
          Conduit.recv flow.flow raw >>= function
          | Error (`Msg err) ->
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.fail (Recv_error err)
          | Error `Not_found -> assert false
          | Ok `End_of_flow ->
              let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
              Log.debug (fun m -> m "[`read] Connection closed.") ;
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Ok (`Input len) ->
              Log.debug (fun m -> m "<- %d byte(s)" len) ;
              let _ =
                We.N.push_exn flow.queue ~blit ~length:Cstruct.len ~off:0 ~len
                  raw in
              recv flow ~read ~read_eof)
    | src :: _ ->
        let len = Bigstringaf.length src in
        let shift = read src ~off:0 ~len in
        Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
        We.N.shift_exn flow.queue shift ;
        if shift = 0 then We.compress flow.queue ;
        Lwt.return `Continue

  let writev ?(timeout = 5_000_000_000L) flow iovecs =
    let sleep () =
      Time.sleep_ns timeout >>= fun () -> Lwt.return (Error `Timeout) in

    let rec go n = function
      | [] -> Lwt.return (`Ok n)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Conduit.send flow.flow raw; sleep () ] >>= function
          | Ok shift ->
              if shift = len
              then go (n + shift) rest
              else
                go (n + shift)
                  ({ Faraday.buffer; off = off + shift; len = len - shift }
                  :: rest)
          | Error `Timeout ->
              flow.wr_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Error (`Msg err) ->
              Log.err (fun m -> m "Got an error while writing: %s." err) ;
              flow.wr_closed <- true ;
              safely_close flow >>= fun () -> Lwt.fail (Send_error err)
          | Error `Not_found -> assert false) in
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
      Conduit.close flow.flow >>= function
      | Ok () ->
          Log.debug (fun m -> m "Connection closed.") ;
          Lwt.return ()
      | Error `Not_found -> assert false
      | Error (`Msg err) ->
          Log.err (fun m -> m "Got an error when closing: %s." err) ;
          Lwt.fail (Close_error err))
    else Lwt.return ()

  let create_server_connection_handler ?(config = Httpaf.Config.default)
      ~request_handler ~error_handler =
    let connection_handler edn flow =
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create ~config ~error_handler:(error_handler edn)
          (request_handler edn) in
      let queue, _ = We.create ~capacity:0x1000 Bigarray.char in
      let flow =
        {
          flow;
          tmp = Cstruct.create config.read_buffer_size;
          queue;
          rd_closed = false;
          wr_closed = false;
        } in
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

  module Qe = Ke.Rke

  type client = {
    flow : Conduit.flow;
    tmp : Cstruct.t;
    queue : (char, Bigarray.int8_unsigned_elt) Qe.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Conduit.close flow.flow >>= function
      | Error `Not_found -> assert false
      | Error (`Msg err) ->
          Log.err (fun m -> m "Got an error when closing: %s" err) ;
          Lwt.fail (Close_error err)
      | Ok () -> Lwt.return ())
    else Lwt.return ()

  let no_lock f = f ()

  let with_lock : ?mutex:Lwt_mutex.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t =
   fun ?mutex ->
    match mutex with
    | Some mutex -> fun f -> Lwt_mutex.with_lock mutex f
    | None -> no_lock

  let rec really_recv ?tls flow ~read ~read_eof =
    let len = Cstruct.len flow.tmp in
    let raw = Cstruct.sub flow.tmp 0 len in
    with_lock ?mutex:tls (fun () ->
        Lwt.catch
          (fun () -> Conduit.recv flow.flow raw)
          (fun exn -> Lwt.return_error (`Exn exn)))
    >>= function
    | Error `Not_found -> assert false
    | Error (`Exn _) ->
        flow.rd_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed
    | Error (`Msg _) ->
        flow.rd_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed
        (* Lwt.fail (Recv_error err) *)
    | Ok `End_of_flow ->
        let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
        Log.debug (fun m -> m "[`read] Connection closed.") ;
        flow.rd_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed
    | Ok (`Input len) ->
        Log.debug (fun m -> m "<- %d byte(s)" len) ;
        Qe.N.push flow.queue ~blit ~length:Cstruct.len ~off:0 ~len raw ;
        recv ?tls flow ~read ~read_eof

  and recv ?tls flow ~read ~read_eof =
    match Qe.N.peek flow.queue with
    | [] ->
        if flow.rd_closed
        then
          let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return `Closed
        else
          really_recv ?tls flow ~read ~read_eof
          (*
        let len = (Cstruct.len flow.tmp) in
        let raw = Cstruct.sub flow.tmp 0 len in
        ( with_lock ?mutex:tls (fun () ->
              Lwt.catch (fun () -> Conduit.recv flow.flow raw) 
              (fun exn -> Lwt.return_error (`Exn exn))) >>= function
            | Error `Not_found -> assert false
            | Error (`Exn _) ->
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
            | Error (`Msg _) ->
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
              (* Lwt.fail (Recv_error err) *)
            | Ok `End_of_flow ->
              let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
              Log.debug (fun m -> m "[`read] Connection closed.") ;
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
            | Ok (`Input len) ->
              Log.debug (fun m -> m "<- %d byte(s)" len) ;
              Qe.N.push flow.queue ~blit ~length:Cstruct.len ~off:0 ~len raw ;
              recv ?tls flow ~read ~read_eof ) *)
    | src :: _ ->
        let len = Bigstringaf.length src in
        Log.debug (fun m -> m "transmit %d byte(s)" len) ;
        let shift = read src ~off:0 ~len in
        Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
        Qe.N.shift_exn flow.queue shift ;
        if shift = 0
        then (
          Qe.compress flow.queue ;
          really_recv ?tls flow ~read ~read_eof)
        else Lwt.return `Continue

  let drain ?tls:_ flow ~read:_ ~read_eof =
    let go () =
      match Qe.N.peek flow.queue with
      | [] ->
          Log.debug (fun m -> m "[`drain] empty queue.") ;
          let _ = read_eof Bigstringaf.empty ~off:0 ~len:0 in
          Lwt.return ()
      | src :: _ ->
          Log.debug (fun m -> m "[`drain] %d byte(s)." (Bigstringaf.length src)) ;
          let _ = read_eof src ~off:0 ~len:(Bigstringaf.length src) in
          Lwt.return () in
    Qe.compress flow.queue ;
    go ()

  let writev ?tls ?(timeout = 1000000000L) flow iovecs =
    let sleep () =
      Time.sleep_ns timeout >>= fun () -> Lwt.return (Error `Timeout) in

    let rec go n = function
      | [] -> Lwt.return (`Ok n)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick
            [
              with_lock ?mutex:tls (fun () -> Conduit.send flow.flow raw);
              sleep ();
            ]
          >>= function
          | Ok shift ->
              if shift = len
              then go (n + shift) rest
              else
                go (n + shift)
                  ({ Faraday.buffer; off = off + shift; len = len - shift }
                  :: rest)
          | Error `Not_found -> assert false
          | Error `Timeout ->
              flow.wr_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
          | Error (`Msg err) ->
              Log.err (fun m -> m "Got an error while reading: %s." err) ;
              Lwt.fail (Send_error err)) in
    go 0 iovecs

  let close ?tls flow =
    if ((not flow.rd_closed) && flow.wr_closed)
       || (flow.rd_closed && not flow.wr_closed)
    then (
      flow.rd_closed <- true ;
      flow.wr_closed <- true ;
      Log.debug (fun m -> m "Properly close the connection.") ;
      with_lock ?mutex:tls (fun () -> Conduit.close flow.flow) >>= function
      | Ok () ->
          Log.debug (fun m -> m "Connection properly closed.") ;
          Lwt.return ()
      | Error `Not_found -> assert false
      | Error (`Msg err) -> Lwt.fail (Close_error err))
    else Lwt.return ()

  let request ?(tls = false) ?(config = Httpaf.Config.default) flow edn request
      ~error_handler ~response_handler =
    let module Client_connection = Httpaf.Client_connection in
    let request_body, connection =
      Client_connection.request ~config request
        ~error_handler:(error_handler flow edn)
        ~response_handler:(response_handler edn) in
    let tls = if tls then Some (Lwt_mutex.create ()) else None in
    let queue = Qe.create ~capacity:config.read_buffer_size Bigarray.char in
    let tmp = Cstruct.create config.read_buffer_size in
    let flow = { flow; queue; tmp; rd_closed = false; wr_closed = false } in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rd_loop () =
      let rec go () =
        match Client_connection.next_read_operation connection with
        | `Read -> (
            Log.debug (fun m -> m "[`read] start to read.") ;
            let read = Client_connection.read connection in
            let read_eof = Client_connection.read_eof connection in
            recv ?tls flow ~read ~read_eof >>= function
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
            drain ?tls flow ~read ~read_eof >>= fun () ->
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
    let writev ?tls = writev ?tls flow in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec wr_loop () =
      let rec go () =
        match Client_connection.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "[`write] start to write.") ;
            writev ?tls iovecs >>= fun res ->
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
    (* XXX(dinosaure): we trust on [write] will be the first call. *)
    wr_loop () ;
    rd_loop () ;
    Lwt.async (fun () ->
        Lwt.join [ read_loop_exited; write_loop_exited ] >>= fun () ->
        Log.debug (fun m -> m "End of transmission.") ;
        close ?tls flow) ;
    request_body
end

module Make (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) = struct
  open Lwt.Infix
  module TCP = Conduit_mirage_tcp.Make (StackV4)
  module Httpaf = Httpaf (Conduit_mirage) (Time)

  let tls_protocol = Conduit_mirage_tls.protocol_with_tls TCP.protocol

  let tls_service = Conduit_mirage_tls.service_with_tls TCP.service tls_protocol

  include Httpaf

  let http ?config ~error_handler ~request_handler master =
    let module Service = (val Conduit_mirage.Service.impl TCP.service) in
    let handler edn flow =
      create_server_connection_handler ?config ~error_handler ~request_handler
        edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
          let edn = TCP.dst flow in
          Lwt.async (fun () ->
              handler edn (Conduit_mirage.pack TCP.protocol flow)) ;
          Lwt.pause () >>= go in
    go ()

  let https ?config ~error_handler ~request_handler master =
    let open Rresult in
    let module Service = (val Conduit_mirage.Service.impl tls_service) in
    let handler edn flow =
      create_server_connection_handler ?config ~error_handler ~request_handler
        edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
          let edn = TCP.dst (Conduit_mirage_tls.underlying flow) in
          Lwt.async (fun () ->
              handler edn (Conduit_mirage.pack tls_protocol flow)) ;
          Lwt.pause () >>= go in
    go ()

  let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

  let request ?config ~resolvers ~error_handler ~response_handler domain_name v
      =
    Conduit_mirage.resolve resolvers domain_name >>= function
    | Error _ as err -> Lwt.return err
    | Ok flow ->
    match
      ( Conduit_mirage.cast flow tls_protocol,
        Conduit_mirage.cast flow TCP.protocol )
    with
    | Some tls, None ->
        let ip, port = TCP.dst (Conduit_mirage_tls.underlying tls) in
        Lwt.return_ok
          (request ~tls:true ?config flow
             (Some (ip, port))
             v ~error_handler ~response_handler)
    | None, Some protocol ->
        let ip, port = TCP.dst protocol in
        Lwt.return_ok
          (request ?config flow
             (Some (ip, port))
             v ~error_handler ~response_handler)
    | Some _, Some _ -> assert false (* tls_protocol <> TCP.protocol *)
    | None, None -> failwith "Unrecognized conduit's protocol"
end
