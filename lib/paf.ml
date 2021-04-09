module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  (** [next_read_connection t] returns a value describing the next operation
      that the caller should conduit on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] an returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value an additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection. {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] registers with the connection to call [continue]
      when reading should resume. {!yield_reader} should be called after
      {!next_read_operation} returns a [`Yield] value. *)

  val next_write_operation :
    t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

      - [`Ok n] indicates that the caller successfully wrote [n] bytes of output
        from the buffer that the caller was provided by {!next_write_operation}
        that returns a [`Write buffer] value.
      - [`Closed] indicates that the output destination will no longer accept
        bytes from the write processor. *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call [continue]
      when writing should resume. {!yield_writer} should be called after
      {!next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the tate [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will return a [`Write _] until all
      buffered output has been flushed, at which point it will return [`Close]. *)

  val shutdown : t -> unit
end

type sleep = int64 -> unit Lwt.t

type 'conn runtime = (module RUNTIME with type t = 'conn)

exception Flow of Mimic.error

module Server (Runtime : RUNTIME) : sig
  val server : sleep:sleep -> Runtime.t -> Mimic.flow -> unit Lwt.t
end = struct
  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)

  type server = {
    flow : Mimic.flow;
    sleep : sleep;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

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
              safely_close flow >>= fun () -> raise (Flow err)
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

  let sleep (flow : server) timeout =
    flow.sleep timeout >>= fun () -> Lwt.return (Error `Closed)

  let writev ?(timeout = 5_000_000_000L) flow iovecs =
    let rec go acc = function
      | [] -> Lwt.return (`Ok acc)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Mimic.write flow.flow raw; sleep flow timeout ]
          >>= function
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

  let server ~sleep connection flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let flow = { flow; sleep; queue; rd_closed = false; wr_closed = false } in
    let rd_exit, notify_rd_exit = Lwt.task () in
    let wr_exit, notify_wr_exit = Lwt.task () in
    let rec rd_fiber () =
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "next read operation: `read") ;
            recv flow ~read:(Runtime.read connection)
              ~read_eof:(Runtime.read_eof connection)
            >>= fun _ -> go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_fiber ;
            Lwt.return ()
        | `Close ->
            Log.debug (fun m -> m "next read operation: `close") ;
            Lwt.wakeup_later notify_rd_exit () ;
            flow.rd_closed <- true ;
            safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return ()) in
    let rec wr_fiber () =
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write") ;
            send flow iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield") ;
            Runtime.yield_writer connection wr_fiber ;
            Lwt.return ()
        | `Close _ ->
            Log.debug (fun m -> m "next write operation: `close") ;
            Lwt.wakeup_later notify_wr_exit () ;
            flow.wr_closed <- true ;
            safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_write_result connection `Closed ;
          Runtime.report_exn connection exn ;
          Lwt.return ()) in
    rd_fiber () ;
    wr_fiber () ;
    let threads = [ rd_exit; wr_exit ] in
    Lwt.join threads >>= fun () ->
    Log.debug (fun m -> m "End of transmission.") ;
    close flow
end

module Client (Runtime : RUNTIME) : sig
  val run : sleep:sleep -> Runtime.t -> Mimic.flow -> unit Lwt.t
end = struct
  open Lwt.Infix

  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  type client = {
    flow : Mimic.flow;
    sleep : sleep;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let sleep (flow : client) timeout =
    flow.sleep timeout >>= fun () -> Lwt.return (Error `Closed)

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Mimic.close flow.flow)
    else Lwt.return ()

  let rec really_recv flow ~read ~read_eof =
    Log.debug (fun m -> m "start to really [`read].") ;
    Mimic.read flow.flow >>= function
    | Error err ->
        Log.err (fun m -> m "[`read] got an error: %a." Mimic.pp_error err) ;
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

  let writev ?(timeout = 1_000_000_000L) flow iovecs =
    let rec go acc = function
      | [] -> Lwt.return (`Ok acc)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Mimic.write flow.flow raw; sleep flow timeout ]
          >>= function
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

  let run ~sleep connection flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let flow = { flow; sleep; queue; rd_closed = false; wr_closed = false } in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec rd_loop () =
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read -> (
            Log.debug (fun m -> m "[`read] start to read.") ;
            let read = Runtime.read connection in
            let read_eof = Runtime.read_eof connection in
            recv flow ~read ~read_eof >>= function
            | `Closed ->
                Log.err (fun m -> m "[`read] connection was closed by peer.") ;
                Lwt.wakeup_later notify_read_loop_exited () ;
                flow.rd_closed <- true ;
                safely_close flow
            | _ -> go ())
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_loop ;
            Lwt.return ()
        | `Close ->
            Log.debug (fun m -> m "[`read] close the connection.") ;
            let read = Runtime.read connection in
            let read_eof = Runtime.read_eof connection in
            drain flow ~read ~read_eof >>= fun () ->
            Lwt.wakeup_later notify_read_loop_exited () ;
            flow.rd_closed <- true ;
            safely_close flow in
      Lwt.async (fun () ->
          Lwt.catch go (fun exn ->
              Log.err (fun m ->
                  m "report a read error: %s" (Printexc.to_string exn)) ;
              Lwt.wakeup_later notify_read_loop_exited () ;
              Runtime.report_exn connection exn ;
              Runtime.shutdown connection ;
              Lwt.return ())) in
    let writev = writev flow in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec wr_loop () =
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "[`write] start to write.") ;
            writev iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "[`write] yield.") ;
            Runtime.yield_writer connection wr_loop ;
            Lwt.return ()
        | `Close _ ->
            Log.debug (fun m -> m "[`write] close.") ;
            Lwt.wakeup_later notify_write_loop_exited () ;
            Lwt.return () in

      Lwt.async (fun () ->
          Lwt.catch go (fun exn ->
              Log.err (fun m ->
                  m "report a write error: %s" (Printexc.to_string exn)) ;
              Runtime.report_exn connection exn ;
              Lwt.return ())) in
    wr_loop () ;
    rd_loop () ;
    Lwt.join [ read_loop_exited; write_loop_exited ] >>= fun () ->
    Log.debug (fun m -> m "End of transmission.") ;
    close flow
end

type impl = Runtime : 'conn runtime * 'conn -> impl

type 't service =
  | Service : {
      accept : 't -> ('flow, ([> `Closed ] as 'error)) result Lwt.t;
      connection : 'flow -> (Mimic.flow * impl, 'error) result Lwt.t;
      close : 't -> unit Lwt.t;
    }
      -> 't service

and ('t, 'flow, 'error) posix = {
  accept : 't -> ('flow, 'error) result Lwt.t;
  close : 't -> unit Lwt.t;
}
  constraint 'error = [> `Closed ]

let service connection accept close = Service { accept; connection; close }

open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

let serve_when_ready :
    type t flow.
    (t, flow, _) posix ->
    ?stop:Lwt_switch.t ->
    handler:(flow -> unit Lwt.t) ->
    t ->
    [ `Initialized of unit Lwt.t ] =
 fun service ?stop ~handler t ->
  let { accept; close } = service in
  `Initialized
    (let switched_off =
       let t, u = Lwt.wait () in
       Lwt_switch.add_hook stop (fun () ->
           Lwt.wakeup_later u (Ok `Stopped) ;
           Lwt.return_unit) ;
       t in
     let rec loop () =
       let accept = accept t >>? fun flow -> Lwt.return_ok (`Flow flow) in
       accept >>? function
       | `Flow flow ->
           Lwt.async (fun () -> handler flow) ;
           Lwt.pause () >>= loop in
     let stop_result =
       Lwt.pick [ switched_off; loop () ] >>= function
       | Ok `Stopped -> close t >>= fun () -> Lwt.return_ok ()
       | Error _ as err -> close t >>= fun () -> Lwt.return err in
     stop_result >>= function Ok () | Error (`Closed | _) -> Lwt.return_unit)

let server : type t. t runtime -> sleep:sleep -> t -> Mimic.flow -> unit Lwt.t =
 fun (module Runtime) ~sleep conn flow ->
  let module Server = Server (Runtime) in
  Server.server ~sleep conn flow

let serve ~sleep ?stop service t =
  let (Service { accept; connection; close }) = service in
  let handler flow =
    connection flow >>= function
    | Ok (flow, Runtime (runtime, conn)) -> server runtime ~sleep conn flow
    | Error _ -> Lwt.return_unit in
  serve_when_ready ?stop ~handler { accept; close } t

let run : type t. t runtime -> sleep:sleep -> t -> Mimic.flow -> unit Lwt.t =
 fun (module Runtime) ~sleep conn flow ->
  let module Client = Client (Runtime) in
  Client.run ~sleep conn flow
