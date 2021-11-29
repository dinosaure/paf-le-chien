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

module Make (Flow : Mirage_flow.S) = struct
  let src = Logs.Src.create "paf-flow"

  module Log = (val Logs.src_log src : Logs.LOG)

  type flow = {
    flow : Flow.flow;
    sleep : sleep;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let create ~sleep flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    Lwt.return { flow; sleep; queue; rd_closed = false; wr_closed = false }

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log.debug (fun m -> m "Close the connection.") ;
      Flow.close flow.flow)
    else Lwt.return ()

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  open Lwt.Infix

  let recv flow ~read ~read_eof =
    (* match Ke.Rke.N.peek flow.queue with
       | src :: _ ->
         let len = Bigstringaf.length src in
         let shift = read src ~off:0 ~len in
         Ke.Rke.N.shift_exn flow.queue shift ;
         Lwt.return `Continue
       | [] when flow.rd_closed ->
         let _ = read_eof Bigstringaf.empty ~off:0 ~len:0 in
         Lwt.return `Closed
       | [] -> *)
    Ke.Rke.compress flow.queue ;
    Flow.read flow.flow >>= function
    | Error _ | Ok `Eof ->
        flow.rd_closed <- true ;
        safely_close flow >>= fun () ->
        let _shift =
          match
            Ke.Rke.compress flow.queue ;
            Ke.Rke.N.peek flow.queue
          with
          | [] -> read_eof Bigstringaf.empty ~off:0 ~len:0
          | [ slice ] -> read_eof slice ~off:0 ~len:(Bigstringaf.length slice)
          | _ -> assert false
          (* XXX(dinosaure): impossible due to [compress]. *) in
        Lwt.return `Closed
    | Ok (`Data v) ->
        let len = Cstruct.length v in
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.length ~off:0 ~len v ;
        let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek flow.queue in
        let shift = read slice ~off:0 ~len:(Bigstringaf.length slice) in
        Ke.Rke.N.shift_exn flow.queue shift ;
        Lwt.return `Continue
  (* XXX(dinosaure): semantically, this is the closer impl. of [recv] if we
   * compare with HTTP/AF. [compress] is called before any [read] and it ensures
   * some assumptions needed by HTTP/AF (or Angstrom) to parse requests.
   *
   * Indeed, without [compress] at the beginning, it seems that HTTP/AF is not
   * able to decide to close the connection.
   *
   * On the other side, introspect [flow.queue] before and gives slices and limit
   * calls to [read] can finish to a situation with ["\r\n"] into the queue and
   * HTTP/AF is not able to shift nor to finalize.
   *
   * In others words, [compress] seems the key to ensure that we deliver something
   * good for HTTP/AF to terminate or not the connection properly. *)

  let sleep flow timeout =
    flow.sleep timeout >>= fun () -> Lwt.return (Error `Closed)

  let writev ?(timeout = 5_000_000_000L) flow iovecs =
    let rec go acc = function
      | [] -> Lwt.return (`Ok acc)
      | { Faraday.buffer; off; len } :: rest -> (
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Lwt.pick [ Flow.write flow.flow raw; sleep flow timeout ] >>= function
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
    match (flow.rd_closed, flow.wr_closed) with
    | true, true -> Lwt.return_unit
    | _ ->
        flow.rd_closed <- true ;
        flow.wr_closed <- true ;
        Flow.close flow.flow
end

module Server (Flow : Mirage_flow.S) (Runtime : RUNTIME) : sig
  val server : sleep:sleep -> Runtime.t -> Flow.flow -> unit Lwt.t
end = struct
  let src = Logs.Src.create "paf-server"

  module Log = (val Logs.src_log src : Logs.LOG)

  module Easy_flow = Make (Flow)
  open Lwt.Infix

  let server ~sleep connection flow =
    Easy_flow.create ~sleep flow >>= fun flow ->
    let rd_exit, notify_rd_exit = Lwt.wait () in
    let wr_exit, notify_wr_exit = Lwt.wait () in
    let rec rd_fiber () =
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "next read operation: `read") ;
            Easy_flow.recv flow ~read:(Runtime.read connection)
              ~read_eof:(Runtime.read_eof connection)
            >>= fun _ -> go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_fiber ;
            Lwt.return_unit
        | `Close ->
            Log.debug (fun m -> m "next read operation: `close") ;
            Lwt.wakeup_later notify_rd_exit () ;
            flow.Easy_flow.rd_closed <- true ;
            Easy_flow.safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return_unit) in
    let rec wr_fiber () =
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write") ;
            Easy_flow.send flow iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield") ;
            Runtime.yield_writer connection wr_fiber ;
            Lwt.return_unit
        | `Close _ ->
            Log.debug (fun m -> m "next write operation: `close") ;
            Lwt.wakeup_later notify_wr_exit () ;
            flow.Easy_flow.wr_closed <- true ;
            Easy_flow.safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          (* Runtime.report_write_result connection `Closed ; *)
          Runtime.report_exn connection exn ;
          Lwt.return_unit) in
    rd_fiber () ;
    wr_fiber () ;
    Lwt.join [ rd_exit; wr_exit ] >>= fun () ->
    Log.debug (fun m -> m "End of transmission.") ;
    Easy_flow.close flow
end

module Client (Flow : Mirage_flow.S) (Runtime : RUNTIME) : sig
  val run : sleep:sleep -> Runtime.t -> Flow.flow -> unit Lwt.t
end = struct
  open Lwt.Infix

  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)

  module Easy_flow = Make (Flow)

  let run ~sleep connection flow =
    Easy_flow.create ~sleep flow >>= fun flow ->
    let rd_exit, notify_rd_exit = Lwt.wait () in
    let wr_exit, notify_wr_exit = Lwt.wait () in

    let rec rd_loop () =
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "[`read] start to read.") ;
            Easy_flow.recv flow ~read:(Runtime.read connection)
              ~read_eof:(Runtime.read_eof connection)
            >>= fun _ -> go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_loop ;
            Lwt.return_unit
        | `Close ->
            Log.debug (fun m -> m "[`read] close the connection.") ;
            Lwt.wakeup_later notify_rd_exit () ;
            flow.Easy_flow.rd_closed <- true ;
            Easy_flow.safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return_unit) in
    let rec wr_loop () =
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "[`write] start to write.") ;
            Easy_flow.send flow iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "[`write] yield.") ;
            Runtime.yield_writer connection wr_loop ;
            Lwt.return ()
        | `Close _ ->
            Log.debug (fun m -> m "[`write] close.") ;
            Lwt.wakeup_later notify_wr_exit () ;
            Lwt.return_unit in

      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return ()) in
    wr_loop () ;
    rd_loop () ;
    Lwt.join [ rd_exit; wr_exit ] >>= fun () -> Easy_flow.close flow
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

open Lwt.Infix

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
       accept t >>= function
       | Ok flow ->
           Lwt.async (fun () -> handler flow) ;
           Lwt.pause () >>= loop
       | Error `Closed -> Lwt.return_error `Closed
       | Error _ -> Lwt.pause () >>= loop in
     let stop_result =
       Lwt.pick [ switched_off; loop () ] >>= function
       | Ok `Stopped -> close t >>= fun () -> Lwt.return_ok ()
       | Error _ as err -> close t >>= fun () -> Lwt.return err in
     stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)

let server : type t. t runtime -> sleep:sleep -> t -> Mimic.flow -> unit Lwt.t =
 fun (module Runtime) ~sleep conn flow ->
  let module Server = Server (Mimic) (Runtime) in
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
  let module Client = Client (Mimic) (Runtime) in
  Client.run ~sleep conn flow
