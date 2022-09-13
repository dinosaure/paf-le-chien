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
  (** [shutdown t] asks to shutdown the connection. *)
end

type 'conn runtime = (module RUNTIME with type t = 'conn)

exception Flow of string
exception Flow_write of string

module Make (Flow : Mirage_flow.S) = struct
  let src = Logs.Src.create "paf-flow"

  module Log = (val Logs.src_log src : Logs.LOG)

  type flow = {
    flow : Flow.flow;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let create flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    Lwt.return { flow; queue; rd_closed = false; wr_closed = false }

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

  type eof = [ `Eof ]

  let recv flow ~report_error ~report_closed ~read ~read_eof =
    Ke.Rke.compress flow.queue ;
    Flow.read flow.flow >>= function
    | (Error _ | Ok #eof) as v ->
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
        (match v with
        | Ok `Eof -> report_closed ()
        | Error err -> report_error err) ;
        Lwt.return `Closed
    | Ok (`Data v) ->
        let len = Cstruct.length v in
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.length ~off:0 ~len v ;
        let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek flow.queue in
        let shift = read slice ~off:0 ~len:(Bigstringaf.length slice) in
        Ke.Rke.N.shift_exn flow.queue shift ;
        Lwt.return `Continue

  let writev ~report_error flow iovecs =
    let iovecs =
      List.map
        (fun { Faraday.buffer; off; len } ->
          Cstruct.copy (Cstruct.of_bigarray buffer ~off ~len) 0 len)
        iovecs in
    let iovecs = List.map Cstruct.of_string iovecs in
    (* XXX(dinosaure): the copy is needed:
       1) [Mirage_flow.S] explicitly says that [write] takes the ownership on
          the given [Cstruct.t]
       2) [ocaml-h2] wants to keep the ownership on given [Faraday.iovec]s

       To protect one from the other, copying is necessary. *)
    Log.debug (fun m ->
        m "Start to write %d byte(s)."
          (List.fold_left (fun acc cs -> Cstruct.length cs + acc) 0 iovecs)) ;
    Flow.writev flow.flow iovecs >>= function
    | Ok () ->
        Lwt.return
          (`Ok
            (List.fold_left (fun acc cs -> acc + Cstruct.length cs) 0 iovecs))
    | Error err ->
        Log.err (fun m ->
            m "Got an errror when we wrote something: %a." Flow.pp_write_error
              err) ;
        report_error err ;
        flow.wr_closed <- true ;
        safely_close flow >>= fun () -> Lwt.return `Closed

  let send ~report_error flow iovecs =
    if flow.wr_closed
    then safely_close flow >>= fun () -> Lwt.return `Closed
    else writev ~report_error flow iovecs

  let close flow =
    match (flow.rd_closed, flow.wr_closed) with
    | true, true -> Lwt.return_unit
    | _ ->
        flow.rd_closed <- true ;
        flow.wr_closed <- true ;
        Flow.close flow.flow
end

module Server (Flow : Mirage_flow.S) (Runtime : RUNTIME) : sig
  val server : Runtime.t -> Flow.flow -> unit Lwt.t
end = struct
  let src = Logs.Src.create "paf-server"

  module Log = (val Logs.src_log src : Logs.LOG)
  module Easy_flow = Make (Flow)
  open Lwt.Infix

  let to_flow_exception err : exn = Flow (Fmt.str "%a" Flow.pp_error err)

  let to_flow_write_exception err : exn =
    Flow_write (Fmt.str "%a" Flow.pp_write_error err)

  let server connection flow =
    Easy_flow.create flow >>= fun flow ->
    let rd_exit, notify_rd_exit = Lwt.wait () in
    let wr_exit, notify_wr_exit = Lwt.wait () in

    let rec rd_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err) in
      let rec go () =
        Log.debug (fun m -> m "Compute next read operation.") ;
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "next read operation: `read") ;
            Easy_flow.recv flow ~report_error ~report_closed:ignore
              ~read:(Runtime.read connection)
              ~read_eof:(Runtime.read_eof connection)
            >>= fun _ -> go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_fiber ;
            Lwt.pause ()
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
      let report_error err =
        Runtime.report_exn connection (to_flow_write_exception err) in
      let rec go () =
        Log.debug (fun m -> m "Compute next write operation.") ;
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write") ;
            Easy_flow.send ~report_error flow iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield") ;
            Runtime.yield_writer connection wr_fiber ;
            Lwt.pause ()
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
  val run : Runtime.t -> Flow.flow -> unit Lwt.t
end = struct
  open Lwt.Infix

  let src = Logs.Src.create "paf"

  module Log = (val Logs.src_log src : Logs.LOG)
  module Easy_flow = Make (Flow)

  let to_flow_exception err : exn = Flow (Fmt.str "%a" Flow.pp_error err)

  let to_flow_write_exception err : exn =
    Flow_write (Fmt.str "%a" Flow.pp_write_error err)

  let run connection flow =
    Easy_flow.create flow >>= fun flow ->
    let rd_exit, notify_rd_exit = Lwt.wait () in
    let wr_exit, notify_wr_exit = Lwt.wait () in

    let rec rd_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err) in
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "next read operation: `read") ;
            Easy_flow.recv flow ~report_error ~report_closed:ignore
              ~read:(Runtime.read connection)
              ~read_eof:(Runtime.read_eof connection)
            >>= fun _ -> go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield") ;
            Runtime.yield_reader connection rd_fiber ;
            Lwt.return_unit
        | `Close ->
            Log.debug (fun m -> m "next read operation: `close.") ;
            Lwt.wakeup_later notify_rd_exit () ;
            flow.Easy_flow.rd_closed <- true ;
            Easy_flow.safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return_unit) in
    let rec wr_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_write_exception err) in
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write.") ;
            Easy_flow.send ~report_error flow iovecs >>= fun res ->
            Runtime.report_write_result connection res ;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield.") ;
            Runtime.yield_writer connection wr_fiber ;
            Lwt.return ()
        | `Close _ ->
            Log.debug (fun m -> m "next write operation: `close.") ;
            Lwt.wakeup_later notify_wr_exit () ;
            flow.Easy_flow.wr_closed <- true ;
            Easy_flow.safely_close flow in
      Lwt.async @@ fun () ->
      Lwt.catch go (fun exn ->
          Runtime.report_exn connection exn ;
          Lwt.return ()) in
    wr_fiber () ;
    rd_fiber () ;
    Lwt.join [ rd_exit; wr_exit ] >>= fun () ->
    Log.debug (fun m -> m "End of transmission.") ;
    Easy_flow.close flow
end

type impl = Runtime : 'conn runtime * 'conn -> impl

type 't service =
  | Service : {
      accept : 't -> ('socket, ([> `Closed ] as 'error)) result Lwt.t;
      handshake : 'socket -> ('flow, ([> `Closed ] as 'error)) result Lwt.t;
      connection : 'flow -> (Mimic.flow * impl, 'error) result Lwt.t;
      close : 't -> unit Lwt.t;
    }
      -> 't service

and ('t, 'socket, 'flow, 'error) posix = {
  accept : 't -> ('socket, 'error) result Lwt.t;
  handshake : 'socket -> ('flow, 'error) result Lwt.t;
  close : 't -> unit Lwt.t;
}
  constraint 'error = [> `Closed ]

let service connection handshake accept close =
  Service { accept; connection; handshake; close }

open Lwt.Infix

let serve_when_ready :
    type t socket flow.
    (t, socket, flow, _) posix ->
    ?stop:Lwt_switch.t ->
    handler:(flow -> unit Lwt.t) ->
    t ->
    [ `Initialized of unit Lwt.t ] =
 fun service ?stop ~handler t ->
  let { accept; handshake; close } = service in
  `Initialized
    (let switched_off =
       let t, u = Lwt.wait () in
       Lwt_switch.add_hook stop (fun () ->
           Lwt.wakeup_later u (Ok `Stopped) ;
           Lwt.return_unit) ;
       t in
     let rec loop () =
       accept t >>= function
       | Ok socket ->
           Lwt.async (fun () ->
               handshake socket >>= function
               | Ok flow -> handler flow
               | Error `Closed ->
                   Logs.info (fun m -> m "Connection closed by peer") ;
                   Lwt.return ()
               | Error _err ->
                   Logs.err (fun m ->
                       m "Got an error from a TCP/IP connection.") ;
                   Lwt.return ()) ;
           loop ()
       | Error `Closed -> Lwt.return_error `Closed
       | Error _ -> Lwt.pause () >>= loop in
     let stop_result =
       Lwt.pick [ switched_off; loop () ] >>= function
       | Ok `Stopped -> close t >>= fun () -> Lwt.return_ok ()
       | Error _ as err -> close t >>= fun () -> Lwt.return err in
     stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)

let server : type t. t runtime -> t -> Mimic.flow -> unit Lwt.t =
 fun (module Runtime) conn flow ->
  let module Server = Server (Mimic) (Runtime) in
  Server.server conn flow

let serve ?stop service t =
  let (Service { accept; handshake; connection; close }) = service in
  let handler flow =
    connection flow >>= function
    | Ok (flow, Runtime (runtime, conn)) -> server runtime conn flow
    | Error _ -> Lwt.return_unit in
  serve_when_ready ?stop ~handler { accept; handshake; close } t

let run : type t. t runtime -> t -> Mimic.flow -> unit Lwt.t =
 fun (module Runtime) conn flow ->
  let module Client = Client (Mimic) (Runtime) in
  Client.run conn flow
