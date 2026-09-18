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

let catch ~on fn = try fn () with exn -> on exn

let rec clean orphans =
  match Miou.care orphans with
  | Some prm ->
      Miou.await_exn prm ;
      clean orphans
  | None -> ()

exception Flow of string
exception Flow_write of string

let src = Logs.Src.create "paf-flow"

module Log_flow = (val Logs.src_log src : Logs.LOG)

module Make (Flow : Mimice.Flow.S) = struct
  type flow = {
    flow : Flow.flow;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable rd_closed : bool;
    mutable wr_closed : bool;
  }

  let create flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    { flow; queue; rd_closed = false; wr_closed = false }

  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then (
      Log_flow.debug (fun m -> m "Safely close the connection.") ;
      Flow.close flow.flow)

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let recv flow ~report_error ~report_closed ~read ~read_eof =
    Ke.Rke.compress flow.queue ;
    match Flow.read flow.flow with
    | (Error _ | Ok `End_of_input) as v ->
        flow.rd_closed <- true ;
        safely_close flow ;
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
        | Ok `End_of_input -> report_closed ()
        | Error err -> report_error err) ;
        `Closed
    | Ok (`Data v) ->
        let len = Cstruct.length v in
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.length ~off:0 ~len v ;
        let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek flow.queue in
        let shift = read slice ~off:0 ~len:(Bigstringaf.length slice) in
        Ke.Rke.N.shift_exn flow.queue shift ;
        `Continue

  let writev ~report_error flow iovecs =
    let iovecs =
      List.map
        (fun { Faraday.buffer; off; len } ->
          Cstruct.to_string (Cstruct.of_bigarray buffer ~off ~len) ~off:0 ~len)
        iovecs in
    let iovecs = List.map Cstruct.of_string iovecs in
    (* XXX(dinosaure): the copy is needed:
       1) [Mirage_flow.S] explicitly says that [write] takes the ownership on
          the given [Cstruct.t]
       2) [ocaml-h2] wants to keep the ownership on given [Faraday.iovec]s

       To protect one from the other, copying is necessary. *)
    Log_flow.debug (fun m ->
        m "Start to write %d byte(s)."
          (List.fold_left (fun acc cs -> Cstruct.length cs + acc) 0 iovecs)) ;
    match Flow.writev flow.flow iovecs with
    | Ok () ->
        `Ok (List.fold_left (fun acc cs -> acc + Cstruct.length cs) 0 iovecs)
    | Error err ->
        Log_flow.err (fun m ->
            m "Got an errror when we wrote something: %a." Flow.pp_write_error
              err) ;
        report_error err ;
        flow.wr_closed <- true ;
        safely_close flow ;
        `Closed

  let send ~report_error flow iovecs =
    if flow.wr_closed
    then (
      safely_close flow ;
      `Closed)
    else writev ~report_error flow iovecs

  let close flow =
    match (flow.rd_closed, flow.wr_closed) with
    | true, true -> ()
    | _ ->
        Log_flow.debug (fun m -> m "Close the socket") ;
        flow.rd_closed <- true ;
        flow.wr_closed <- true ;
        Flow.close flow.flow
end

let src = Logs.Src.create "paf-server"

module Log_server = (val Logs.src_log src : Logs.LOG)

module Server (Flow : Mimice.Flow.S) (Runtime : RUNTIME) : sig
  val server : Runtime.t -> Flow.flow -> unit
end = struct
  module Buffered_flow = Make (Flow)

  let to_flow_exception err : exn = Flow (Fmt.str "%a" Flow.pp_error err)

  let to_flow_write_exception err : exn =
    Flow_write (Fmt.str "%a" Flow.pp_write_error err)

  let server connection flow =
    let flow = Buffered_flow.create flow in
    let notify_rd_exit = ref false in
    let notify_wr_exit = ref false in

    let rec rd_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err) in
      let rec go () =
        Log_server.debug (fun m -> m "Compute next read operation.") ;
        match Runtime.next_read_operation connection with
        | `Read ->
            Log_server.debug (fun m -> m "next read operation: `read") ;
            let _ =
              Buffered_flow.recv flow ~report_error ~report_closed:ignore
                ~read:(Runtime.read connection)
                ~read_eof:(Runtime.read_eof connection) in
            go ()
        | `Yield ->
            Log_server.debug (fun m -> m "next read operation: `yield") ;
            (* NOTE(dinosaure): here we trust on [http/af] that every [continuation ()]
               will be consumed by [http/af] in any situations! *)
            let prm = Miou.call_cc rd_fiber in
            let continuation () =
              match Miou.await prm with
              | Ok () -> ()
              | Error exn ->
                  Log_server.err (fun m ->
                      m "Got an exception from the reader side: %S"
                        (Printexc.to_string exn)) in
            Runtime.yield_reader connection continuation ;
            Miou.yield ()
        | `Close ->
            Log_server.debug (fun m -> m "next read operation: `close") ;
            flow.Buffered_flow.rd_closed <- true ;
            Buffered_flow.safely_close flow ;
            notify_rd_exit := true in
      catch ~on:(Runtime.report_exn connection) go in
    let rec wr_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_write_exception err) in
      let rec go () =
        Log_server.debug (fun m -> m "Compute next write operation.") ;
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log_server.debug (fun m -> m "next write operation: `write") ;
            let result = Buffered_flow.send ~report_error flow iovecs in
            Runtime.report_write_result connection result ;
            go ()
        | `Yield ->
            Log_server.debug (fun m -> m "next write operation: `yield") ;
            let prm = Miou.call_cc wr_fiber in
            let continuation () =
              match Miou.await prm with
              | Ok () -> ()
              | Error exn ->
                  Log_server.err (fun m ->
                      m "Got an exception from the writer side: %S"
                        (Printexc.to_string exn)) in
            Runtime.yield_writer connection continuation ;
            Miou.yield ()
        | `Close _ ->
            Log_server.debug (fun m -> m "next write operation: `close") ;
            flow.Buffered_flow.wr_closed <- true ;
            Buffered_flow.safely_close flow ;
            notify_wr_exit := true in
      catch ~on:(Runtime.report_exn connection) go in
    let rd_prm = Miou.call_cc rd_fiber in
    let wr_prm = Miou.call_cc wr_fiber in
    let _ = Miou.await_all [ rd_prm; wr_prm ] in
    let rec go () =
      match (!notify_rd_exit, !notify_rd_exit) with
      | true, true -> ()
      | _ ->
          Miou.yield () ;
          go () in
    go () ;
    Log_server.debug (fun m -> m "End of transmission.") ;
    Buffered_flow.close flow
end

let src = Logs.Src.create "paf-client"

module Log_client = (val Logs.src_log src : Logs.LOG)

module Client (Flow : Mimice.Flow.S) (Runtime : RUNTIME) : sig
  val run :
    Runtime.t ->
    ?give:Miou.Ownership.t list ->
    ?disown:(Flow.flow -> unit) ->
    Flow.flow ->
    unit
end = struct
  module Buffered_flow = Make (Flow)

  let to_flow_exception err : exn = Flow (Fmt.str "%a" Flow.pp_error err)

  let to_flow_write_exception err : exn =
    Flow_write (Fmt.str "%a" Flow.pp_write_error err)

  let run connection ?give ?(disown = Fun.const ()) flow =
    let flow = Buffered_flow.create flow in
    let orphans = Miou.orphans () in
    let notify_rd_exit = ref false in
    let notify_wr_exit = ref false in

    let rec rd_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err) in
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log_client.debug (fun m -> m "next read operation: `read") ;
            let _ =
              Buffered_flow.recv flow ~report_error ~report_closed:ignore
                ~read:(Runtime.read connection)
                ~read_eof:(Runtime.read_eof connection) in
            go ()
        | `Yield ->
            Log_client.debug (fun m -> m "next read operation: `yield") ;
            let continuation () =
              let _ = Miou.call_cc ~orphans ?give rd_fiber in
              () in
            Runtime.yield_reader connection continuation ;
            Miou.yield ()
        | `Close ->
            Log_client.debug (fun m -> m "next read operation: `close.") ;
            flow.Buffered_flow.rd_closed <- true ;
            Buffered_flow.safely_close flow ;
            notify_rd_exit := true in
      Fun.protect ~finally:(fun () -> disown flow.Buffered_flow.flow)
      @@ fun () -> catch ~on:(Runtime.report_exn connection) go in
    let rec wr_fiber () =
      let report_error err =
        Runtime.report_exn connection (to_flow_write_exception err) in
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log_client.debug (fun m -> m "next write operation: `write.") ;
            let result = Buffered_flow.send ~report_error flow iovecs in
            Runtime.report_write_result connection result ;
            go ()
        | `Yield ->
            Log_client.debug (fun m -> m "next write operation: `yield.") ;
            let continuation () =
              let _ = Miou.call_cc ~orphans ?give wr_fiber in
              () in
            Runtime.yield_writer connection continuation ;
            Miou.yield ()
        | `Close _ ->
            Log_client.debug (fun m -> m "next write operation: `close.") ;
            flow.Buffered_flow.wr_closed <- true ;
            Buffered_flow.safely_close flow ;
            notify_wr_exit := true in
      Fun.protect ~finally:(fun () -> disown flow.Buffered_flow.flow)
      @@ fun () -> catch ~on:(Runtime.report_exn connection) go in
    let rd_prm = Miou.call_cc ~orphans ?give rd_fiber in
    let wr_prm = Miou.call_cc ~orphans ?give wr_fiber in
    Log_client.debug (fun m ->
        m "%a is a reader, %a is a writer." Miou.Promise.pp rd_prm
          Miou.Promise.pp wr_prm) ;
    let rec go () =
      clean orphans ;
      match (!notify_rd_exit, !notify_rd_exit) with
      | true, true -> clean orphans
      | _ ->
          Miou.yield () ;
          go () in
    go () ;
    Log_client.debug (fun m -> m "End of transmission.") ;
    let prm = Miou.call_cc ?give @@ fun () -> Buffered_flow.close flow in
    clean orphans ;
    Miou.await_exn prm
end

type impl = Runtime : 'conn runtime * 'conn -> impl

type 't service =
  | Service : {
      accept : 't -> ('socket, ([> `Closed ] as 'error)) result;
      handshake : 'socket -> ('flow, ([> `Closed ] as 'error)) result;
      connection : 'flow -> (Mimice.flow * impl, 'error) result;
      close : 't -> unit;
    }
      -> 't service

and ('t, 'socket, 'flow, 'error) posix = {
  accept : 't -> ('socket, 'error) result;
  handshake : 'socket -> ('flow, 'error) result;
  close : 't -> unit;
}
  constraint 'error = [> `Closed ]

let service connection handshake accept close =
  Service { accept; connection; handshake; close }

let serve_when_ready :
    type t socket flow.
    (t, socket, flow, _) posix ->
    handler:(flow -> unit) ->
    t ->
    [ `Initialized of (unit, [> `Closed ]) result Miou.t ] =
 fun service ~handler t ->
  let { accept; handshake; close = _ } = service in
  let orphans = Miou.orphans () in
  let rec loop () =
    clean orphans ;
    match accept t with
    | Ok socket ->
        let _ =
          Miou.call ~orphans @@ fun () ->
          match handshake socket with
          | Ok flow -> handler flow
          | Error `Closed -> Logs.debug (fun m -> m "Connection closed by peer")
          | Error _err ->
              Logs.err (fun m -> m "Got an error from a TCP/IP connection.")
        in
        loop ()
    | Error `Closed -> Error `Closed
    | Error _ -> loop () in
  `Initialized (Miou.call loop)

let server : type t. t runtime -> t -> Mimice.flow -> unit =
 fun (module Runtime) conn flow ->
  let module Server = Server (Mimice) (Runtime) in
  Server.server conn flow

let serve service t =
  let (Service { accept; handshake; connection; close }) = service in
  let handler flow =
    match connection flow with
    | Ok (flow, Runtime (runtime, conn)) -> server runtime conn flow
    | Error _ -> () in
  serve_when_ready ~handler { accept; handshake; close } t

let run :
    type t.
    t runtime ->
    t ->
    ?give:Miou.Ownership.t list ->
    ?disown:(Mimice.flow -> unit) ->
    Mimice.flow ->
    unit =
 fun (module Runtime) conn ?give ?disown flow ->
  let module Client = Client (Mimice) (Runtime) in
  Client.run conn ?give ?disown flow
