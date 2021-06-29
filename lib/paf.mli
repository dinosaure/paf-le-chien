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

type 'conn runtime = (module RUNTIME with type t = 'conn)

type impl = Runtime : 'conn runtime * 'conn -> impl

type sleep = int64 -> unit Lwt.t

val server : 'conn runtime -> sleep:sleep -> 'conn -> Mimic.flow -> unit Lwt.t

val run : 'conn runtime -> sleep:sleep -> 'conn -> Mimic.flow -> unit Lwt.t

type 't service

val service :
  ('flow -> (Mimic.flow * impl, 'error) result Lwt.t) ->
  ('t -> ('flow, ([> `Closed ] as 'error)) result Lwt.t) ->
  ('t -> unit Lwt.t) ->
  't service

val serve :
  sleep:sleep ->
  ?stop:Lwt_switch.t ->
  't service ->
  't ->
  [ `Initialized of unit Lwt.t ]
