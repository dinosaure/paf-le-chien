module Ke = Ke.Rke.Weighted

let ( <.> ) f g = fun x -> f (g x)

module type HTTPAF = sig
  type endpoint = Ipaddr.V4.t * int
  type flow

  val create_connection_handler
    :  ?config:Httpaf.Config.t
    -> request_handler:(endpoint -> Httpaf.Server_connection.request_handler)
    -> error_handler:(endpoint -> Httpaf.Server_connection.error_handler)
    -> endpoint
    -> flow
    -> unit Lwt.t
end

module Httpaf
    (Tuyau : Tuyau.S with type 'a s = 'a Lwt.t
                      and type input = Cstruct.t
                      and type output = Cstruct.t)
    (Time : Mirage_time.S) : HTTPAF with type flow = Tuyau.flow = struct
  let src = Logs.Src.create "paf"
  module Log = (val Logs.src_log src : Logs.LOG)
  
  type t =
    { flow  : Tuyau.flow
    ; tmp   : Cstruct.t
    ; queue : (char, Bigarray.int8_unsigned_elt) Ke.t
    ; mutable rd_closed : bool
    ; mutable wr_closed : bool }
  and flow = Tuyau.flow
  and endpoint = Ipaddr.V4.t * int
  
  exception Recv_error  of string
  exception Send_error  of string
  exception Close_error of string
  
  open Lwt.Infix
  
  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let concat lst =
    let len = List.fold_left (fun a x -> a + Bigstringaf.length x) 0 lst in
    let res = Bigstringaf.create len in
    let rec transmit off = function
      | [] -> ()
      | src :: rest ->
        let len = Bigstringaf.length src in
        Bigstringaf.blit src ~src_off:0 res ~dst_off:off ~len ;
        transmit (off + len) rest in
    transmit 0 lst ; res
  
  let safely_close flow =
    if flow.rd_closed && flow.wr_closed
    then
      ( Log.debug (fun m -> m "close the connection.\n%!")
      ; Tuyau.close flow.flow >>= function
      | Error (`Msg err) -> Lwt.fail (Close_error err)
      | Ok () -> Lwt.return () )
    else Lwt.return ()
  
  let rec recv flow ~read ~read_eof =
    match Ke.N.peek flow.queue with
    | [] ->
      if flow.rd_closed
      then ( let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in Lwt.return `Closed )
      else
        let len = min (Ke.available flow.queue) (Cstruct.len flow.tmp) in
        let raw = Cstruct.sub flow.tmp 0 len in
        ( Tuyau.recv flow.flow raw >>= function
            | Error (`Msg err) -> Lwt.fail (Recv_error err)
            | Ok `End_of_input ->
              let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
              Log.debug (fun m -> m "[`read] Connection closed.") ;
              flow.rd_closed <- true ;
              safely_close flow >>= fun () -> Lwt.return `Closed
            | Ok (`Input len) ->
              Log.debug (fun m -> m "<- %S" (Cstruct.to_string (Cstruct.sub raw 0 len))) ;
              let _ = Ke.N.push_exn flow.queue ~blit ~length:Cstruct.len ~off:0 ~len raw in
              recv flow ~read ~read_eof )
    | src :: _ ->
      let len = Bigstringaf.length src in
      Log.debug (fun m -> m "[`read] wants to transmit %S" (Bigstringaf.to_string src)) ;
      let shift = read src ~off:0 ~len in
      Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
      Ke.N.shift_exn flow.queue shift ;
      if shift = 0 then Ke.compress flow.queue ;
      Lwt.return `Continue
  
  let writev ?(timeout= 1000000000L) flow iovecs =
    let sleep () = Time.sleep_ns timeout >>= fun () -> Lwt.return (Error `Timeout) in
  
    let rec go n = function
      | [] -> Lwt.return (`Ok n)
      | { Faraday.buffer; off; len; } :: rest ->
        let raw = Cstruct.of_bigarray buffer ~off ~len in
        Lwt.pick
          [ Tuyau.send flow.flow raw
          ; sleep () ] >>= function
        | Ok shift ->
          if shift = len
          then go (n + shift) rest
          else go (n + shift) ({ Faraday.buffer; off= off + shift; len= len - shift; } :: rest)
        | Error `Timeout ->
          flow.wr_closed <- true ;
          safely_close flow >>= fun () ->
          Lwt.return `Closed
        | Error (`Msg err) -> Lwt.fail (Send_error err) in
    go 0 iovecs
  
  let send flow iovecs =
    if flow.wr_closed
    then safely_close flow >>= fun () -> Lwt.return `Closed
    else writev flow iovecs
  
  let close flow =
    if (not flow.rd_closed && flow.wr_closed) || (flow.rd_closed && not flow.wr_closed)
    then ( flow.rd_closed <- true
         ; flow.wr_closed <- true
         ; Log.debug (fun m -> m "properly close the connection.\n%!")
         ; Tuyau.close flow.flow >>= function
           | Ok () -> Lwt.return ()
           | Error (`Msg err) -> Lwt.fail (Close_error err) )
    else Lwt.return ()
  
  let create_connection_handler ?(config= Httpaf.Config.default) ~request_handler ~error_handler =
    let connection_handler edn flow =
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create ~config ~error_handler:(error_handler edn)
          (request_handler edn) in
      let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
      let flow =
        { flow
        ; tmp= Cstruct.create config.read_buffer_size
        ; queue
        ; rd_closed= false; wr_closed= false; } in
      let rd_exit, notify_rd_exit = Lwt.task () in
      let wr_exit, notify_wr_exit = Lwt.task () in
      let rec rd_fiber () =
        let rec go () = match Server_connection.next_read_operation connection with
          | `Read ->
            Log.debug (fun m -> m "next read operation: `read") ;
            recv flow
              ~read:(Server_connection.read connection)
              ~read_eof:(Server_connection.read_eof connection) >>= fun _ ->
            go ()
          | `Yield ->
            Server_connection.yield_reader connection rd_fiber ;
            Lwt.return ()
          | `Close ->
            Lwt.wakeup_later notify_rd_exit () ;
            flow.rd_closed <- true ;
            safely_close flow in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_exn connection exn ;
            Lwt.return ()) in
      let rec wr_fiber () =
        let rec go () = match Server_connection.next_write_operation connection with
          | `Write iovecs ->
            send flow iovecs >>= fun res ->
            Server_connection.report_write_result connection res ;
            go ()
          | `Yield ->
            Server_connection.yield_writer connection wr_fiber ;
            Lwt.return ()
          | `Close _ ->
            Lwt.wakeup_later notify_wr_exit () ;
            flow.wr_closed <- true ;
            safely_close flow in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_exn connection exn ;
            Lwt.return ()) in
      rd_fiber () ;
      wr_fiber () ;
      let threads = [ rd_exit; wr_exit; ] in
      Lwt.join threads >>= fun () ->
      close flow in
    connection_handler
end

module Make (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) = struct
  open Lwt.Infix

  module TCP = Tuyau_mirage_tcp.Make(StackV4)
  module Httpaf = Httpaf(Tuyau_mirage)(Time)

  let tls_endpoint, tls_protocol = Tuyau_mirage_tls.protocol_with_tls ~key:TCP.endpoint TCP.protocol
  let tls_configuration, tls_service = Tuyau_mirage_tls.service_with_tls ~key:TCP.configuration TCP.service tls_protocol

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let http ?config ~error_handler ~request_handler master =
    Tuyau_mirage.impl_of_service ~key:TCP.configuration TCP.service |> Lwt.return >>? fun (module Service) ->
    Tuyau_mirage.impl_of_protocol ~key:TCP.endpoint TCP.protocol |> Lwt.return >>? fun (module Protocol) ->
    let handler edn flow = Httpaf.create_connection_handler ?config ~error_handler ~request_handler edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
        let edn = TCP.dst flow in
        Lwt.async (fun () -> handler edn (Tuyau_mirage.Flow (flow, (module Protocol)))) ; Lwt.pause () >>= go in
    go ()

  let https ?config ~error_handler ~request_handler master =
    let open Rresult in
    Tuyau_mirage.impl_of_service ~key:tls_configuration tls_service |> Lwt.return >>? fun (module Service) ->
    Tuyau_mirage.impl_of_protocol ~key:tls_endpoint tls_protocol |> Lwt.return >>? fun (module Protocol) ->
    let handler edn flow = Httpaf.create_connection_handler ?config ~error_handler ~request_handler edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
        let edn = TCP.dst (Tuyau_mirage_tls.underlying flow) in
        Lwt.async (fun () -> handler edn (Tuyau_mirage.Flow (flow, (module Protocol)))) ; Lwt.pause () >>= go in
    go ()
end
