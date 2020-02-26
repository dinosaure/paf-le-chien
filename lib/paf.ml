module Ke = Ke.Rke.Weighted

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
    (Time : Mirage_time.S)
    (Service : Tuyau_mirage.SERVICE)
    (Flow : Tuyau_mirage.PROTOCOL with type flow = Service.flow)
  : HTTPAF with type flow = Flow.flow
= struct
  let src = Logs.Src.create "paf"
  module Log = (val Logs.src_log src : Logs.LOG)

  type t =
    { flow : Flow.flow
    ; rd : Cstruct.t
    ; queue : (char, Bigarray.int8_unsigned_elt) Ke.t
    ; mutable rd_closed : bool
    ; mutable wr_closed : bool
    ; mutable closed : bool }

  type flow = Flow.flow
  type endpoint = Ipaddr.V4.t * int

  exception Recv_error of Flow.error
  exception Send_error of Flow.error
  exception Close_error of Flow.error
  exception Timeout

  open Lwt.Infix

  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let rec recv flow ~read ~read_eof =
    match Ke.N.peek flow.queue with
    | [] ->
      if flow.rd_closed
      then ( let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in Lwt.return `Closed )
      else
        let len = min (Ke.available flow.queue) (Cstruct.len flow.rd) in
        let raw = Cstruct.sub flow.rd 0 len in
        ( Flow.recv flow.flow raw >>= function
            | Error err -> Lwt.fail (Recv_error err)
            | Ok `End_of_input ->
              let _ (* 0 *) = read_eof Bigstringaf.empty ~off:0 ~len:0 in
              Log.debug (fun m -> m "[`read] Connection closed.") ;
              flow.rd_closed <- true ; Lwt.return `Closed
            | Ok (`Input len) ->
              Log.debug (fun m -> m "<- %S" (Cstruct.to_string (Cstruct.sub raw 0 len))) ;
              let _ = Ke.N.push_exn flow.queue ~blit ~length:Cstruct.len ~off:0 ~len raw in
              recv flow ~read ~read_eof )
    | src :: _ ->
      let len = Bigstringaf.length src in
      let shift = read src ~off:0 ~len in
      Log.debug (fun m -> m "[`read] shift %d/%d byte(s)" shift len) ;
      Ke.N.shift_exn flow.queue shift ;
      Lwt.return `Continue

  let send flow iovecs =
    if flow.wr_closed then Lwt.return `Closed
    else
      let rec go w = function
        | [] -> Lwt.return (`Ok w)
        | { Faraday.buffer; Faraday.off; Faraday.len; } :: rest ->
          let raw = Cstruct.of_bigarray buffer ~off ~len in
          Log.debug (fun m -> m "-> %S" (Cstruct.to_string raw)) ;
          let sleep () =
            Time.sleep_ns 1000000000L >>= fun () -> Lwt.return (Error `Timeout) in
          Lwt.pick [ sleep ()
                   ; Flow.send flow.flow raw
                     >|= Rresult.R.reword_error (fun err -> `Send err) ]
          >>= function
          | Ok ws ->
            if ws = len then go (w + ws) rest
            else Lwt.return (`Ok (w + ws))
          | Error `Timeout ->
            Log.err (fun m -> m "Timeout on send (more than 1s to send something)") ;
            flow.wr_closed <- true ;
            Lwt.fail Timeout
          | Error (`Send err) ->
            Log.err (fun m -> m "Got an error while sending data.") ;
            (* TODO(dinosaure): [Socket_closed]. *)
            flow.wr_closed <- true ;
            Lwt.fail (Send_error err) in
      go 0 iovecs

  let close flow =
    if flow.closed
    then Lwt.return ()
    else ( flow.rd_closed <- true
         ; flow.wr_closed <- true
         ; flow.closed <- true
         ; Flow.close flow.flow >>= function
           | Ok () -> Lwt.return ()
           | Error err -> Lwt.fail (Close_error err) )

  let create_connection_handler ?(config= Httpaf.Config.default) ~request_handler ~error_handler =
    let connection_handler (edn : endpoint) flow =
      let ip, port = edn in
      Log.debug (fun m -> m "new server connection <%a:%d>." Ipaddr.V4.pp ip port) ;
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create ~config ~error_handler:(error_handler edn)
          (request_handler edn) in
      let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
      let flow =
        { flow; rd= Cstruct.create config.read_buffer_size; queue; rd_closed= false; wr_closed= false; closed= false; } in
      let rd_exit, notify_rd_exit = Lwt.task () in
      let wr_exit, notify_wr_exit = Lwt.task () in
      let rec rd_fiber () =
        let rec go () = match Server_connection.next_read_operation connection with
          | `Read ->
            Log.debug (fun m -> m "[`read]") ;
            ( recv flow
              ~read:(Server_connection.read connection)
              ~read_eof:(Server_connection.read_eof connection) >>= function
              | `Closed ->
                Log.debug (fun m -> m "[`read] Connection closed (wake-up threads)") ;
                Lwt.wakeup notify_rd_exit () ;
                Lwt.wakeup notify_wr_exit () ;
                flow.rd_closed <- true ;
                Lwt.return ()
              | `Continue -> Log.debug (fun m -> m "[`read] is done.") ; go () )
          | `Yield ->
            Log.debug (fun m -> m "[read:`yield]") ;
            Server_connection.yield_reader connection rd_fiber ;
            Lwt.return ()
          | `Close ->
            Log.debug (fun m -> m "[read:`close]") ;
            Lwt.wakeup_later notify_rd_exit () ;
            (* XXX(dinosaure): HTTP/AF relies on one specific aspect of UNIX socket:
               the ability to close as soon as possible the read-side regardless the write
               side. However, the TCP socket provided by [mirage-tcpip] or interface provided
               by [tuyau.mirage] does not give to us this aspect.

               So we fake it by two boolean and ensure to _really_ close the socket at the
               end of the process.
            
               See [httpaf-lwt-unix] for more details. *)
            flow.rd_closed <- true ;
            Lwt.return () in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_exn connection exn ;
            Log.warn (fun m -> m "[`read] got an exception: %s" (Printexc.to_string exn)) ;
            if Lwt.state rd_exit = Sleep then Lwt.wakeup notify_rd_exit () ;
            Lwt.return ()) in
      let rec wr_fiber () =
        let rec go () = match Server_connection.next_write_operation connection with
          | `Write iovecs ->
            Log.debug (fun m -> m "[`write]");
            send flow iovecs >>= fun res ->
            Log.debug (fun m ->
                let pp_res ppf = function
                  | `Ok len -> Fmt.pf ppf "(`Ok %d byte(s))" len
                  | `Closed -> Fmt.pf ppf "`Closed" in
                m "[`write] write %a" pp_res res) ;
            Server_connection.report_write_result connection res ;
            go ()
          | `Yield ->
            Log.debug (fun m -> m "[write:`yield]") ;
            Server_connection.yield_writer connection wr_fiber ;
            Lwt.return ()
          | `Close _ ->
            Log.debug (fun m -> m "[write:`close]") ;
            Lwt.wakeup_later notify_wr_exit () ;
            flow.wr_closed <- true ;
            Lwt.return () in
        Lwt.async @@ fun () ->
        Lwt.catch go (fun exn ->
            Server_connection.report_exn connection exn ;
            Log.warn (fun m -> m "[`write] got an exception: %s" (Printexc.to_string exn)) ;
            if Lwt.state wr_exit = Sleep then Lwt.wakeup notify_wr_exit () ;
            Lwt.return ()) in
      rd_fiber () ;
      wr_fiber () ;
      let threads = [ rd_exit; wr_exit; ] in
      let catch_and_cancel = function
        | Lwt.Canceled -> ()
        | v ->
          List.iter Lwt.cancel threads ;
          !Lwt.async_exception_hook v in
      List.iter (fun th -> Lwt.on_failure th catch_and_cancel) threads ;
      let ip, port = edn in
      Log.debug (fun m -> m "<%a:%d> waiting." Ipaddr.V4.pp ip port) ;
      Lwt.join threads >>= fun () ->
      Log.debug (fun m -> m "close <%a:%d>." Ipaddr.V4.pp ip port) ;
      close flow in
    connection_handler
end

module Make (Time : Mirage_time.S) (StackV4 : Mirage_stack.V4) = struct
  open Lwt.Infix

  module TCP = Tuyau_mirage_tcp.Make(StackV4)

  let tls_endpoint, tls_protocol = Tuyau_mirage_tls.protocol_with_tls ~key:TCP.endpoint TCP.protocol
  let tls_configuration, tls_service = Tuyau_mirage_tls.service_with_tls ~key:TCP.configuration TCP.service tls_protocol

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let http ?config ~error_handler ~request_handler master =
    Tuyau_mirage.impl_of_service ~key:TCP.configuration TCP.service |> Lwt.return >>? fun (module Service) ->
    Tuyau_mirage.impl_of_protocol ~key:TCP.endpoint TCP.protocol |> Lwt.return >>? fun (module Protocol) ->
    let module Httpaf = Httpaf(Time)(Service)(Protocol) in
    let handler edn flow = Httpaf.create_connection_handler ?config ~error_handler ~request_handler edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
        let edn = TCP.dst flow in
        handler edn flow >>= go in
    go ()

  let https ?config ~error_handler ~request_handler master =
    let open Rresult in
    Tuyau_mirage.impl_of_service ~key:tls_configuration tls_service |> Lwt.return >>? fun (module Service) ->
    Tuyau_mirage.impl_of_protocol ~key:tls_endpoint tls_protocol |> Lwt.return >>? fun (module Protocol) ->
    let module Httpaf = Httpaf(Time)(Service)(Protocol) in
    let handler edn flow = Httpaf.create_connection_handler ?config ~error_handler ~request_handler edn flow in
    let rec go () =
      let open Lwt.Infix in
      Service.accept master >>= function
      | Error err -> Lwt.return (Rresult.R.error_msgf "%a" Service.pp_error err)
      | Ok flow ->
        let edn = TCP.dst (Tuyau_mirage_tls.underlying flow) in
        Lwt.async (fun () -> handler edn flow) ; Lwt.pause () >>= go in
    go ()
end
