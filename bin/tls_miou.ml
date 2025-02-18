module Make (Flow : Mimice.Flow.S) = struct
  type error =
    [ `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Read of Flow.error
    | `Write of Flow.write_error ]

  let pp_error ppf = function
    | `Tls_failure failure -> Tls.Engine.pp_failure ppf failure
    | `Tls_alert alert -> Fmt.string ppf (Tls.Packet.alert_type_to_string alert)
    | `Read err -> Flow.pp_error ppf err
    | `Write err -> Flow.pp_write_error ppf err

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.string ppf "Connection closed by peer"

  type write_error = [ Mimice.Flow.write_error | error ]
  type state = [ `Active of Tls.Engine.state | `End_of_input | `Error of error ]

  type flow = {
    role : [ `Server | `Client ];
    flow : Flow.flow;
    mutable state : state;
    mutable linger : Cstruct.t list;
  }

  let tls_alert alert = `Error (`Tls_alert alert)
  let tls_fail failure = `Error (`Tls_failure failure)

  let lift_read_result = function
    | Ok ((`Data _ | `End_of_input) as x) -> x
    | Error err -> `Error (`Read err)

  let lift_write_result = function
    | Ok () -> `Ok ()
    | Error err -> `Error (`Write err)

  let check_write flow res =
    let () =
      match (flow.state, lift_write_result res) with
      | `Active _, ((`End_of_input | `Error _) as err) ->
          flow.state <- err ;
          Flow.close flow.flow
      | _ -> () in
    match res with
    | Ok () -> Ok ()
    | Error err -> Error (`Write err :> write_error)

  let read_react flow =
    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (res, `Response resp, `Data data) ->
          let state =
            match res with
            | `Ok tls -> `Active tls
            | `Eof -> `End_of_input
            | `Alert alert -> tls_alert alert in
          flow.state <- state ;
          let _ =
            match resp with
            | None -> Ok ()
            | Some buf -> check_write flow (Flow.write flow.flow buf) in
          let () = match res with `Ok _ -> () | _ -> Flow.close flow.flow in
          `Ok data
      | Error (failure, `Response resp) ->
          let reason = tls_fail failure in
          flow.state <- reason ;
          let _ = Flow.write flow.flow resp in
          Flow.close flow.flow ;
          reason in
    match flow.state with
    | (`End_of_input | `Error _) as err -> err
    | `Active _ ->
    match lift_read_result (Flow.read flow.flow) with
    | (`End_of_input | `Error _) as v ->
        flow.state <- v ;
        v
    | `Data buf ->
    match flow.state with
    | `Active tls -> handle tls buf
    | (`End_of_input | `Error _) as v -> v

  let rec read flow =
    match flow.linger with
    | _ :: _ as bufs ->
        flow.linger <- [] ;
        Ok (`Data (Cstruct.concat (List.rev bufs)))
    | [] ->
    match read_react flow with
    | `Ok None -> read flow
    | `Ok (Some buf) -> Ok (`Data buf)
    | `End_of_input -> Ok `End_of_input
    | `Error e -> Error e

  let writev flow bufs =
    match flow.state with
    | `End_of_input -> Error `Closed
    | `Error e -> Error (e :> write_error)
    | `Active tls ->
    match Tls.Engine.send_application_data tls bufs with
    | Some (tls, answer) ->
        flow.state <- `Active tls ;
        check_write flow (Flow.write flow.flow answer)
    | None -> assert false

  let write flow buf = writev flow [ buf ]

  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> Ok flow
    | _ ->
    match read_react flow with
    | `Ok mbuf ->
        flow.linger <- Option.to_list mbuf @ flow.linger ;
        drain_handshake flow
    | `Error e -> Error (e :> write_error)
    | `End_of_input -> Error `Closed

  let close flow =
    match flow.state with
    | `Active tls ->
        flow.state <- `End_of_input ;
        let _, buf = Tls.Engine.send_close_notify tls in
        let _ = Flow.write flow.flow buf in
        Flow.close flow.flow
    | _ -> ()

  let client_of_flow conf ?host flow =
    let conf' =
      match host with None -> conf | Some host -> Tls.Config.peer conf host
    in
    let tls, init = Tls.Engine.client conf' in
    let tls_flow = { role = `Client; flow; state = `Active tls; linger = [] } in
    match check_write tls_flow (Flow.write flow init) with
    | Ok () -> drain_handshake tls_flow
    | Error _ as err -> err
end
