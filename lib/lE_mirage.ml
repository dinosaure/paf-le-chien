open Lwt.Infix

let ( >>? ) = Lwt_result.bind

let pp_error ppf = function
  | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
  | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

module Make
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK) =
struct
  module Paf = Paf_mirage.Make (Stack.TCP)
  module LE = LE.Make (Time) (Stack)
  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  module Nss = Ca_certs_nss.Make (Pclock)

  let get_certificates ~yes_my_port_80_is_reachable_and_unused:stackv4v6
      ~production config =
    let ctx =
      let gethostbyname dns domain_name =
        DNS.gethostbyname dns domain_name >>? fun ipv4 ->
        Lwt.return_ok (Ipaddr.V4 ipv4) in
      LE.ctx ~gethostbyname
        ~authenticator:(Result.get_ok (Nss.authenticator ()))
        (DNS.create stackv4v6) stackv4v6 in
    Paf.init ~port:80 (Stack.tcp stackv4v6) >>= fun t ->
    let `Initialized web_server, stop_web_server =
      let request_handler _ = LE.request_handler in
      let error_handler _dst ?request err _ =
        Logs.err (fun m ->
            m "error %a while processing request %a" pp_error err
              Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum)
              request) in
      let stop = Lwt_switch.create () in
      (Paf.serve ~stop (Paf.http_service ~error_handler request_handler) t, stop)
    in
    Logs.info (fun m -> m "listening on 80/HTTP (let's encrypt provisioning)") ;
    let provision_certificate =
      (* XXX(dinosaure): we assume that [provision_certificate] terminates.
         By this way, we are able to stop our web-server and resolve our
         [Lwt.both]. *)
      LE.provision_certificate ~production config ctx >>= fun v ->
      Lwt_switch.turn_off stop_web_server >>= fun () -> Lwt.return v in
    Lwt.both web_server provision_certificate >|= snd
end
