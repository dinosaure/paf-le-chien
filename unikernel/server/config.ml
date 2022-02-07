open Mirage

type paf_server = Paf_server

let paf_server = Type.v Paf_server

let paf ~port =
  let packages = [ package ~min:"0.0.8" "paf" ~sublibs:[ "mirage" ] ] in
  let keys = [ Key.v port ] in
  let connect _ modname = function
    | [ _time; tcpv4v6; ] ->
      Fmt.str "%s.init ~port:%a %s" modname Key.serialize_call (Key.v port) tcpv4v6
    | _ -> assert false in
  impl ~packages ~keys ~connect "Paf_mirage.Make"
    (time @-> tcpv4v6 @-> paf_server)

let tcpv4v6_of_stackv4v6 =
  let connect _ modname = function
    | [ stackv4v6 ] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
    | _ -> assert false in
  impl ~connect "Paf_mirage.TCPV4V6" (stackv4v6 @-> tcpv4v6)

let tcpv4v6_of_stackv4v6 stackv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6

let paf ~port ?(time= default_time) tcpv4v6 = paf ~port $ time $ tcpv4v6

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

let email =
  let doc = Key.Arg.info ~doc:"Let's encrypt email." [ "email" ] in
  Key.(create "email" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname of the unikernel." [ "hostname" ] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." [ "cert-seed" ] in
  Key.(create "cert_seed" Arg.(opt (some string) None doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." [ "account-seed" ] in
  Key.(create "account_seed" Arg.(opt (some string) None doc))

let production =
  let doc = Key.Arg.info ~doc:"Let's encrypt production environment." [ "production" ] in
  Key.(create "production" Arg.(opt bool false doc))

let https =
  let doc = Key.Arg.info ~doc:"Start an HTTP server with a TLS certificate." [ "https" ] in
  Key.(create "https" Arg.(flag doc))

let minipaf =
  foreign "Unikernel.Make"
    ~keys:[ Key.v email
          ; Key.v hostname
          ; Key.v cert_seed
          ; Key.v account_seed
          ; Key.v production
          ; Key.v https ]
    ~packages:[ package "ca-certs-nss"
              ; package "dns-client" ~min:"6.1.0" ~sublibs:[ "mirage" ]
              ; package "paf-le" ~min:"0.0.8"
              ; package "rock" ]
    (console @-> random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> paf_server @-> job)

let random = default_random
let console = default_console
let time = default_time
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let stackv4v6 = generic_stackv4v6 default_network

let () = register "minipaf"
  [ minipaf $ console $ random $ time $ mclock $ pclock $ stackv4v6
            $ paf ~port (tcpv4v6_of_stackv4v6 stackv4v6) ]
