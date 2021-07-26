open Mirage

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt (some int) None doc))

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
    ~keys:[ Key.abstract port
          ; Key.abstract email
          ; Key.abstract hostname
          ; Key.abstract cert_seed
          ; Key.abstract account_seed
          ; Key.abstract production
          ; Key.abstract https ]
    ~packages:[ package "ca-certs-nss"
              ; package "dns-client.mirage"
              ; package "paf"
              ; package "paf" ~sublibs:[ "mirage" ]
              ; package "paf-le"
              ; package "rock" ]
    (console @-> random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> job)

let random = default_random
let console = default_console
let time = default_time
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let stackv4v6 = generic_stackv4v6 default_network

let () = register "minipaf" [ minipaf $ console $ random $ time $ mclock $ pclock $ stackv4v6 ]
