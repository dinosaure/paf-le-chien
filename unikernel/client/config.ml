open Mirage

type paf = Paf
let paf = typ Paf

let paf_conf () =
  let packages = [ package "paf" ~sublibs:[ "mirage" ] ] in
  impl @@ object
    inherit base_configurable
    method ty = time @-> stackv4v6 @-> paf
    method module_name = "Paf_mirage.Make"
    method! packages = Key.pure packages
    method name = "paf"
  end

type dns = Dns
let dns = typ Dns

let dns_conf () =
  let packages = [ package "dns-client" ~min:"5.0.0" ~sublibs:[ "mirage" ] ] in
  impl @@ object
    inherit base_configurable
    method ty = random @-> time @-> mclock @-> stackv4v6 @-> dns
    method module_name = "Dns_client_mirage.Make"
    method! packages = Key.pure packages
    method name = "dns"
    method! connect _ modname = function
      | [ _random; _time; _mclock; stackv4v6; ] ->
        Fmt.str {ocaml|Lwt.return (%s.create %s)|ocaml} modname stackv4v6
      | _ -> assert false
  end

let uri =
  let doc = Key.Arg.info ~doc:"URI" [ "u"; "uri" ] in
  Key.(create "uri" Arg.(required string doc))

let minipaf =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract uri ]
    ~packages:[ package "paf-cohttp"
              ; package "ca-certs-nss" ]
    (console @-> time @-> pclock @-> stackv4v6 @-> dns @-> paf @-> job)

let paf time stackv4v6 = paf_conf () $ time $ stackv4v6
let dns random time mclock stackv4v6 = dns_conf () $ random $ time $ mclock $ stackv4v6

let random = default_random
let console = default_console
let time = default_time
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let stackv4v6 = generic_stackv4v6 default_network
let dns = dns random time mclock stackv4v6

let () = register "minipaf" [ minipaf $ console $ time $ pclock $ stackv4v6 $ dns $ paf time stackv4v6 ]
