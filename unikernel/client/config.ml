open Mirage

type paf = Paf
let paf = typ Paf

let paf_conf () =
  let packages = [ package "paf" ] in
  impl @@ object
    inherit base_configurable
    method ty = time @-> stackv4v6 @-> paf
    method module_name = "Paf.Make"
    method! packages = Key.pure packages
    method name = "paf"
  end

type paf_cohttp = Paf_cohttp
let paf_cohttp = typ Paf_cohttp

let paf_cohttp_conf () =
  let packages = [ package "paf" ~sublibs:[ "cohttp" ] ] in
  impl @@ object
    inherit base_configurable
    method ty = paf @-> paf_cohttp
    method module_name = "Paf_cohttp.Make"
    method! packages = Key.pure packages
    method name = "paf_cohttp"
  end

type dns = Dns
let dns = typ Dns

let dns_conf () =
  let packages = [ package "dns-client" ~sublibs:[ "mirage" ] ] in
  impl @@ object
    inherit base_configurable
    method ty = random @-> time @-> mclock @-> stackv4 @-> dns
    method module_name = "Dns_client_mirage.Make"
    method! packages = Key.pure packages
    method name = "dns"
    method! connect _ modname = function
      | [ _random; _time; _mclock; stackv4; ] ->
        Fmt.str {ocaml|Lwt.return (%s.create %s)|ocaml} modname stackv4
      | _ -> assert false
  end

let uri =
  let doc = Key.Arg.info ~doc:"URI" [ "u"; "uri" ] in
  Key.(create "uri" Arg.(required string doc))

let minipaf =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract uri ]
    ~packages:[ package "ca-certs-nss" ]
    (console @-> time @-> pclock @-> stackv4v6 @-> dns @-> paf_cohttp @-> job)

let paf time stackv4v6 = paf_conf () $ time $ stackv4v6
let paf_cohttp paf = paf_cohttp_conf () $ paf
let dns random time mclock stackv4 = dns_conf () $ random $ time $ mclock $ stackv4

let random = default_random
let console = default_console
let time = default_time
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let stackv4v6 = generic_stackv4v6 default_network
let stackv4 = generic_stackv4 default_network
let dns = dns random time mclock stackv4
let paf_cohttp = paf_cohttp (paf time stackv4v6) 

let () = register "minipaf" [ minipaf $ console $ time $ pclock $ stackv4v6 $ dns $ paf_cohttp ]
