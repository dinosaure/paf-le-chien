open Mirage

let uri =
  let doc = Key.Arg.info ~doc:"URI to fetch." [ "uri" ] in
  Key.(create "uri" Arg.(required string doc))

type http_client = HTTP_client
let http_client = typ HTTP_client

let minipaf =
  foreign "Unikernel.Make"
    ~keys:[ Key.v uri; ]
    ~packages:[ package "paf"
              ; package "h2"
              ; package "hxd" ~sublibs:[ "core"; "string" ]
              ; package "httpaf" ]
    (console @-> time @-> http_client @-> job)

let stack = generic_stackv4v6 default_network
let dns = generic_dns_client stack
let tcp = tcpv4v6_of_stackv4v6 stack

let http_client =
  let connect _ modname = function
    | [ _pclock; _tcpv4v6; ctx ] ->
      Fmt.str {ocaml|%s.connect %s|ocaml} modname ctx
    | _ -> assert false in
  impl ~connect "Http_mirage_client.Make"
    (pclock @-> tcpv4v6 @-> git_client @-> http_client)

let http_client =
  let happy_eyeballs = git_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  http_client $ default_posix_clock $ tcp $ happy_eyeballs

let () = register "minipaf"
    [ minipaf $ default_console $ default_time $ http_client ]
