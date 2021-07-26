## Paf le chien - A MirageOS compatible layer for [HTTP/AF][httpaf] and [H2][h2]

This library wants to provide an easy way to use HTTP/AF & H2 into a unikernel.
It implements the global /loop/ with a protocol implementation.

The protocol implementation is given by [Mimic][mimic] and should be the
[mirage-tcpip][mirage-tcpip] implementation - however, it can be something
else.

It does the composition between the TLS encryption layer and the
[StackV4V6][stackv4v6] implementation to provide a way to initiate a TLS
server.

```ocaml
module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  module P = Paf_mirage.make(Time)(Stack)

  let start stack =
    let* t = P.init ~port:80 stack in
    let service = P.http_service ~error_handler request_handler in
    let `Initialized th = P.serve service t in
    th
end

(* For UNIX with mirage-time-unix & tcpip.stack-socket *)

include Make (Time) (Tcpip_stack_socket.V4V6)

let stack () =
  let open Tcpip_stack_socket.V4V6 in
  UDP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None >>= fun udp ->
  TCP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.global None >>= fun tcp ->
  connect udp tcp

let () = Lwt_main.run (stack () >>= start)
```

It provides a client-side with the logic of Mimic and let the user to implement
the resolution process to determine if the connection needs the TLS encryption
layer or not.

### Mimic

Paf wants to provide an agnostic implementation of HTTP with the ability to
launch a server or a client from an user-defined context: a `Mimic.ctx`. It
does not exist one and unique way to use Paf because the context can be:
- a MirageOS
- a simple executable
- something else like a JavaScript script (with `js_of_ocaml`)

Mimic ensures the ability to gives a [Mirage_flow.S][mirage-flow] to Paf
(client side). The underlying implementation of this /flow/ depends on what the
user wants. It can be:
- [ocaml-tls][ocaml-tls]
- [lwt_ssl][lwt_ssl]
- [mirage-tcpip][mirage-tcpip]
- The host TCP/IP stack (see the `Unix` module)

All of these choices **is not** done by Paf but must be defined by the user.
Then, the CoHTTP layer trusts on [mirage-tcpip][mirage-tcpip] and
[ocaml-tls][ocaml-tls] to easily communicate with a peer from a given `Uri.t`.
Even if it seems to be the easy way to do HTTP requests (over TLS or not), the
user is able to choose some others possibilities/paths.

For example, the user is able to start a connection with an Unix domain socket:

```ocaml
module Unix_domain_socket : Mimic.Mirage_protocol.S
  with type flow = Unix.file_descr
   and type endpoint = Fpath.t

let unix_domain_socket =
  Mimic.register ~name:"unix-domain-socket" (module Unix_domain_socket)

let ctx =
  Mimic.add unix_domain_socket 
    (Fpath.v "/var/my_domain.sock") Mimic.empty
    
let run =
  Mimic.resolve ~ctx >>= function
  | Error _ as err -> Lwt.return err
  | Ok flow ->
    let body, conn = Httpaf.Client_connection.request ?config:None req
      ~error_handler ~response_handler in
    Paf.run (module Httpaf.Client_connection) ~sleep conn flow >>= fun () ->
    Lwt.return_ok body
```

### CoHTTP layer

Paf comes with a not-fully-implemented compatible layer with CoHTTP. From this
sub-package and the [letsencrypt][letsencrypt] package, Paf provides a process
to download a Let's encrypt TLS certificate ready to launch an HTTPS server.

```ocaml
let cfg =
  { LE.email= Result.to_option (Emile.of_string "romain@x25519.net")
  ; LE.seed= None
  ; LE.certificate_seed= None
  ; LE.hostname= Domain_name.(host_exn (of_string_exn "x25519.net")) }

let ctx = ... (* see [mimic] *)

module P = Paf_mirage.Make (Time) (Tcpip_stack_socket.V4V6)

let get_tls_certificate () =
  Lwt_switch.with_switch @@ fun stop ->
  let* t = P.init ~port:80 stack in
  let service = P.http_service
    ~error_handler
    LE.request_handler in
  let `Initialized th = P.serve ~stop service in
  let fiber =
    LE.provision_certificate ~production:false cfg ctx >>= fun res ->
    Lwt_switch.turn_off stop >>= fun () -> Lwt.return res in
  Lwt.both (th, fiber) >>= fun (_, tls) -> Lwt.return tls
```

### Application Layer Protocol Negotiation

Paf provides the logic behind ALPN negotiation according a _certain_ TLS/SSL
implementation. In other words, Paf is able to correctly dispatch which
protocol the client wants without a requirement of [ocaml-tls][ocaml-tls] or
[lwt_ssl][lwt_ssl]. The module [Alpn] is a HTTP service which handles:
- HTTP/1.1
- H2

[Alpn] requires:
- the `accept` and the `close` function
- a way to extract the result of the Application Layer Protocol Negotiation
- the Mimic's _injection_
- `error_handler` and `request_handler` which handle HTTP/1.0, HTTP/1.1 and
  H2 requests

Here is an example with HTTP (without TLS):
```ocaml
let _, protocol
  : Unix.sockaddr Mimic.value
    * (Unix.sockaddr, Lwt_unix.file_descr) Mimic.protocol
  = Mimic.register ~name:"lwt-tcp" (module TCP)

let accept t =
  Lwt.catch begin fun () ->
    Lwt_unix.accept >>= fun (socket, _) ->
    Lwt.return_ok socket
  end @@ function
  | Unix.Unix_error (err, f, v) ->
    Lwt.return_error (`Unix (err, f, v))
  | exn -> raise exn

let info =
  let module R = (val Mimic.register protocol) in
  { Alpn.alpn= const None
  ; Alpn.peer= (fun socket ->
    sockaddr_to_string (Lwt_unix.getpeername socket))
  ; Alpn.injection=
    (fun socket -> R.T socket) }

let service = Alpn.service info
  ~error_handler
  ~request_handler
  accept Lwt_unix.close

let fiber =
  let t = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind t (Unix.ADDR_INET (Unix.inet_addr_loopback, 8080))
  >>= fun () ->
  let `Initialized th = Paf.serve
    ~sleep:(Lwt_unix.sleep <.> Int64.to_float)
    service t in th

let () = Lwt_main.run fiber
```

### Tests & Benchmark

The distribution comes with a tool which launch several clients to communicate
with a server. We record the time spent for each request and show as the result
the histogram of them. It's not really a benchmark as is but it a good
stress-test and we check that we don't have failure from the server.

[httpaf]: https://github.com/inhabitedtype/httpaf
[mimic]: https://github.com/mirage/ocaml-git
[mirage-tcpip]: https://github.com/mirage/mirage-tcpip
[letsencrypt]: https://github.com/mmaker/ocaml-letsencrypt
[stackv4v6]: https://github.com/mirage/mirage-stack
[ocaml-tls]: https://github.com/mirleft/ocaml-tls
[lwt_ssl]: https://github.com/ocsigen/lwt_ssl
[mirage-flow]: https://github.com/mirage/mirage-flow
