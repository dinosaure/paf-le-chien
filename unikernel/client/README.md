## A MirageOS with Paf

This simple unikernel shows how to use `paf` and MirageOS **as a client**. The
choice was made to infer dynamically the best solution to start a connection
with a peer (with `mimic`) when we use `paf` as a client and to be **explicit**
when we want to start an HTTP(S) server.

### The config.ml

The MirageOS manifest describes 2 things:

- How to make a simple `Paf` module (which provides HTTP and HTTPS connections)
- How to make a simple DNS client (with `ocaml-dns`)

It requires an `uri` argument such as:
```sh
$ ./minipaf -u https://www.google.com/
```

The unikernel needs `ca-certs-nss` which is a serialized version of `ca-certs`
available for MirageOS. By this way, the client is able to start a TLS
connection from trusted TLS anchors - as it's usually done by your
operating-system.

### The unikernel.ml

The unikernel shows how to _compose_ a `Mimic.ctx` to be able to start a TCP/IP
or a TLS connection from a given `Uri.t`. `Mimic.ctx` is like the contex of your
unikernel. It contains multiple processes to _resolve_ a connection:

- `with_tcp` ensures the possibility to start a TCP/IP connection only if
  `Mimic.ctx` contains a `stack`, an `ipaddr` and a port (default to `80`)
- `with_tls` ensures the possibility to start a TLS connection only if
  `Mimic.ctx` contains a `stack`, an `ipaddr`, a port (default to `443`) and a
  `Tls.Config.client` (default to a configuration with the `ca-certs-nss`'s
  authenticator) and optionally a `domain_name`
- `with_resolv` ensures the possibility to provide an `ipaddr` from a given
  `domain_name`
- then, we put the `stack` that the HTTP client should use
- and we put the `dns` that the `with_resolv` should use

All of these parameters are dynamic! Feel free to change them:
- you can restrict the `Tls.Config.client` if you want
- you can force the DNS client to use a special nameserver
- you can remove the DNS resolver...
- etc.

### How to use?

It's really simple (for the UNIX target):
```sh
$ mirage configure
$ make depends
$ mirage build
$ ./minipaf -u https://www.google.com/
```
