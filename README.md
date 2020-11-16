## Paf le chien - An MirageOS compatible layer for [HTTP/AF][httpaf]

This library wants to provide an easy way to use HTTP/AF into a unikernel. It
implements the global /loop/ with a protocol implementation.

The protocol implementation is given by [Conduit][conduit] and should be the
[mirage-tcpip][mirage-tcpip] implementation - however, it can be something else.

It does the composition between the TLS encryption layer and the
[StackV4][stackv4] implementation to provide a way to initiate a TLS server.

It provides a client-side with the logic of Conduit and let the user to
implement the resolution process to determine if the connection needs the TLS
encryption layer or not.

### Tests & Benchmark

The distribution comes with a tool which launch several clients to communicate
with a server. We record the time spent for each request and show as the result
the histogram of them. It's not really a benchmark as is but it a good
stress-test to see that we don't have failure from the server.

[httpaf]: https://github.com/inhabitedtype/httpaf
[conduit]: https://github.com/mirage/ocaml-conduit
[stackv4]: https://github.com/mirage/mirage-stack
