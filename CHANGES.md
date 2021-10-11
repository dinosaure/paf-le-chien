### 0.0.6 (2021-10-11) Paris - France

- Use `tls.0.15.0`, `x509.0.15.0` and `letsencrypt.0.4.0` (@hannesm, @dinosaure, #42)
- Fix the documentation (@dinosaure, #43)

### 0.0.5 (2021-03-08) Paris - France

- Use `tls.0.14.0` (@hannesm, @dinosaure, #38)

### 0.0.4 (2021-26-07) Paris - France

- Don't use `disconnect` when the server terminates (@dinosaure, #28)
- The main loop should not leave when it get an error from a client (@dinosaure, #28)
- `Closed` error from TLS layer does not mean that the service is close,
  wrap such error into a client's `write_error` (@dinosaure, #28)
- Do the compression of the ring-buffer at any call of `read` (@dinosaure, #29)
- Refactore the loop under a common implementation (@dinosaure, #30)
- Add a simple accessor to the peer identity (@dinosaure, #31)
- Cut package between `paf` & `paf-cohttp` (@dinosaure, @hannesm, #32)
- Cut package between `paf` & `paf-le` (@dinosaure, @hannesm, #33)
- Update unikernels with the package layout (@dinosaure, #34)

### 0.0.3 (2021-26-04) Paris - France

- Update to X509.0.13.0 (@hannesm, #26)

### 0.0.2 (2021-20-04) Paris - France

- Add simple unikernel as an example of `paf` (@dinosaure, #17)
- Unfunctorize the client part of HTTP/AF (@dinosaure, #18)
- Ensure to pass queries to the server handler (@dinosaure, #19)
- Add the support of `h2` and ALPN (@dinosaure, #20)
- Support `tls.0.13.0` (@dinosaure, @hannesm, #21)
- Add tests about ALPN dispatcher (@dinosaure, #22)

### 0.0.1 (2021-27-01) Paris - France

- First release of paf
