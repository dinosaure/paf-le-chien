### 0.5.0 (2023-21-03) Paris - France

- Upgrade to `mirage-crypto-rng.0.11.0` (@hannesm, @dinosaure, #85)
- Be able to specify ALPN protocols (@kit-ty-kate, @dinosaure, #86)
  Also merged into `ocaml-letsencrypt` (see mmaker/ocaml-letsencrypt#33)
- Set the default protocol used for the ALPN negotiation to "http/1.1" (@dinosaure, #87)
  Also merged into `ocaml-letsencrypt` (see mmaker/ocaml-letsencrypt#33)
- Upgrade `paf` to `h2.0.10.0` (@kit-ty-kate, @dinosaure, #83)
- Replace `Cstruct.copy` (deprecated) by `Cstruct.to_string` (@dinosaure, #83)
- Delete `paf-le` package (@dinosaure, @hannesm, #88)
  Implementations are available via the new package `letsencrypt-mirage`

### 0.4.0 (2023-01-10) Paris - France

- Fix memory leak about functor application (@dinosaure, #78)
- Add a new sub-package `le.mirage` to facilite obtaining a let's encrypt certificate
  and expose few functions to handle Let's encrypt certificates (@kit-ty-kate, @dinosaure, @hannesm, #75)

### 0.3.0 (2022-10-19) Paris - France

- Fix a file-descriptor leak when we fail on the TLS handshake (#72, @TheLortex, @dinosaure, @hannesm)
- Add `reneg` function into `Paf_mirage.Make.TLS` (#73, @dinosaure)

### 0.2.0 (2022-09-29) Paris - France

- Fix several issues about h2 protocols (@dinosaure, #70)
- Delete the `Time` device (@dinosaure, @hannesm, #70)
- Upgrade the distribution with the new interface of `Alpn` module and `Paf_mirage` (@dinosaure, #70)
- Update unikernels (@dinosaure, #70)

### 0.1.0 (2022-08-28) Paris - France

- Fix the Let's encrypt support (@dinosaure, #65)
- Update the codebase with `ocamlformat.0.23.0` (@dinosaure, #66)
- Update the project with `h2.0.9.0` (@dinosaure, #67)

### 0.0.9 (2022-04-11) Paris - France

- Fix unikernels (@hannesm, #58)
- Improve the API and documentation (@dinosaure, #59)
- Add TCPV4V6 module to be able to provide a simple functoria device (@dinosaure, #59)
- The HTTP server requires a TCP/IP implementation instead of a Stack implementation (@dinosaure, #59)
- Extend the API by an _handshake_ function which handle the TLS handshake (@TheLortex, #59)

### 0.0.8 (2022-04-01) Paris - France

* Upgrade to `tcpip.7.0.0` (@dinosaure, #54)
* Let the user to manipulate the incoming flow (@dinosaure, #55)

### 0.0.7 (2021-10-12) Paris - France

- Avoid `astring` dependency (@dinosaure, #45)
- Remove `rresult` dependency (@hannesm, @dinosaure, #47)
- Upgrade the code-base with `mirage-stack.3.0.0` (@dinosaure, #49)
- Update ALPN module without GADT (@dinosaure, #50)

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
