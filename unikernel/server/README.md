# A MirageOS with Paf

This simple unikernel shows how to use `paf` and MirageOS **as a server**. It
launches a simple server with several endpoints and TLS. You must create 2
directories `certificates` and `keys` and generate a new certificate with
`gen.sh` if you want to test the TLS support.

The server has 4 endpoints:
- `http{,s}://localhost/` which emits a simple `text/plain` page
- `http{,s}://localhost/random` which requires a `x-length` value and an
  optionally `x-seed` (base64 encoded) seed to generate a Base64 page of
  random bytes
- `http{,s}://localhost/hash` requires `x-length` & `x-seed` and it calcultates
  the hash of "random" bytes as the `random` page generated (if you used the
  same seed)
- `http{,s}://localhost/transmit` which copy what you sent

For instance, this is some example of how to use this server:
```sh
$ curl -H x-length:15000 -H x-seed:Vau9IWDa4Jg= --insecure --http1.1 \
  https://localhost:4343/random | base64 -di - | sha256sum > hash.result
$ curl -H x-length:15000 -H x-seed:Vau9IWDa4Jg= --insecure --http2 \
  https://localhost:4343/hash -w "  -\n" > hash.expected
$ diff hash.result hash.expected
$ curl -H x-length:15000 -H x-seed:w3CJxsQ+deQ= https://localhost:4343/random \
  --insecure --http2 > file.txt
$ cat file.txt | curl https://localhost:4343/transmit --http1.1 -d @- \
  --insecure | base64 -d - | sha256sum > hash.0
$ curl -H x-length:15000 -H x-seed:w3CJxsQ+deQ= https://localhost:4343/hash \
  -w "  -\n" > hash.1
$ cat file.txt | base64 -di - | sha256sum > hash.2
$ diff hash.0 hash.1
$ diff hash.0 hash.2
```

Finally, the server implements a proxy which can be used in this way:
```sh
$ curl --proxy http://localhost:8080/ https://www.google.com/
...
```

### How to build and use it?

As any MirageOS projects:
```sh
$ mirage configure -t unix
$ make depends
$ mirage build
$ ./dist/minipaf --tls
...
```

### Issues

Some issues persists on H2 but they already signaled to the
[ocaml-h2][ocaml-h2] maintainer. We suggest to use http/1.1 protocol or help us
to fix these issues.
