## A MirageOS with Paf

This simple unikernel shows how to use `paf` and MirageOS **as a client**. We
describe a new _device_ `http_client` which is a composition of several modules
such as [happy-eyeballs][happy-eyeballs] (to be able to resolve domain name)
and [ca-certs][ca-certs] to introduce an _authenticator_.

The usage of this unikernel is simple:
```sh
$ opam pin add -y https://github.com/dinosaure/paf-le-chien
$ git clone https://github.com/dinosaure/paf-le-chien
$ cp -r paf-le-chien/unikernel/client/* paf-unikernel/
$ cd paf-unikernel
$ mirage configure -t unix
$ make depends
$ mirage build
$ ./dist/minipaf -u https://www.google.com/
```

This unikernel is able to talk via http/1.1 and h2 (as `paf` can do). It prints
out the response and the body (in hexadecimal format, thanks to [hxd][hxd]).
The API of `Http_mirage_client` comes from the great project
[http-lwt-client][http-lwt-client] which does the same but specially for the
`unix` platform.

[happy-eyeballs]: https://github.com/roburio/happy-eyeballs
[ca-certs]: https://github.com/mirage/ca-certs
[http-lwt-client]: https://github.com/roburio/http-lwt-client
