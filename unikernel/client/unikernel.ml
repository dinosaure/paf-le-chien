open Lwt.Infix

module Make
  (Console : Mirage_console.S)
  (Time : Mirage_time.S)
  (HTTP_Client : Http_mirage_client.S) = struct
  let one_request =
    Http_mirage_client.one_request
      ~alpn_protocol:HTTP_Client.alpn_protocol
      ~authenticator:HTTP_Client.authenticator

  let log console fmt = Fmt.kstr (Console.log console) fmt

  let print_response console uri response =
    let ( let* ) = Lwt.bind in
    let* () = log console ">> %S" uri in
    let* () = log console "> Version: %a" Httpaf.Version.pp_hum response.Http_mirage_client.version in
    let* () = log console "> Status: %a" H2.Status.pp_hum response.Http_mirage_client.status in
    let headers = H2.Headers.to_list response.Http_mirage_client.headers in
    let headers =
      let tbl = Hashtbl.create 0x100 in
      List.iter (fun (k, v) -> Hashtbl.add tbl k v) headers ; tbl in
    let* () = log console "> Headers: @[<hov>%a@]"
      Fmt.(Dump.hashtbl string string) headers in
    let* () = log console ">" in
    Lwt.return_unit

  let print_body console = function
    | None -> Lwt.return_unit
    | Some body ->
      log console "@[<hov>%a@]" (Hxd_string.pp Hxd.default) body

  let start console _time ctx =
    one_request ~ctx (Key_gen.uri ()) >>= fun res ->
    (* XXX(dinosaure): let 5s for the underlying fiber executed into a
       [Lwt.async] to properly finish the job. *)
    Time.sleep_ns 5_000_000_000L >>= fun () -> match res with
    | Ok (response, body) ->
      print_response console (Key_gen.uri ()) response >>= fun () ->
      print_body console body
    | Error err ->
      log console "ERROR: %a" Mimic.pp_error err
end
