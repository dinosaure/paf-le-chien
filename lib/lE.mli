type configuration = {
  email : Emile.mailbox option;
  seed : string option;
  certificate_seed : string option;
  hostname : [ `host ] Domain_name.t;
}

module Make (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) : sig
  type nonrec configuration = configuration = {
    email : Emile.mailbox option;
    seed : string option;
    certificate_seed : string option;
    hostname : [ `host ] Domain_name.t;
  }

  val request_handler :
    Ipaddr.t * int -> Httpaf.Server_connection.request_handler

  val provision_certificate :
    ?production:bool ->
    configuration ->
    Mimic.ctx ->
    (Tls.Config.own_cert, [> `Msg of string ]) result Lwt.t

  val ctx :
    gethostbyname:
      ('dns ->
      [ `host ] Domain_name.t ->
      (Ipaddr.t, [> `Msg of string ]) result Lwt.t) ->
    authenticator:X509.Authenticator.t ->
    'dns ->
    Stack.t ->
    Mimic.ctx

  val with_uri : Uri.t -> Mimic.ctx -> Mimic.ctx
end
