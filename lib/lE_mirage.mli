(** High-level module for Let's encrypt *)

module Make
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK) : sig
  val get_certificates :
    yes_my_port_80_is_reachable_and_unused:Stack.t ->
    production:bool ->
    LE.configuration ->
    (Tls.Config.own_cert, [> `Msg of string ]) result Lwt.t
  (** [get_certificates ~yes_my_port_80_is_reachable_and_unused ~production cfg]
      tries to resolve the Let's encrypt challenge by initiating an HTTP server
      on port 80 and handling requests from it with [ocaml-letsencrypt].

      This resolution requires that your domain name (requested in the given
      [cfg.hostname]) redirects Let's encrypt to this HTTP server. You probably
      need to check your DNS configuration. *)
end
