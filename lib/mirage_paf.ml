open Mirage

let pin_conduit = "git+https://github.com/dinosaure/ocaml-conduit.git#3.0.0"

type conduit = Conduit

let conduit = Type Conduit

let conduit =
  impl
  @@ object
       inherit base_configurable

       method ty = conduit

       method name = "conduit"

       method module_name = "Conduit_mirage"

       method! packages =
         Key.pure
           [
             package ~pin:pin_conduit "conduit";
             package ~pin:pin_conduit "conduit-lwt";
             package ~pin:pin_conduit "conduit-mirage";
           ]
     end

type paf = Paf

let paf = Type Paf

let httpaf =
  impl
  @@ object
       inherit base_configurable

       method ty = time @-> stackv4 @-> paf

       method name = "paf"

       method module_name = "Paf.Make"

       method! packages =
         Key.pure [ package "paf"; package ~pin:pin_conduit "conduit-tls" ]

       method! deps = [ abstract conduit ]
     end
