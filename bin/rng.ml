open Mirage_crypto_rng

let periodic ~f delta =
  Miou.call @@ fun () ->
  let rec one () =
    f () ;
    Miou_unix.sleep (Duration.to_f delta) ;
    one () in
  one ()

let getrandom delta source =
  let f () =
    let per_pool = 8 in
    let size = per_pool * pools None in
    let random = Mirage_crypto_rng_unix.getrandom size in
    let idx = ref 0 in
    let f () =
      incr idx ;
      Cstruct.sub random (per_pool * pred !idx) per_pool in
    Entropy.feed_pools None source f in
  periodic ~f delta

let rdrand delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> Fmt.failwith "RNG not supported"
  | Ok cpu_rng -> periodic ~f:(cpu_rng None) delta

let running = ref false

let getrandom_init i =
  let data = Mirage_crypto_rng_unix.getrandom 128 in
  Entropy.header i data

let initialize (type a) ?g ?(sleep = Duration.of_sec 1) (rng : a generator) =
  if not !running
  then begin
    let () =
      try
        let _ = default_generator () in
        ()
      with No_default_generator -> () in
    running := true ;
    let seed =
      let init =
        Entropy.[ bootstrap; whirlwind_bootstrap; bootstrap; getrandom_init ]
      in
      List.mapi (fun i f -> f i) init |> Cstruct.concat in
    let rng = create ?g ~seed ~time:Mtime_clock.elapsed_ns rng in
    set_default_generator rng ;
    Miou.call_cc @@ fun () ->
    let rdrand = rdrand sleep in
    let source = Entropy.register_source "getrandom" in
    let getrandom = getrandom (Int64.mul sleep 10L) source in
    ignore (Miou.await_all [ rdrand; getrandom ])
  end
  else Fmt.failwith "Rng.initialize has already beed called"
