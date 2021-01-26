open Fiber

let pr fmt = Format.printf fmt

let epr fmt = Format.eprintf fmt

external now : unit -> (int64[@unboxed]) = "b_mclock" "n_mclock" [@@noalloc]

let reporter pid ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%06d]%a[%a]: " ^^ fmt ^^ "\n%!")
        pid Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let run uri =
  let open Lwt.Infix in
  let t0 = now () in
  Simple_client.run uri >>= fun _ ->
  let t1 = now () in
  Lwt.return (Int64.sub t1 t0)

let run_client uri =
  run_process (fun () ->
      let () = Mirage_crypto_rng_unix.initialize () in
      Lwt_main.run (run uri))

let const x _ = x

let clients ~n uri =
  parallel_map (List.init n (const uri)) ~f:run_client >>| Array.of_list

let count ~p arr =
  let res = ref 0 in
  Array.iter (fun x -> if p x then incr res) arr ;
  !res

let fold ~f a v = Array.fold_left f a v

let map ~f a = Array.map f a

let is_ok = function Ok _ -> true | _ -> false

let is_error = function Error _ -> true | _ -> false

let get_ok = function Ok v -> v | Error _ -> assert false

let to_sec x =
  let x = Int64.to_float x in
  x /. 1e9

let ( <.> ) f g x = f (g x)

let histogram res =
  let tbl = Hashtbl.create 0x100 in
  Array.iter
    (fun v ->
      let v = Float.round (to_sec v *. 1e2) in
      try
        let n = Hashtbl.find tbl v in
        Hashtbl.replace tbl v (succ n)
      with _ -> Hashtbl.add tbl v 1)
    res ;
  let res = Hashtbl.fold (fun k v a -> (k *. 1e-2, v) :: a) tbl [] in
  List.sort (fun (a, _) (b, _) -> Float.compare a b) res

let utf8_chars =
  (* Characters: space @ [0x258F .. 0x2589] *)
  [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let utf_num = Array.length utf8_chars - 1

(* (c) CraigFe *)
let show_bar width ppf proportion =
  let bar_width =
    let width = width () in
    width - 2 in
  let squaresf = Float.of_int bar_width *. proportion in
  let squares = Float.to_int squaresf in
  let filled = min squares bar_width in
  let not_filled = bar_width - filled - 1 in
  Format.pp_print_string ppf "│" ;
  for _ = 1 to filled do
    Format.pp_print_string ppf utf8_chars.(utf_num)
  done ;
  (if filled <> bar_width
  then
    let () =
      let chunks = Float.to_int (squaresf *. Float.of_int utf_num) in
      let index = chunks - (filled * utf_num) in
      if index < utf_num then Format.pp_print_string ppf utf8_chars.(index)
    in
    for _ = 1 to not_filled do
      Format.pp_print_string ppf utf8_chars.(0)
    done) ;
  Format.pp_print_string ppf "│"

let exit_failure = 1

let exit_success = 0

let show res =
  let er = count ~p:is_error res in
  if er > 0
  then (
    pr "Got %d error(s).\n%!" er ;
    exit exit_failure)
  else
    let res = map ~f:get_ok res in
    let total = fold ~f:Int64.add 0L res in
    let max = fold ~f:max 0L res in
    let min = fold ~f:min max res in
    let avg = Int64.div total (Int64.of_int (Array.length res)) in
    pr "Total:   %2.03fs\n%!" (to_sec total) ;
    pr "Slowest: %2.03fs\n%!" (to_sec max) ;
    pr "Fastest: %2.03fs\n%!" (to_sec min) ;
    pr "Average: %2.03fs\n%!" (to_sec avg) ;
    let histogram = histogram res in
    let width () = 40 in
    let max =
      Float.of_int (List.fold_left (fun a (_, v) -> v + a) 0 histogram) in
    pr "\n%!" ;
    pr "Response time histogram:\n%!" ;
    List.iter
      (fun (k, v) ->
        let v = Int64.of_int v in
        let p = Int64.to_float v /. max in
        pr "%0.3f [%03Ld]\t%a\n%!" k v (show_bar width) p)
      histogram ;
    exit exit_success

let concurrency = ref 50

let number = ref 200

let uri = ref None

let anonymous_argument v =
  match !uri with
  | None -> (
      try uri := Some (Uri.of_string v)
      with _ ->
        Format.eprintf "Invalid uri: %S.\n%!" v ;
        exit exit_failure)
  | Some _ -> ()

let spec =
  [
    ( "-c",
      Arg.Set_int concurrency,
      "Number of workers to run concurrently. Total number of requests cannot \
       be smaller than the concurrency level. Default is 50." );
    ("-n", Arg.Set_int number, "Number of requests to run. Default is 200.");
  ]

let usage = Format.asprintf "%s [-c <number>] [-n <number>] uri" Sys.argv.(0)

let () =
  Arg.parse spec anonymous_argument usage ;
  match !uri with
  | Some uri ->
      Fiber.set_concurrency !concurrency ;
      (* Lwt_preemptive.init !concurrency !concurrency ignore ; *)
      let res = Fiber.run (clients ~n:!number uri) in
      show res
  | None ->
      Format.eprintf "%s\n%!" usage ;
      exit exit_failure
