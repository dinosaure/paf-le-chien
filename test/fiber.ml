let src = Logs.Src.create "fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a t = ('a -> unit) -> unit

let return x k = k x

let ( >>> ) a b k = a (fun () -> b k)

let ( >>= ) t f k = t (fun x -> f x k)

let ( >>| ) t f k = t (fun x -> k (f x))

let both a b =
  a >>= fun a ->
  b >>= fun b -> return (a, b)

module Ivar = struct
  type 'a state = Full of 'a | Empty of ('a -> unit) Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x =
    match t.state with
    | Full _ -> failwith "Ivar.fill"
    | Empty q ->
        t.state <- Full x ;
        Queue.iter (fun f -> f x) q

  let read t k = match t.state with Full x -> k x | Empty q -> Queue.push k q
end

type 'a ivar = 'a Ivar.t

module Future = struct
  let wait = Ivar.read
end

let fork f k =
  let ivar = Ivar.create () in
  f () (fun x -> Ivar.fill ivar x) ;
  k ivar

let fork_and_join f g =
  fork f >>= fun a ->
  fork g >>= fun b -> both (Future.wait a) (Future.wait b)

let fork_and_join_unit f g =
  fork f >>= fun a ->
  fork g >>= fun b -> Future.wait a >>> Future.wait b

let rec parallel_map l ~f =
  match l with
  | [] -> return []
  | x :: l ->
      fork (fun () -> f x) >>= fun future ->
      parallel_map l ~f >>= fun l ->
      Future.wait future >>= fun x -> return (x :: l)

let rec parallel_iter l ~f =
  match l with
  | [] -> return ()
  | x :: l ->
      fork (fun () -> f x) >>= fun future ->
      parallel_iter l ~f >>= fun () -> Future.wait future

let safe_close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let create_process prgn =
  let out0, out1 = Unix.pipe () in
  (* XXX(dinosaure): to ~safely~ use [Lwt_main.run] , we must use [Lwt_unix.fork].
   * However, this code is **really bad**! You should never start an [Lwt_main.run]
   * inside a /fork/. [Lwt_unix.fork] ensures to properly clone() for a sub-lwt-process
   * but this code can easily break. *)
  Log.debug (fun m -> m "Create a new process.") ;
  match Lwt_unix.fork () with
  | 0 -> (
      Unix.close out0 ;
      let oc = Unix.out_channel_of_descr out1 in
      try
        Marshal.to_channel oc (prgn ()) [ Marshal.No_sharing ] ;
        Log.debug (fun m ->
            m "Transmit the result of the program to the parent.") ;
        flush oc ;
        Unix.close out1 ;
        Log.debug (fun m -> m "Process ended.") ;
        exit 0
      with exn ->
        Log.err (fun m ->
            m "Process ended with an exception: %s." (Printexc.to_string exn)) ;
        exit 127)
  | pid ->
      Log.debug (fun m -> m "%d created." pid) ;
      Unix.close out1 ;
      (out0, pid)

let concurrency = ref 4

let running = Hashtbl.create ~random:false !concurrency

let waiting_for_slot = Queue.create ()

let set_concurrency n = concurrency := n

let get_concurrency () = !concurrency

let throttle () =
  if Hashtbl.length running >= !concurrency
  then (
    let ivar = Ivar.create () in
    Queue.push ivar waiting_for_slot ;
    Log.debug (fun m -> m "Waiting for a new slot.") ;
    Ivar.read ivar)
  else return ()

let restart_throttle () =
  while
    Hashtbl.length running < !concurrency
    && not (Queue.is_empty waiting_for_slot)
  do
    Ivar.fill (Queue.pop waiting_for_slot) ()
  done

let run_process prgn =
  throttle () >>= fun () ->
  let fd, pid = create_process prgn in
  let ivar = Ivar.create () in
  Hashtbl.add running pid ivar ;
  Ivar.read ivar >>= fun status ->
  Log.debug (fun m -> m "%d ended." pid) ;
  let ic = Unix.in_channel_of_descr fd in
  let res = Marshal.from_channel ic in
  safe_close fd ;
  match status with
  | Unix.WEXITED 0 ->
      Log.debug (fun m -> m "%d ended properly." pid) ;
      return (Ok res)
  | Unix.WEXITED n ->
      Log.err (fun m -> m "%d got an error: %d." pid n) ;
      return (Error n)
  | Unix.WSIGNALED _ ->
      Log.err (fun m -> m "%d received a signal." pid) ;
      return (Error 255)
  | Unix.WSTOPPED _ ->
      Log.err (fun m -> m "%d was stopped." pid) ;
      assert false

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x) ;
  let rec loop () =
    if Hashtbl.length running > 0
    then (
      Log.debug (fun m -> m "Waiting a process.") ;
      let pid, status = Unix.wait () in
      let ivar = Hashtbl.find running pid in
      Hashtbl.remove running pid ;
      Ivar.fill ivar status ;
      restart_throttle () ;
      loop ())
    else match !result with Some x -> x | None -> failwith "fiber" in
  loop ()
