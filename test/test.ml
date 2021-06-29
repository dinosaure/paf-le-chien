let null = Unix.openfile "/dev/null" Unix.[ O_CLOEXEC ] 0o644

let () = at_exit (fun () -> try Unix.close null with _exn -> ())

let create_lock filename =
  let fd = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  ignore (Unix.lseek fd 0 Unix.SEEK_SET) ;
  fd

let lock fd = Unix.lockf fd Unix.F_LOCK 0

let unlock fd = Unix.lockf fd Unix.F_ULOCK 0

(* XXX(dinosaure): this test wants to check with **true** parallelism
 * that our server and our client works together (at least). The true
 * parallelism is done by the clone()/fork() syscall - by this way,
 * we are not constrained by the global GC lock.
 *
 * locks ([lock.8080]/[lock.4343]) permit to launch safely clients
 * when, at least, servers are initialised. Then, we launch clients
 * [N] times on some specific endpoints:
 * - [/]
 * - [/large]
 * with TLS and without TLS. We see then (with a monotonic clock)
 * the time spent by such request and generate an histogram. If
 * one request fails, the test fails. Otherwise, we have a performance
 * report about our implementation.
 *
 * The test does not want to provide metrics about performance. It
 * gives this information but it's not real /benchmark/! *)

let launch_server () =
  let lock0 = create_lock "lock.4343" in
  let lock1 = create_lock "lock.8080" in
  let pid0 =
    Unix.create_process_env "./simple_server.exe"
      [|
        "./simple_server.exe";
        "--with-tls";
        "server.pem";
        "server.key";
        "file.txt";
      |]
      [||] null Unix.stdout null in
  let pid1 =
    Unix.create_process_env "./simple_server.exe"
      [| "./simple_server.exe"; "file.txt" |]
      [||] null Unix.stdout null in
  at_exit (fun () ->
      try
        Unix.close lock0 ;
        Unix.unlink "lock.4343"
      with _exn -> ()) ;
  at_exit (fun () ->
      try
        Unix.close lock1 ;
        Unix.unlink "lock.8080"
      with _exn -> ()) ;
  lock lock0 ;
  lock lock1 ;
  (lock0, lock1, pid0, pid1)

let launch_clients c n uri =
  Format.printf "===== -c %d -n %d %a =====\n%!" c n Uri.pp uri ;
  let pid =
    Unix.create_process_env "./clients.exe"
      [|
        "./clients.exe";
        "-c";
        string_of_int c;
        "-n";
        string_of_int n;
        Uri.to_string uri;
      |]
      [||] Unix.stdin Unix.stdout Unix.stderr in
  let _, _ = Unix.waitpid [] pid in
  Format.printf "\n%!"

let concurrency = ref 50

let number = ref 200

let anonymous_argument _ = ()

let spec =
  [
    ( "-c",
      Arg.Set_int concurrency,
      "Number of workers to run concurrently. Total number of requests cannot \
       be smaller than the concurrency level. Default is 50." );
    ("-n", Arg.Set_int number, "Number of requests to run. Default is 200.");
  ]

let usage = Format.asprintf "%s [-c <number>] [-n <number>]" Sys.argv.(0)

let () =
  Arg.parse spec anonymous_argument usage ;
  let lock0, lock1, pid0, pid1 = launch_server () in
  lock lock0 ;
  lock lock1 ;
  Unix.sleep 2 ;
  (* XXX(dinosaure): needed because [Paf.init/Stack.listen] does not ensure that
   * we listen **after**. Lwt can schedule it in an other way... see mirage/mirage-tcpip#438 *)
  launch_clients !concurrency !number (Uri.of_string "https://localhost:4343/") ;
  launch_clients !concurrency !number
    (Uri.of_string "https://localhost:4343/large") ;
  launch_clients !concurrency !number (Uri.of_string "http://localhost:8080/") ;
  launch_clients !concurrency !number
    (Uri.of_string "http://localhost:8080/large") ;
  Unix.kill pid0 Sys.sigint ;
  Unix.kill pid1 Sys.sigint ;
  unlock lock0 ;
  unlock lock1
