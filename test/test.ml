open Obuilder

module B = Builder(Mock_store)(Mock_sandbox)(Docker_extract)

let ( / ) = Filename.concat
let sprintf = Printf.sprintf
let root = if Sys.win32 then "C:/" else "/"

let () =
  Logs.(set_level ~all:true (Some Info));
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Os.process_exec := Mock_exec.exec

let build_result =
  Alcotest.of_pp @@ fun f x ->
  match x with
  | Error (`Msg msg) -> Fmt.string f msg
  | Error `Cancelled -> Fmt.string f "Cancelled"
  | Ok id -> Fmt.string f id

let get store path id =
  let result = Mock_store.path store id in
  Ok (In_channel.with_open_bin (result / "rootfs" / path) In_channel.input_all)

let with_config fn =
  Eio.Switch.run @@ fun sw ->
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create () in
  let builder = B.v ~sw ~store ~sandbox in
  Fun.protect ~finally:(fun () -> B.finish builder) @@ fun () ->
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  fn ~src_dir ~store ~sandbox ~builder

let with_default_exec fn =
  Fun.protect (fun () ->
      Os.process_exec := Os.default_exec;
      fn ())
    ~finally:(fun () -> Os.process_exec := Mock_exec.exec)

let with_file path flags perms fn =
  let fd = Unix.openfile path flags perms in
  Fun.protect (fun () -> fn fd) ~finally:(fun () -> (try Unix.close fd with Unix.Unix_error _ -> ()))

(* In direct-style, [result] is an Eio promise that will be resolved later
   to provide the mock operation's return value. When not specified, it defaults
   to a pre-resolved [Ok ()].
   [cancel] is a resolver: when the build is cancelled, it resolves the [result]
   promise to [Error `Cancelled]. *)
let mock_op ?result ?(delay_store=Mock_store.already_resolved) ?cancel ?output () =
  let default_result =
    let p, r = Eio.Promise.create () in
    Eio.Promise.resolve r (Ok ());
    p
  in
  let result = Option.value ~default:default_result result in
  fun ~cancelled ?stdin:_ ~log (config:Obuilder.Config.t) dir ->
  Mock_store.delay_store := delay_store;
  let cmd =
    match config.argv with
    | ["/usr/bin/env" ; "bash"; "-c"; cmd] | ["cmd"; "/S"; "/C"; cmd] -> cmd
    | x -> Fmt.str "%a" Fmt.(Dump.list string) x
  in
  Build_log.printf log "%s@." cmd;
  let rootfs = dir / "rootfs" in
  begin match output with
    | Some (`Constant v) -> Out_channel.with_open_bin (rootfs / "output") (fun ch -> Out_channel.output_string ch v)
    | Some (`Append (v, src)) ->
      let src = In_channel.with_open_bin (rootfs / src) In_channel.input_all in
      Out_channel.with_open_bin (rootfs / "output") (fun ch -> Out_channel.output_string ch (src ^ v))
    | Some `Append_cmd ->
      let src = In_channel.with_open_bin (rootfs / "output") In_channel.input_all in
      Out_channel.with_open_bin (rootfs / "output") (fun ch -> Out_channel.output_string ch (src ^ cmd))
    | None -> ()
  end;
  match cancel with
  | Some cancel ->
    (* Race between: the result being resolved normally, or cancelled firing. *)
    Eio.Fiber.first
      (fun () -> Eio.Promise.await result)
      (fun () ->
        Eio.Promise.await cancelled;
        (* Also resolve the result promise so the other fiber can complete *)
        Eio.Promise.resolve cancel (Error `Cancelled);
        Error `Cancelled)
  | None ->
    Eio.Promise.await result

let test_simple () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = Result.bind (B.build builder context spec) (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Append))
      Append
      ;---> saved as .*
     |} root) log;
  (* Check result is cached *)
  Log.clear log;
  let result = Result.bind (B.build builder context spec) (get store "output") in
  Alcotest.(check build_result) "Final result cached" (Ok "base-distro\nrunner") result;
  Log.check "Check cached log"
    (sprintf {|(from base)
      ;---> using .* from cache
      %s: (run (shell Append))
      Append
      ;---> using .* from cache
     |} root) log

let test_prune () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let start = Unix.(gettimeofday () |> gmtime) in
  let log = Log.create "b" in
  let context = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = Result.bind (B.build builder context spec) (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Append))
      Append
      ;---> saved as .*
     |} root) log;
  let log id = Logs.info (fun f -> f "Deleting %S" id) in
  let n = B.prune ~log builder ~before:start 10 in
  Alcotest.(check int) "Nothing before start time" 0 n;
  let end_time = Unix.(gettimeofday () +. 60.0 |> gmtime) in
  let n = B.prune ~log builder ~before:end_time 10 in
  Alcotest.(check int) "Prune" 2 n

(* Helper to fork a build into a fiber and get a promise for the result *)
let fork_build ~sw builder context spec =
  Eio.Fiber.fork_promise ~sw (fun () ->
    B.build builder context spec
  )

(* Two builds, [A;B] and [A;C] are started together. The [A] command is only run once,
   with the log visible to both while the build is still in progress. *)
let test_concurrent () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base"[ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base"[ run "A"; run "C" ]) in
  let a, a_done = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec1 in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  let b2 = fork_build ~sw builder context2 spec2 in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  Eio.Promise.resolve a_done (Ok ());
  let b1 = Eio.Promise.await_exn b1 |> fun r -> Result.bind r (get store "output") in
  let b2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "AB") b1;
  Alcotest.(check build_result) "Final result" (Ok "AC") b2;
  Log.check "Check AB log"
    (sprintf {| (from base)
      ;---> saved as .*
       %s: (run (shell A))
       A
      ;---> saved as .*
       %s: (run (shell B))
       B
      ;---> saved as .*
     |} root root)
    log1;
  Log.check "Check AC log"
    (sprintf {| (from base)
      ;---> using .* from cache
       %s: (run (shell A))
       A
      ;---> saved as .*
       %s: (run (shell C))
       C
      ;---> saved as .*
     |} root root)
    log2

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails. *)
let test_concurrent_failure () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "C" ]) in
  let a, a_done = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ());
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec1 in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  let b2 = fork_build ~sw builder context2 spec2 in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  Eio.Promise.resolve a_done (Error (`Msg "Mock build failure"));
  let b1 = Eio.Promise.await_exn b1 |> fun r -> Result.bind r (get store "output") in
  let b2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log"
    (sprintf {| (from base)
      ;---> saved as .*
       %s: (run (shell A))
       A
     |} root)
    log1;
  Log.check "Check AC log"
    (sprintf {| (from base)
      ;---> using .* from cache
       %s: (run (shell A))
       A
     |} root)
    log2

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails
   just as the second build is trying to open the log file. *)
let test_concurrent_failure_2 () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~shell:(Mock_sandbox.shell sandbox) ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "C" ]) in
  let a, a_done = Eio.Promise.create () in
  let fail_result =
    let p, r = Eio.Promise.create () in
    Eio.Promise.resolve r (Error (`Msg "Mock build failure"));
    p
  in
  Mock_sandbox.expect sandbox (mock_op ~result:fail_result ~delay_store:a ());
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec1 in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  let b2 = fork_build ~sw builder context2 spec2 in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell A))\nA\n" root);
  Eio.Promise.resolve a_done ();
  let b1 = Eio.Promise.await_exn b1 |> fun r -> Result.bind r (get store "output") in
  let b2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log"
    (sprintf {| (from base)
      ;---> saved as .*
       %s: (run (shell A))
       A
     |} root)
    log1;
  Log.check "Check AC log"
    (sprintf {| (from base)
      ;---> using .* from cache
       %s: (run (shell A))
       A
     |} root)
    log2

let test_cancel () =
  with_config @@ fun ~src_dir ~store:_ ~sandbox ~builder ->
  let log = Log.create "b" in
  let cancelled, resolve_cancelled = Eio.Promise.create () in
  let context = Context.v ~shell:(Mock_sandbox.shell sandbox) ~cancelled ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  Eio.Switch.run @@ fun sw ->
  let b = fork_build ~sw builder context spec in
  Log.await log (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve resolve_cancelled ();
  let result = Eio.Promise.await_exn b in
  Alcotest.(check build_result) "Final result" (Error `Cancelled) result;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Wait))
      Wait
     |} root) log

(* Two users are sharing a build. One cancels. *)
let test_cancel_2 () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~output:(`Constant "ok") ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let cancelled1, resolve_cancelled1 = Eio.Promise.create () in
  let cancelled2, _resolve_cancelled2 = Eio.Promise.create () in
  let context1 = Context.v ~cancelled:cancelled1 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~cancelled:cancelled2 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log2) () in
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  let b2 = fork_build ~sw builder context2 spec in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve resolve_cancelled1 ();
  let result1 = Eio.Promise.await_exn b1 in
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Wait))
      Wait
     |} root) log1;
  Eio.Promise.resolve set_r (Ok ());
  let result2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "ok") result2;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> using .* from cache
      %s: (run (shell Wait))
      Wait
      ;---> saved as .*
     |} root) log2

(* Two users are sharing a build. Both cancel. *)
let test_cancel_3 () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let cancelled1, resolve_cancelled1 = Eio.Promise.create () in
  let cancelled2, resolve_cancelled2 = Eio.Promise.create () in
  let context1 = Context.v ~cancelled:cancelled1 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~cancelled:cancelled2 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log2) () in
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  let b2 = fork_build ~sw builder context2 spec in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve resolve_cancelled1 ();
  let result1 = Eio.Promise.await_exn b1 in
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Wait))
      Wait
     |} root) log1;
  Eio.Promise.resolve resolve_cancelled2 ();
  let result2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "User 2 result" (Error `Cancelled) result2;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> using .* from cache
      %s: (run (shell Wait))
      Wait
     |} root) log2;
  let r = Eio.Promise.await r in
  let r = Result.map (fun () -> "-") r in
  Alcotest.(check build_result) "Build cancelled" (Error `Cancelled) r

(* One user cancels a failed build after its replacement has started. *)
let test_cancel_4 () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let cancelled1, _resolve_cancelled1 = Eio.Promise.create () in
  let cancelled2, _resolve_cancelled2 = Eio.Promise.create () in
  let context1 = Context.v ~cancelled:cancelled1 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~cancelled:cancelled2 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log2) () in
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve set_r (Error (`Msg "Build failed"));
  (* Ensure b1's build completes and is cleaned up before starting b2.
     In Eio, unlike Lwt, promise resolution doesn't eagerly run continuations,
     so we must explicitly wait for the failed build to be fully processed. *)
  let result1 = Eio.Promise.await_exn b1 in
  Alcotest.(check build_result) "User 1 result" (Error (`Msg "Build failed")) result1;
  (* Begin a new build. *)
  let r2, set_r2 = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r2 ~cancel:set_r2 ~output:(`Constant "ok") ());
  let b2 = fork_build ~sw builder context2 spec in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  (* Start a third build. It should attach to the second build. *)
  let log3 = Log.create "b3" in
  let cancelled3, _resolve_cancelled3 = Eio.Promise.create () in
  let context3 = Context.v ~cancelled:cancelled3 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log3) () in
  let b3 = fork_build ~sw builder context3 spec in
  Log.await log3 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve set_r2 (Ok ());
  let result2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result2;
  let result3 = Eio.Promise.await_exn b3 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "User 3 result" (Ok "ok") result3

(* Start a new build while the previous one is cancelling. *)
let test_cancel_5 () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Eio.Promise.create () in
  let delay_store, set_delay = Eio.Promise.create () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~delay_store ());
  let log1 = Log.create "b1" in
  let cancelled1, resolve_cancelled1 = Eio.Promise.create () in
  let context1 = Context.v ~cancelled:cancelled1 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log1) () in
  Eio.Switch.run @@ fun sw ->
  let b1 = fork_build ~sw builder context1 spec in
  Log.await log1 (sprintf "(from base)\n%s: (run (shell Wait))\nWait\n" root);
  Eio.Promise.resolve resolve_cancelled1 ();
  let result1 = Eio.Promise.await_exn b1 in
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  (* Begin a new build. *)
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "ok") ());
  let log2 = Log.create "b2" in
  let cancelled2, _resolve_cancelled2 = Eio.Promise.create () in
  let context2 = Context.v ~cancelled:cancelled2 ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log2) () in
  let b2 = fork_build ~sw builder context2 spec in
  Log.await log2 (sprintf "(from base)\n%s: (run (shell Wait))\n" root);
  Eio.Promise.resolve set_delay ();
  let result2 = Eio.Promise.await_exn b2 |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result2

let test_delete () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "B") ());
  let log1 = Log.create "b1" in
  let context1 = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log1) () in
  let result1 = B.build builder context1 spec |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "Build 1 result" (Ok "B") result1;
  (* Remove A *)
  let id = Mock_store.find ~output:"A" store in
  let id = Option.get id in
  let log = ref [] in
  B.delete ~log:(fun x -> log := x :: !log) builder id;
  Alcotest.(check int) "Deleted 2 items" 2 (List.length !log);
  (* Check rebuild works *)
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "B") ());
  let log2 = Log.create "b2" in
  let context2 = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log2) () in
  let result2 = B.build builder context2 spec |> fun r -> Result.bind r (get store "output") in
  Alcotest.(check build_result) "Build 2 result" (Ok "B") result2

let test_tar_long_filename () =
  let do_test length =
    Logs.info (fun f -> f "Test copy length %d " length);
    let src_dir = Filename.temp_dir "test-copy-src-" "" in
    let dst_dir = Filename.temp_dir "test-copy-dst-" "" in
    Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s %s" (Filename.quote src_dir) (Filename.quote dst_dir)))
    ) @@ fun () ->
    let filename = src_dir / String.make length 'a' in
    Logs.info (fun f -> f "length: %d %s" (String.length filename) filename);
    Out_channel.with_open_bin filename (fun ch -> Out_channel.output_string ch "file-data");
    with_file (dst_dir / "out.tar") Unix.[O_WRONLY; O_CREAT; O_CLOEXEC] 0
    @@ fun to_untar ->
    let src_manifest = Manifest.generate ~exclude:[] ~src_dir "." |> Result.get_ok in
    let user = Spec.(`Unix { uid=1000; gid=1000 }) in
    Tar_transfer.send_file
      ~src_dir
      ~src_manifest
      ~dst:dst_dir
      ~user
      ~to_untar
  in
  do_test 80;
  do_test 160;
  (* Maximum path length on Windows is 260 characters. *)
  do_test (260 - 1 (* NUL *) - String.length {|C:\cygwin64\tmp\build_123456_dune\test-copy-src-123456\|})

let sexp = Alcotest.of_pp Sexplib.Sexp.pp_hum

let remove_line_indents = function
  | (_ :: x :: _) as lines ->
    let indent = Astring.String.find ((<>) ' ') x |> Option.value ~default:0 in
    lines |> List.map (fun line ->
        Astring.String.drop line ~sat:((=) ' ') ~max:indent
      )
  | x -> List.map String.trim x

let remove_indent s =
  String.split_on_char '\n' s
  |> remove_line_indents
  |> String.concat "\n"


(* Check that parsing an S-expression and then serialising it again gets the same result. *)
let test_sexp () =
  let test name s =
    let s = String.trim (remove_indent s) in
    let s1 = Sexplib.Sexp.of_string s in
    let spec = Spec.t_of_sexp s1 in
    let s2 = Spec.sexp_of_t spec in
    Alcotest.(check sexp) name s1 s2;
    Alcotest.(check string) name s (Fmt.str "%a" Spec.pp spec)
  in
  test "copy" {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "a command"))
      (run (cache (a (target /data)) (b (target /srv)))
           (secrets (a (target /run/secrets/a)) (b (target /b)))
           (shell "a very very very very very very very very very very very very very very very long command"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
      (env DEBUG 1)
      (user (uid 1) (gid 2))
     )|}

let test_docker_unix () =
  let test ~buildkit name expect sexp =
    let spec = Spec.t_of_sexp (Sexplib.Sexp.of_string sexp) in
    let got = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit ~os:`Unix spec in
    let expect = remove_indent expect in
    Alcotest.(check string) name expect got
  in
  test ~buildkit:false "Dockerfile"
    {| FROM base
       # A test comment
       WORKDIR /src
       RUN command1
       SHELL [ "/bin/sh", "-c" ]
       RUN command2 && \
           command3
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       ENV ESCAPE="\"quote\\sla\\sh\""
       USER 1:2
       COPY --chown=1:2 a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "command1"))
      (shell /bin/sh -c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2 &&
               command3"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (env ESCAPE "\"quote\\sla\\sh\"")
      (user (uid 1) (gid 2))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:true "BuildKit"
    {| FROM base
       # A test comment
       WORKDIR /src
       RUN command1
       SHELL [ "/bin/sh", "-c" ]
       RUN --mount=type=cache,id=a,target=/data,uid=0 --mount=type=cache,id=b,target=/srv,uid=0 command2
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       USER 1:2
       COPY --chown=1:2 a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "command1"))
      (shell /bin/sh -c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (user (uid 1) (gid 2))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:false "Multi-stage"
    {| FROM base as tools
       RUN make tools

       FROM base
       COPY --from=tools binary /usr/local/bin/
    |} {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
     ) |};
  test ~buildkit:true "Secrets"
    {| FROM base as tools
       RUN make tools

       FROM base
       RUN --mount=type=secret,id=a,target=/secrets/a,uid=0 --mount=type=secret,id=b,target=/secrets/b,uid=0 command1
    |} {|
     ((build tools
            ((from base)
             (run (shell "make tools"))))
      (from base)
      (run
       (secrets (a (target /secrets/a))
                (b (target /secrets/b)))
       (shell "command1"))
     ) |}

let test_docker_windows () =
  let test ~buildkit name expect sexp =
    let spec = Spec.t_of_sexp (Sexplib.Sexp.of_string sexp) in
    let got = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit ~os:`Windows spec in
    let expect = remove_indent expect in
    Alcotest.(check string) name expect got
  in
  test ~buildkit:false "Dockerfile"
    {| #escape=`
       FROM base
       # A test comment
       WORKDIR C:/src
       RUN command1
       SHELL [ "C:/Windows/System32/cmd.exe", "/c" ]
       RUN command2 && `
           command3
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       USER Zaphod
       COPY a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir C:/src)
      (run (shell "command1"))
      (shell C:/Windows/System32/cmd.exe /c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2 &&
               command3"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (user (name Zaphod))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:false "Multi-stage"
    {| #escape=`
       FROM base as tools
       RUN make tools

       FROM base
       COPY --from=tools binary /usr/local/bin/
    |} {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
     ) |}

let manifest =
  Alcotest.result
    (Alcotest.testable
       (fun f x -> Sexplib.Sexp.pp_mach f (Manifest.sexp_of_t x))
       (fun a b -> Manifest.sexp_of_t a = Manifest.sexp_of_t b))
    (Alcotest.of_pp (fun f (`Msg m) -> Fmt.string f m))

(* Test copy step. *)
let test_copy generate =
  let src_dir = Filename.temp_dir "test-copy-src-" "" in
  Fun.protect ~finally:(fun () ->
    ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote src_dir)))
  ) @@ fun () ->
  Out_channel.with_open_bin (src_dir / "file") (fun ch -> Out_channel.output_string ch "file-data");
  let root = if Sys.unix then "/root" else "C:/Windows" in
  (* Files *)
  let f1hash = Sha256.string "file-data" in
  let r = generate ~exclude:[] ~src_dir "file" in
  Alcotest.(check manifest) "File" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "./file" in
  Alcotest.(check manifest) "File relative" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "/file" in
  Alcotest.(check manifest) "File absolute" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "file2" in
  Alcotest.(check manifest) "Missing" (Error (`Msg {|Source path "file2" not found|})) r;
  let r = generate ~exclude:[] ~src_dir "file/file2" in
  Alcotest.(check manifest) "Not dir" (Error (`Msg {|Not a directory: file (in "file/file2")|})) r;
  let r = generate ~exclude:[] ~src_dir "../file" in
  Alcotest.(check manifest) "Parent" (Error (`Msg {|Can't use .. in source paths! (in "../file")|})) r;
  (* Symlinks *)
  Unix.symlink ~to_dir:true root (src_dir / "link");
  let r = generate ~exclude:[] ~src_dir "link" in
  Alcotest.(check manifest) "Link" (Ok (`Symlink (("link", root)))) r;
  let r = generate ~exclude:[] ~src_dir "link/file" in
  Alcotest.(check manifest) "Follow link" (Error (`Msg {|Not a regular file: link (in "link/file")|})) r;
  (* Directories *)
  let r = generate ~exclude:["file"] ~src_dir "" in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`Symlink ("link", root)]))) r;
  let r = generate ~exclude:[] ~src_dir "." in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`File ("file", f1hash);
                    `Symlink ("link", root)]))) r;
  Unix.mkdir (src_dir / "dir1") 0o700;
  Unix.mkdir (src_dir / "dir1" / "dir2") 0o700;
  Out_channel.with_open_bin (src_dir / "dir1" / "dir2" / "file2") (fun ch -> Out_channel.output_string ch "file2");
  let f2hash = Sha256.string "file2" in
  let r = generate ~exclude:[] ~src_dir "dir1/dir2/file2" in
  Alcotest.(check manifest) "Nested file" (Ok (`File ("dir1/dir2/file2", f2hash))) r;
  let r = generate ~exclude:[] ~src_dir "dir1" in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("dir1", [`Dir ("dir1/dir2", [`File ("dir1/dir2/file2", f2hash)])]))) r

(* Test the Manifest module. *)
let test_copy_ocaml () =
  if Sys.win32 then
    Alcotest.skip ();
  test_copy (fun ~exclude ~src_dir src -> Manifest.generate ~exclude ~src_dir src)

(* Test the manifest.bash script. *)
let test_copy_bash () =
  let generate ~exclude ~src_dir src =
    let bash, src_dir =
      if Sys.win32 then
        let bash = Os.pread ["cygpath"; "-m"; "/usr/bin/bash"] in
        let src_dir_cyg = Os.pread ["cygpath"; "-m"; src_dir] in
        (String.trim bash, String.trim src_dir_cyg)
      else
        let bash = Os.pread ["which"; "bash"] in
        (String.trim bash, src_dir)
    in
    let manifest_bash =
      Printf.sprintf "exec %s %S %S %d %s %d %s"
        "./manifest.bash"
        src_dir
        "/"
        (List.length exclude)
        (String.concat " " (List.map Filename.quote exclude))
        1
        (Filename.quote src)
    in
    let argv = [ "--login"; "-c"; manifest_bash ] in
    let pp f = Os.pp_cmd f (bash, argv) in
    let (n, stdout, stderr) = Os.pread_all ~pp ~cmd:bash argv in
    if n = 0 then
      Ok (Manifest.t_of_sexp (Sexplib.Sexp.of_string stdout))
    else if n = 1 then
      Error (`Msg stderr)
    else
      Fmt.error_msg "%t failed with exit status %d" pp n
  in
  with_default_exec (fun () -> test_copy generate)

(* Test the manifest.bash module. *)
let test_copy_bash_wrapper () =
  let ch = Unix.open_process_in "uname -s" in
  let os = input_line ch in
  close_in ch;
  if os = "Darwin" then
    Alcotest.skip ();
  test_copy_bash ()

let test_cache_id () =
  let check expected id =
    Alcotest.(check string) ("ID-" ^ id) expected (Escape.cache id)
  in
  check "c-ok" "ok";
  check "c-" "";
  check "c-123" "123";
  check "c-a1" "a1";
  check "c-a%2f1" "a/1";
  check "c-.." "..";
  check "c-%2520" "%20";
  check "c-foo%3abar" "foo:bar";
  check "c-Az09-id.foo_orig" "Az09-id.foo_orig"

let test_secrets_not_provided () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/run/secrets/test" "test"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = Result.bind (B.build builder context spec) (get store "output") in
  Alcotest.(check build_result) "Final result" (Error (`Msg "Couldn't find value for requested secret 'test'")) result

let test_secrets_simple () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~shell:(Mock_sandbox.shell sandbox) ~src_dir ~log:(Log.add log) ~secrets:["test", "top secret value"; "test2", ""] () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/testsecret" "test"; Secret.v "test2"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = Result.bind (B.build builder context spec) (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check b log"
    (sprintf {| (from base)
        ;---> saved as ".*"
         %s: (run (secrets (test (target /testsecret)) (test2 (target /run/secrets/test2)))
         [ ]+(shell Append))
         Append
        ;---> saved as ".*"
       |} root)
    log

let test_exec_nul () =
  with_default_exec @@ fun () ->
  let args = ["dummy"; "stdout"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args;
  let args = ["dummy"; "stderr"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args

let test_pread_nul () =
  with_default_exec @@ fun () ->
  let expected = "the quick brown fox jumps over the lazy dog" in
  let args = ["dummy"; "stdout"] in
  let actual = Os.pread ~stderr:`Dev_null args in
  Alcotest.(check string) "stdout" actual expected

let () =
  let is_win32_gha =
    match Sys.getenv "CI", Sys.getenv "GITHUB_ACTIONS", Sys.win32 with
    | "true", "true", true -> true
    | _ | exception _ -> false in
  let needs_docker =
    let skip_if_gha name speed f =
      if is_win32_gha then Alcotest.test_case name speed (fun () -> Alcotest.skip ())
      else Alcotest.test_case name speed f
    in
    [
      "build", [
        skip_if_gha "Simple"     `Quick test_simple;
        skip_if_gha "Prune"      `Quick test_prune;
        skip_if_gha "Concurrent" `Quick test_concurrent;
        skip_if_gha "Concurrent failure" `Quick test_concurrent_failure;
        skip_if_gha "Concurrent failure 2" `Quick test_concurrent_failure_2;
        skip_if_gha "Cancel"     `Quick test_cancel;
        skip_if_gha "Cancel 2"   `Quick test_cancel_2;
        skip_if_gha "Cancel 3"   `Quick test_cancel_3;
        skip_if_gha "Cancel 4"   `Quick test_cancel_4;
        skip_if_gha "Cancel 5"   `Quick test_cancel_5;
        skip_if_gha "Delete"     `Quick test_delete;
      ];
      "secrets", [
        skip_if_gha "Simple"     `Quick test_secrets_simple;
        skip_if_gha "No secret provided" `Quick test_secrets_not_provided;
      ];
    ] in
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun () ->
  Alcotest.run "OBuilder" ([
    "spec", [
      Alcotest.test_case "Sexp"     `Quick test_sexp;
      Alcotest.test_case "Cache ID" `Quick test_cache_id;
      Alcotest.test_case "Docker Windows" `Quick test_docker_windows;
      Alcotest.test_case "Docker UNIX"    `Quick test_docker_unix;
    ];
    "tar_transfer", [
      Alcotest.test_case "Long filename"  `Quick test_tar_long_filename;
    ];
    "manifest", [
      Alcotest.test_case "Copy using manifest.bash" `Quick test_copy_bash_wrapper;
      Alcotest.test_case "Copy using Manifest" `Quick test_copy_ocaml
    ];
    "process", [
      Alcotest.test_case "Execute a process" `Quick test_exec_nul;
      Alcotest.test_case "Read stdout of a process" `Quick test_pread_nul;
    ];
  ] @ needs_docker)
