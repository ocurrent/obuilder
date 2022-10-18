open Lwt.Infix
open Obuilder

module B = Builder(Mock_store)(Mock_sandbox)(Docker)

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind
let sprintf = Printf.sprintf

let () =
  Logs.(set_level ~all:true (Some Info));
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Os.lwt_process_exec := Mock_exec.exec

let build_result =
  Alcotest.of_pp @@ fun f x ->
  match x with
  | Error (`Msg msg) -> Fmt.string f msg
  | Error `Cancelled -> Fmt.string f "Cancelled"
  | Ok id -> Fmt.string f id

let get store path id =
  let result = Mock_store.path store id in
  Lwt_io.(with_file ~mode:input) (result / "rootfs" / path) Lwt_io.read >|= Result.ok

let with_config fn =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create () in
  let builder = B.v ~store ~sandbox in
  Fun.flip Lwt.finalize (fun () -> B.finish builder) @@ fun () ->
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  fn ~src_dir ~store ~sandbox ~builder

let mock_op ?(result=Lwt_result.return ()) ?(delay_store=Lwt.return_unit) ?cancel ?output () =
  fun ~cancelled ?stdin:_ ~log (config:Obuilder.Config.t) dir ->
  Mock_store.delay_store := delay_store;
  let cmd =
    match config.argv with
    | ["/bin/bash"; "-c"; cmd] -> cmd
    | x -> Fmt.str "%a" Fmt.(Dump.list string) x
  in
  Build_log.printf log "%s@." cmd >>= fun () ->
  cancel |> Option.iter (fun cancel ->
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup cancel (Error `Cancelled))
    );
  let rootfs = dir / "rootfs" in
  begin match output with
    | Some (`Constant v) -> Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch v)
    | Some (`Append (v, src)) ->
      Lwt_io.(with_file ~mode:input) (rootfs / src) Lwt_io.read >>= fun src ->
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch (src ^ v))
    | Some `Append_cmd ->
      Lwt_io.(with_file ~mode:input) (rootfs / "output") Lwt_io.read >>= fun src ->
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch (src ^ cmd))
    | None -> Lwt.return_unit
  end >>= fun () ->
  result

let test_simple _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log"
    {|(from base)
      ;---> saved as .*
      /: (run (shell Append))
      Append
      ;---> saved as .*
     |} log;
  (* Check result is cached *)
  Log.clear log;
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result cached" (Ok "base-distro\nrunner") result;
  Log.check "Check cached log"
    {|(from base)
      ;---> using .* from cache
      /: (run (shell Append))
      Append
      ;---> using .* from cache
     |} log;
  Lwt.return_unit

let test_prune _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let start = Unix.(gettimeofday () |> gmtime) in
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log"
    {|(from base)
      ;---> saved as .*
      /: (run (shell Append))
      Append
      ;---> saved as .*
     |} log;
  let log id = Logs.info (fun f -> f "Deleting %S" id) in
  B.prune ~log builder ~before:start 10 >>= fun n ->
  Alcotest.(check int) "Nothing before start time" 0 n;
  let end_time = Unix.(gettimeofday () +. 60.0 |> gmtime) in
  B.prune ~log builder ~before:end_time 10 >>= fun n ->
  Alcotest.(check int) "Prune" 2 n;
  Lwt.return_unit

(* Two builds, [A;B] and [A;C] are started together. The [A] command is only run once,
   with the log visible to both while the build is still in progress. *)
let test_concurrent _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base"[ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base"[ run "A"; run "C" ]) in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  let b1 = B.build builder context1 spec1 in
  Log.await log1 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  let b2 = B.build builder context2 spec2 in
  Log.await log2 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done (Ok ());
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "Final result" (Ok "AB") b1;
  Alcotest.(check build_result) "Final result" (Ok "AC") b2;
  Log.check "Check AB log"
    {| (from base)
      ;---> saved as .*
       /: (run (shell A))
       A
      ;---> saved as .*
       /: (run (shell B))
       B
      ;---> saved as .*
     |}
    log1;
  Log.check "Check AC log"
    {| (from base)
      ;---> using .* from cache
       /: (run (shell A))
       A
      ;---> saved as .*
       /: (run (shell C))
       C
      ;---> saved as .*
     |}
    log2;
  Lwt.return ()

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails. *)
let test_concurrent_failure _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "C" ]) in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ());
  let b1 = B.build builder context1 spec1 in
  Log.await log1 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  let b2 = B.build builder context2 spec2 in
  Log.await log2 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done (Error (`Msg "Mock build failure"));
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log"
    {| (from base)
      ;---> saved as .*
       /: (run (shell A))
       A
     |}
    log1;
  Log.check "Check AC log"
    {| (from base)
      ;---> using .* from cache
       /: (run (shell A))
       A
     |}
    log2;
  Lwt.return ()

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails
   just as the second build is trying to open the log file. *)
let test_concurrent_failure_2 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  let spec2 = Obuilder.Spec.(stage ~from:"base" [ run "A"; run "C" ]) in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:(Lwt_result.fail (`Msg "Mock build failure")) ~delay_store:a ());
  let b1 = B.build builder context1 spec1 in
  Log.await log1 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  let b2 = B.build builder context2 spec2 in
  Log.await log2 "(from base)\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done ();
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log"
    {| (from base)
      ;---> saved as .*
       /: (run (shell A))
       A
     |}
    log1;
  Log.check "Check AC log"
    {| (from base)
      ;---> using .* from cache
       /: (run (shell A))
       A
     |}
    log2;
  Lwt.return ()

let test_cancel _switch () =
  with_config @@ fun ~src_dir ~store:_ ~sandbox ~builder ->
  let log = Log.create "b" in
  let switch = Lwt_switch.create () in
  let context = Context.v ~switch ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let b = B.build builder context spec in
  Log.await log "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch >>= fun () ->
  b >>= fun result ->
  Alcotest.(check build_result) "Final result" (Error `Cancelled) result;
  Log.check "Check log"
    {|(from base)
      ;---> saved as .*
      /: (run (shell Wait))
      Wait
     |} log;
  Lwt.return_unit

(* Two users are sharing a build. One cancels. *)
let test_cancel_2 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~output:(`Constant "ok") ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  let b2 = B.build builder context2 spec in
  Log.await log2 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log"
    {|(from base)
      ;---> saved as .*
      /: (run (shell Wait))
      Wait
     |} log1;
  Lwt.wakeup set_r (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "Final result" (Ok "ok") result2;
  Log.check "Check log"
    {|(from base)
      ;---> using .* from cache
      /: (run (shell Wait))
      Wait
      ;---> saved as .*
     |} log2;
  Lwt.return_unit

(* Two users are sharing a build. Both cancel. *)
let test_cancel_3 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  let b2 = B.build builder context2 spec in
  Log.await log2 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log"
    {|(from base)
      ;---> saved as .*
      /: (run (shell Wait))
      Wait
     |} log1;
  Lwt_switch.turn_off switch2 >>= fun () ->
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Error `Cancelled) result2;
  Log.check "Check log"
    {|(from base)
      ;---> using .* from cache
      /: (run (shell Wait))
      Wait
     |} log2;
  r >>= fun r ->
  let r = Result.map (fun () -> "-") r in
  Alcotest.(check build_result) "Build cancelled" (Error `Cancelled) r;
  Lwt.return_unit

(* One user cancels a failed build after its replacement has started. *)
let test_cancel_4 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r (Error (`Msg "Build failed"));
  (* Begin a new build. *)
  let r2, set_r2 = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r2 ~cancel:set_r2 ~output:(`Constant "ok") ());
  let b2 = B.build builder context2 spec in
  Log.await log2 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  (* Cancel the original build. *)
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error (`Msg "Build failed")) result1;
  (* Start a third build. It should attach to the second build. *)
  let log3 = Log.create "b3" in
  let switch3 = Lwt_switch.create () in
  let context3 = Context.v ~switch:switch3 ~src_dir ~log:(Log.add log3) () in
  let b3 = B.build builder context3 spec in
  Log.await log3 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r2 (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result2;
  b3 >>!= get store "output" >>= fun result3 ->
  Alcotest.(check build_result) "User 3 result" (Ok "ok") result3;
  Lwt.return_unit

(* Start a new build while the previous one is cancelling. *)
let test_cancel_5 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "Wait" ]) in
  let r, set_r = Lwt.wait () in
  let delay_store, set_delay = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~delay_store ());
  let log1 = Log.create "b1" in
  let switch1 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "(from base)\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  (* Begin a new build. *)
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "ok") ());
  let log2 = Log.create "b2" in
  let switch2 = Lwt_switch.create () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b2 = B.build builder context2 spec in
  Log.await log2 "(from base)\n/: (run (shell Wait))\n" >>= fun () ->
  Lwt.wakeup set_delay ();
  b2 >>!= get store "output" >>= fun result1 ->
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result1;
  Lwt.return_unit

let test_delete _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.(stage ~from:"base" [ run "A"; run "B" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "B") ());
  let log1 = Log.create "b1" in
  let switch1 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let b1 = B.build builder context1 spec in
  b1 >>!= get store "output" >>= fun result1 ->
  Alcotest.(check build_result) "Build 1 result" (Ok "B") result1;
  (* Remove A *)
  Mock_store.find ~output:"A" store >>= fun id ->
  let id = Option.get id in
  let log = ref [] in
  B.delete ~log:(fun x -> log := x :: !log) builder id >>= fun () ->
  Alcotest.(check int) "Deleted 2 items" 2 (List.length !log);
  (* Check rebuild works *)
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "B") ());
  let log2 = Log.create "b2" in
  let switch2 = Lwt_switch.create () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b2 = B.build builder context2 spec in
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "Build 2 result" (Ok "B") result2;
  Lwt.return_unit

let test_tar_long_filename _switch () =
  let do_test length =
    Logs.info (fun m -> m "Test copy length %d " length);
    Lwt_io.with_temp_dir ~prefix:"test-copy-src-" @@ fun src_dir ->
    Lwt_io.with_temp_dir ~prefix:"test-copy-dst-" @@ fun dst_dir ->
    let filename = String.make length 'a' in
    Lwt_io.(with_file ~mode:output)
      (src_dir / filename)
      (fun ch -> Lwt_io.write ch "file-data")
    >>= fun () ->
    Lwt_unix.openfile (dst_dir / "out.tar") [Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT] 0
    >>= fun to_untar ->
    let src_manifest = Manifest.generate ~exclude:[] ~src_dir "." |> Result.get_ok in
    let user = Spec.(`Unix { uid=1000; gid=1000 }) in
    Tar_transfer.send_file
      ~src_dir
      ~src_manifest
      ~dst:dst_dir
      ~user
      ~to_untar
  in
  do_test 80 >>= fun () ->
  do_test 160 >>= fun () ->
  do_test 240

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
let test_copy _switch () =
  Lwt_io.with_temp_dir ~prefix:"test-copy-" @@ fun src_dir ->
  Lwt_io.(with_file ~mode:output) (src_dir / "file") (fun ch -> Lwt_io.write ch "file-data") >>= fun () ->
  (* Files *)
  let f1hash = Sha256.string "file-data" in
  Alcotest.(check manifest) "File" (Ok (`File ("file", f1hash)))
  @@ Manifest.generate ~exclude:[] ~src_dir "file";
  Alcotest.(check manifest) "File" (Ok (`File ("file", f1hash)))
  @@ Manifest.generate ~exclude:[] ~src_dir "./file";
  Alcotest.(check manifest) "File" (Ok (`File ("file", f1hash)))
  @@ Manifest.generate ~exclude:[] ~src_dir "/file";
  Alcotest.(check manifest) "Missing" (Error (`Msg {|Source path "file2" not found|}))
  @@ Manifest.generate ~exclude:[] ~src_dir "file2";
  Alcotest.(check manifest) "Not dir" (Error (`Msg {|Not a directory: file (in "file/file2")|}))
  @@ Manifest.generate ~exclude:[] ~src_dir "file/file2";
  Alcotest.(check manifest) "Parent" (Error (`Msg {|Can't use .. in source paths! (in "../file")|}))
  @@ Manifest.generate ~exclude:[] ~src_dir "../file";
  (* Symlinks *)
  Unix.symlink "/root" (src_dir / "link");
  Alcotest.(check manifest) "Link" (Ok (`Symlink (("link", "/root"))))
  @@ Manifest.generate ~exclude:[] ~src_dir "link";
  Alcotest.(check manifest) "Follow link" (Error (`Msg {|Not a regular file: link (in "link/file")|}))
  @@ Manifest.generate ~exclude:[] ~src_dir "link/file";
  (* Directories *)
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`Symlink ("link", "/root")])))
  @@ Manifest.generate ~exclude:["file"] ~src_dir "";
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`File ("file", f1hash);
                    `Symlink ("link", "/root")])))
  @@ Manifest.generate ~exclude:[] ~src_dir ".";
  Unix.mkdir (src_dir / "dir1") 0o700;
  Unix.mkdir (src_dir / "dir1" / "dir2") 0o700;
  Lwt_io.(with_file ~mode:output) (src_dir / "dir1" / "dir2" / "file2") (fun ch -> Lwt_io.write ch "file2") >>= fun () ->
  let f2hash = Sha256.string "file2" in
  Alcotest.(check manifest) "Nested file" (Ok (`File ("dir1/dir2/file2", f2hash)))
  @@ Manifest.generate ~exclude:[] ~src_dir "dir1/dir2/file2";
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("dir1", [`Dir ("dir1/dir2", [`File ("dir1/dir2/file2", f2hash)])])))
  @@ Manifest.generate ~exclude:[] ~src_dir "dir1";
  Lwt.return_unit

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

let test_secrets_not_provided _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/run/secrets/test" "test"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Error (`Msg "Couldn't find value for requested secret 'test'")) result;
  Lwt.return_unit

let test_secrets_simple _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) ~secrets:["test", "top secret value"; "test2", ""] () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/testsecret" "test"; Secret.v "test2"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check b log"
    {| (from base)
        ;---> saved as .*
         /: (run (secrets (test (target /testsecret)) (test2 (target /run/secrets/test2)))
         ........(shell Append))
         Append
        ;---> saved as .*
       |}
    log;
  Lwt.return_unit

let test_exec_nul _switch () =
  Os.lwt_process_exec := Os.default_exec;
  let args = ["dummy"; "stdout"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args >>= fun actual ->
  Alcotest.(check unit) "stdout" actual ();
  let args = ["dummy"; "stderr"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args >|= fun actual ->
  Alcotest.(check unit) "stderr" actual ();
  Os.lwt_process_exec := Mock_exec.exec

let test_pread_nul _switch () =
  Os.lwt_process_exec := Os.default_exec;
  let expected = "the quick brown fox jumps over the lazy dog" in
  let args = ["dummy"; "stdout"] in
  Os.pread ~stderr:`Dev_null args >|= fun actual ->
  Alcotest.(check string) "stdout" actual expected;
  Os.lwt_process_exec := Mock_exec.exec

let () =
  let open Alcotest_lwt in
  let test_case name speed f =
    let wrap switch () =
      let s = 10.0 in
      let timeout = Lwt_unix.sleep s >|= fun () ->
                    Alcotest.(check reject (sprintf "timeout %fs" s) () ()) in
      Lwt.pick ([f switch (); timeout])
    in
    test_case name speed wrap
  in
  Lwt_main.run begin
    run "OBuilder" [
      "spec", [
        test_case_sync "Sexp"     `Quick test_sexp;
        test_case_sync "Cache ID" `Quick test_cache_id;
        test_case_sync "Docker UNIX"    `Quick test_docker_unix;
        test_case_sync "Docker Windows" `Quick test_docker_windows;
      ];
      "build", [
        test_case "Simple"     `Quick test_simple;
        test_case "Prune"      `Quick test_prune;
        test_case "Concurrent" `Quick test_concurrent;
        test_case "Concurrent failure" `Quick test_concurrent_failure;
        test_case "Concurrent failure 2" `Quick test_concurrent_failure_2;
        test_case "Cancel"     `Quick test_cancel;
        test_case "Cancel 2"   `Quick test_cancel_2;
        test_case "Cancel 3"   `Quick test_cancel_3;
        test_case "Cancel 4"   `Quick test_cancel_4;
        test_case "Cancel 5"   `Quick test_cancel_5;
        test_case "Delete"     `Quick test_delete;
      ];
      "secrets", [
        test_case "Simple"     `Quick test_secrets_simple;
        test_case "No secret provided" `Quick test_secrets_not_provided;
      ];
      "tar_transfer", [
        test_case "Long filename"  `Quick test_tar_long_filename;
      ];
      "manifest", [
        test_case "Copy"       `Quick test_copy;
      ];
      "process", [
        test_case "Execute a process" `Quick test_exec_nul;
        test_case "Read stdout of a process" `Quick test_pread_nul;
      ];
    ]
  end
