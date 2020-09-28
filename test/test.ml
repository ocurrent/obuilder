open Lwt.Infix
open Obuilder

module B = Builder(Mock_store)(Mock_sandbox)

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let () =
  Logs.(set_level ~all:true (Some Info));
  Logs.set_reporter @@ Logs.format_reporter ();
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
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  fn ~src_dir ~store ~sandbox ~builder

let mock_op ?(result=Lwt_result.return ()) ?(delay_store=Lwt.return_unit) ?cancel ?output () =
  fun ~cancelled ?stdin:_ ~log (config:Obuilder.Config.t) dir ->
  Mock_store.delay_store := delay_store;
  let cmd =
    match config.argv with
    | ["/bin/bash"; "-c"; cmd] -> cmd
    | x -> Fmt.strf "%a" Fmt.(Dump.list string) x
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
  let spec = Spec.{ from = "base"; ops = [ run "Append" ] } in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell Append))
      Append
     |} log;
  (* Check result is cached *)
  Log.clear log;
  B.build builder context spec >>!= get store "output" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check cached log"
    {|FROM base
      /: (run (shell Append))
      Append
     |} log;
  Lwt.return_unit

(* Two builds, [A;B] and [A;C] are started together. The [A] command is only run once,
   with the log visible to both while the build is still in progress. *)
let test_concurrent _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ~output:(`Constant "A") ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  Mock_sandbox.expect sandbox (mock_op ~output:`Append_cmd ());
  let b1 = B.build builder context1 spec1 in
  let b2 = B.build builder context2 spec2 in
  Log.await log1 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done (Ok ());
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "Final result" (Ok "AB") b1;
  Alcotest.(check build_result) "Final result" (Ok "AC") b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       A
       /: (run (shell B))
       B
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
       /: (run (shell A))
       A
       /: (run (shell C))
       C
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
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:a ());
  let b1 = B.build builder context1 spec1 in
  let b2 = B.build builder context2 spec2 in
  Log.await log1 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done (Error (`Msg "Mock build failure"));
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       A
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
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
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:(Lwt_result.fail (`Msg "Mock build failure")) ~delay_store:a ());
  let b1 = B.build builder context1 spec1 in
  Log.await log1 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  let b2 = B.build builder context2 spec2 in
  Log.await log2 "FROM base\n/: (run (shell A))\nA\n" >>= fun () ->
  Lwt.wakeup a_done ();
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       A
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
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
  let spec = Spec.{ from = "base"; ops = [ run "Wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let b = B.build builder context spec in
  Log.await log "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch >>= fun () ->
  b >>= fun result ->
  Alcotest.(check build_result) "Final result" (Error `Cancelled) result;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell Wait))
      Wait
     |} log;
  Lwt.return_unit

(* Two users are sharing a build. One cancels. *)
let test_cancel_2 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.{ from = "base"; ops = [ run "Wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~output:(`Constant "ok") ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  let b2 = B.build builder context2 spec in
  Log.await log1 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell Wait))
      Wait
     |} log1;
  Lwt.wakeup set_r (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "Final result" (Ok "ok") result2;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell Wait))
      Wait
     |} log2;
  Lwt.return_unit

(* Two users are sharing a build. Both cancel. *)
let test_cancel_3 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.{ from = "base"; ops = [ run "Wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  let b2 = B.build builder context2 spec in
  Log.await log1 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell Wait))
      Wait
     |} log1;
  Lwt_switch.turn_off switch2 >>= fun () ->
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Error `Cancelled) result2;
  Log.check "Check log" 
    {|FROM base
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
  let spec = Spec.{ from = "base"; ops = [ run "Wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ());
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r (Error (`Msg "Build failed"));
  (* Begin a new build. *)
  let r2, set_r2 = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r2 ~cancel:set_r2 ~output:(`Constant "ok") ());
  let b2 = B.build builder context2 spec in
  Log.await log2 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  (* Cancel the original build. *)
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error (`Msg "Build failed")) result1;
  (* Start a third build. It should attach to the second build. *)
  let log3 = Log.create "b3" in
  let switch3 = Lwt_switch.create () in
  let context3 = Context.v ~switch:switch3 ~src_dir ~log:(Log.add log3) () in
  let b3 = B.build builder context3 spec in
  Log.await log3 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r2 (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result2;
  b3 >>!= get store "output" >>= fun result3 ->
  Alcotest.(check build_result) "User 3 result" (Ok "ok") result3;
  Lwt.return_unit

(* Start a new build while the previous one is cancelling. *)
let test_cancel_5 _switch () =
  with_config @@ fun ~src_dir ~store ~sandbox ~builder ->
  let spec = Spec.{ from = "base"; ops = [ run "Wait" ] } in
  let r, set_r = Lwt.wait () in
  let delay_store, set_delay = Lwt.wait () in
  Mock_sandbox.expect sandbox (mock_op ~result:r ~cancel:set_r ~delay_store ());
  let log1 = Log.create "b1" in
  let switch1 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "FROM base\n/: (run (shell Wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  (* Begin a new build. *)
  Mock_sandbox.expect sandbox (mock_op ~output:(`Constant "ok") ());
  let log2 = Log.create "b2" in
  let switch2 = Lwt_switch.create () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b2 = B.build builder context2 spec in
  Log.await log2 "FROM base\n/: (run (shell Wait))\n" >>= fun () ->
  Lwt.wakeup set_delay ();
  b2 >>!= get store "output" >>= fun result1 ->
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result1;
  Lwt.return_unit

let sexp = Alcotest.of_pp Sexplib.Sexp.pp_hum

(* Check that parsing an S-expression and then serialising it again gets the same result. *)
let test_sexp () =
  let test name s =
    let s1 = Sexplib.Sexp.of_string s in
    let stage = Spec.stage_of_sexp s1 in
    let s2 = Spec.sexp_of_stage stage in
    Alcotest.(check sexp) name s1 s2
  in
  test "copy" {|
     ((from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "command"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (user (uid 1) (gid 2))
     ) |}

let () =
  let open Alcotest_lwt in
  Lwt_main.run begin
    run "OBuilder" [
      "spec", [
        test_case_sync "Sexp" `Quick test_sexp;
      ];
      "build", [
        test_case "Simple"     `Quick test_simple;
        test_case "Concurrent" `Quick test_concurrent;
        test_case "Concurrent failure" `Quick test_concurrent_failure;
        test_case "Concurrent failure 2" `Quick test_concurrent_failure_2;
        test_case "Cancel"     `Quick test_cancel;
        test_case "Cancel 2"   `Quick test_cancel_2;
        test_case "Cancel 3"   `Quick test_cancel_3;
        test_case "Cancel 4"   `Quick test_cancel_4;
        test_case "Cancel 5"   `Quick test_cancel_5;
      ];
    ]
  end
