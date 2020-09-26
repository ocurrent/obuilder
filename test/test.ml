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

module Log = struct
  type t = {
    label : string;
    buf : Buffer.t;
    cond : unit Lwt_condition.t;
  }

  let create label =
    let buf = Buffer.create 1024 in
    let cond = Lwt_condition.create () in
    { label; buf; cond }

  let add t tag x =
    Logs.info (fun f -> f "%s: %S" t.label x);
    begin match tag with
      | `Heading -> Buffer.add_string t.buf (x ^ "\n")
      | `Output -> Buffer.add_string t.buf x
      | `Note -> print_endline x
    end;
    Lwt_condition.broadcast t.cond ()

  let contents t =
    Buffer.contents t.buf

  let clear t =
    Buffer.clear t.buf

  let rec await t expect =
    let got = Buffer.contents t.buf in
    if got = expect then Lwt.return_unit
    else if String.length got > String.length expect then (
        Fmt.failwith "Log expected %S but got %S" expect got
    ) else (
      let common = min (String.length expect) (String.length got) in
      if String.sub got 0 common = String.sub expect 0 common then (
        Lwt_condition.wait t.cond >>= fun () ->
        await t expect
      ) else (
        Fmt.failwith "Log expected %S but got %S" expect got
      )
    )

  let check name pattern t =
    let pattern = String.split_on_char '\n' pattern |> List.map String.trim |> String.concat "\n" in
    let re = Str.regexp pattern in
    let got = contents t in
    if not (Str.string_match re got 0) then
      Alcotest.(check string) name pattern got
end

let get store path id =
  let result = Mock_store.path store id in
  Lwt_io.(with_file ~mode:input) (result / "rootfs" / path) Lwt_io.read >|= Result.ok

let test_simple _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.{
      from = "base";
      ops = [
        run "append-to base-id runner";
      ] 
    }
  in
  Mock_sandbox.expect sandbox (fun ~cancelled:_ ?stdin:_ ~log config dir ->
      Build_log.printf log "Append@." >>= fun () ->
      Alcotest.(check (list string)) "Run command arguments" ["/bin/bash"; "-c"; "append-to base-id runner"] config.argv;
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:input) (rootfs / "base-id") Lwt_io.read >>= fun orig ->
      Lwt_io.(with_file ~mode:output) (rootfs / "appended") (fun ch -> Lwt_io.write ch (orig ^ "runner")) >>= fun () ->
      Lwt_result.return ()
    );
  B.build builder context spec >>!= get store "appended" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell "append-to base-id runner"))
      Append
     |} log;
  (* Check result is cached *)
  Log.clear log;
  B.build builder context spec >>!= get store "appended" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check cached log"
    {|FROM base
      /: (run (shell "append-to base-id runner"))
      Append
     |} log;
  Lwt.return_unit

(* Two builds, [A;B] and [A;C] are started together. The [A] command is only run once,
   with the log visible to both while the build is still in progress. *)
let test_concurrent _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Obuilder.Os.ensure_dir src_dir;
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled:_ ?stdin:_ ~log config dir ->
      Alcotest.(check (list string)) "Run A" ["/bin/bash"; "-c"; "A"] config.argv;
      Build_log.printf log "Running A@." >>= fun () ->
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch "A") >>= fun () ->
      a
    );
  let do_append ~cancelled:_ ?stdin:_ ~log config dir =
    match config.Config.argv with
    | ["/bin/bash"; "-c"; cmd] ->
      Build_log.printf log "Running %s@." cmd >>= fun () ->
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:input) (rootfs / "output") Lwt_io.read >>= fun prev ->
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch (prev ^ cmd)) >>= fun () ->
      Lwt_result.return ()
    | x -> Fmt.failwith "Unexpected command %a" Fmt.(Dump.list string) x
  in
  Mock_sandbox.expect sandbox do_append;
  Mock_sandbox.expect sandbox do_append;
  let b1 = B.build builder context1 spec1 in
  let b2 = B.build builder context2 spec2 in
  Log.await log1 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  Lwt.wakeup a_done (Ok ());
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "Final result" (Ok "AB") b1;
  Alcotest.(check build_result) "Final result" (Ok "AC") b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       Running A
       /: (run (shell B))
       Running B
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
       /: (run (shell A))
       Running A
       /: (run (shell C))
       Running C
     |}
    log2;
  Lwt.return ()

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails. *)
let test_concurrent_failure _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Obuilder.Os.ensure_dir src_dir;
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled:_ ?stdin:_ ~log config _dir ->
      Alcotest.(check (list string)) "Run A" ["/bin/bash"; "-c"; "A"] config.argv;
      Build_log.printf log "Running A@." >>= fun () ->
      a >>= fun () ->
      Lwt.fail_with "Mock build failure"
    );
  let b1 = B.build builder context1 spec1 in
  let b2 = B.build builder context2 spec2 in
  Log.await log1 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  Lwt.wakeup a_done ();
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       Running A
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
       /: (run (shell A))
       Running A
     |}
    log2;
  Lwt.return ()

(* Two builds, [A;B] and [A;C] are started together. The [A] command fails
   just as the second build is trying to open the log file. *)
let test_concurrent_failure_2 _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Obuilder.Os.ensure_dir src_dir;
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let context1 = Obuilder.Context.v ~log:(Log.add log1) ~src_dir () in
  let context2 = Obuilder.Context.v ~log:(Log.add log2) ~src_dir () in
  let spec1 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "B" ] } in
  let spec2 = Obuilder.Spec.{ from = "base"; ops = [ run "A"; run "C" ] } in
  let a, a_done = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled:_ ?stdin:_ ~log config _dir ->
      Mock_store.delay_store := a;
      Alcotest.(check (list string)) "Run A" ["/bin/bash"; "-c"; "A"] config.argv;
      Build_log.printf log "Running A@." >>= fun () ->
      Lwt.fail_with "Mock build failure"
    );
  let b1 = B.build builder context1 spec1 in
  Log.await log1 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  let b2 = B.build builder context2 spec2 in
  Log.await log2 "FROM base\n/: (run (shell A))\nRunning A\n" >>= fun () ->
  Lwt.wakeup a_done ();
  b1 >>!= get store "output" >>= fun b1 ->
  b2 >>!= get store "output" >>= fun b2 ->
  Alcotest.(check build_result) "B1 result" (Error (`Msg "Mock build failure")) b1;
  Alcotest.(check build_result) "B2 result" (Error (`Msg "Mock build failure")) b2;
  Log.check "Check AB log" 
    {| FROM base
       /: (run (shell A))
       Running A
     |}
    log1;
  Log.check "Check AC log" 
    {| FROM base
       /: (run (shell A))
       Running A
     |}
    log2;
  Lwt.return ()

let test_cancel _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  let log = Log.create "b" in
  let switch = Lwt_switch.create () in
  let context = Context.v ~switch ~src_dir ~log:(Log.add log) () in
  let spec = Spec.{ from = "base"; ops = [ run "wait" ] } in
  Mock_sandbox.expect sandbox (fun ~cancelled ?stdin:_ ~log _config _dir ->
      Build_log.printf log "Wait@." >>= fun () ->
      let r, set_r = Lwt.wait () in
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup set_r (Error `Cancelled));
      r
    );
  let b = B.build builder context spec in
  Log.await log "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch >>= fun () ->
  b >>!= get store "appended" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Error `Cancelled) result;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell wait))
      Wait
     |} log;
  Lwt.return_unit

(* Two users are sharing a build. One cancels. *)
let test_cancel_2 _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  let spec = Spec.{ from = "base"; ops = [ run "wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled ?stdin:_ ~log _config dir ->
      Build_log.printf log "Wait@." >>= fun () ->
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup set_r (Error `Cancelled));
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch "ok") >>= fun () ->
      r
    );
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  let b2 = B.build builder context2 spec in
  Log.await log1 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell wait))
      Wait
     |} log1;
  Lwt.wakeup set_r (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "Final result" (Ok "ok") result2;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell wait))
      Wait
     |} log2;
  Lwt.return_unit

(* Two users are sharing a build. Both cancel. *)
let test_cancel_3 _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  let spec = Spec.{ from = "base"; ops = [ run "wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled ?stdin:_ ~log _config _dir ->
      Build_log.printf log "Wait@." >>= fun () ->
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup set_r (Error `Cancelled));
      r
    );
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  let b2 = B.build builder context2 spec in
  Log.await log1 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Log.await log2 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error `Cancelled) result1;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell wait))
      Wait
     |} log1;
  Lwt_switch.turn_off switch2 >>= fun () ->
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Error `Cancelled) result2;
  Log.check "Check log" 
    {|FROM base
      /: (run (shell wait))
      Wait
     |} log2;
  r >>= fun r ->
  let r = Result.map (fun () -> "-") r in
  Alcotest.(check build_result) "Build cancelled" (Error `Cancelled) r;
  Lwt.return_unit

(* One user cancels a failed build after its replacement has started. *)
let test_cancel_4 _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = B.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  let spec = Spec.{ from = "base"; ops = [ run "wait" ] } in
  let r, set_r = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled ?stdin:_ ~log _config _dir ->
      Build_log.printf log "Wait@." >>= fun () ->
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup set_r (Error `Cancelled));
      r
    );
  let log1 = Log.create "b1" in
  let log2 = Log.create "b2" in
  let switch1 = Lwt_switch.create () in
  let switch2 = Lwt_switch.create () in
  let context1 = Context.v ~switch:switch1 ~src_dir ~log:(Log.add log1) () in
  let context2 = Context.v ~switch:switch2 ~src_dir ~log:(Log.add log2) () in
  let b1 = B.build builder context1 spec in
  Log.await log1 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r (Error (`Msg "Build failed"));
  (* Begin a new build. *)
  let r2, set_r2 = Lwt.wait () in
  Mock_sandbox.expect sandbox (fun ~cancelled ?stdin:_ ~log _config dir ->
      Build_log.printf log "Wait@." >>= fun () ->
      Lwt.on_termination cancelled (fun () -> Lwt.wakeup set_r (Error `Cancelled));
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:output) (rootfs / "output") (fun ch -> Lwt_io.write ch "ok") >>= fun () ->
      r2
    );
  let b2 = B.build builder context2 spec in
  Log.await log2 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  (* Cancel the original build. *)
  Lwt_switch.turn_off switch1 >>= fun () ->
  b1 >>= fun result1 ->
  Alcotest.(check build_result) "User 1 result" (Error (`Msg "Build failed")) result1;
  (* Start a third build. It should attach to the second build. *)
  let log3 = Log.create "b3" in
  let switch3 = Lwt_switch.create () in
  let context3 = Context.v ~switch:switch3 ~src_dir ~log:(Log.add log3) () in
  let b3 = B.build builder context3 spec in
  Log.await log3 "FROM base\n/: (run (shell wait))\nWait\n" >>= fun () ->
  Lwt.wakeup set_r2 (Ok ());
  b2 >>!= get store "output" >>= fun result2 ->
  Alcotest.(check build_result) "User 2 result" (Ok "ok") result2;
  b3 >>!= get store "output" >>= fun result3 ->
  Alcotest.(check build_result) "User 3 result" (Ok "ok") result3;
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
      ];
    ]
  end
