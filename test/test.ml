open Lwt.Infix

module Builder = Obuilder.Builder(Mock_store)(Mock_sandbox)
module Os = Obuilder.Os

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let () =
  Os.lwt_process_exec := Mock_exec.exec

let build_result =
  Alcotest.of_pp @@ fun f x ->
  match x with
  | Error (`Exn ex) -> Fmt.exn f ex
  | Ok id -> Fmt.string f id

let get store path id =
  let result = Mock_store.path store id in
  Lwt_io.(with_file ~mode:input) (result / "rootfs" / path) Lwt_io.read >|= Result.ok

let test_simple _switch () =
  Mock_store.with_store @@ fun store ->
  let sandbox = Mock_sandbox.create (Mock_store.state_dir store / "sandbox") in
  let builder = Builder.v ~store ~sandbox in
  let src_dir = Mock_store.state_dir store / "src" in
  Obuilder.Os.ensure_dir src_dir;
  let context = Obuilder.Context.v ~src_dir () in
  let spec = Obuilder.Spec.{
      from = "base";
      ops = [
        run "append-to base-id runner";
      ] 
    }
  in
  Mock_sandbox.expect sandbox (fun ?stdin:_ config dir ->
      Alcotest.(check (list string)) "Run command arguments" ["/bin/bash"; "-c"; "append-to base-id runner"] config.argv;
      let rootfs = dir / "rootfs" in
      Lwt_io.(with_file ~mode:input) (rootfs / "base-id") Lwt_io.read >>= fun orig ->
      Lwt_io.(with_file ~mode:output) (rootfs / "appended") (fun ch -> Lwt_io.write ch (orig ^ "runner"))
    );
  Builder.build builder context spec >>!= get store "appended" >>= fun result ->
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Lwt.return_unit

let sexp = Alcotest.of_pp Sexplib.Sexp.pp_hum

(* Check that parsing an S-expression and then serialising it again gets the same result. *)
let test_sexp () =
  let test name s =
    let s1 = Sexplib.Sexp.of_string s in
    let stage = Obuilder.Spec.stage_of_sexp s1 in
    let s2 = Obuilder.Spec.sexp_of_stage stage in
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
      ];
    ]
  end
