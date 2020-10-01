open Lwt.Infix
open Obuilder

let ( / ) = Filename.concat

let read path =
  Lwt_io.(with_file ~mode:input) path Lwt_io.read

let write ~path data =
  Lwt_io.(with_file ~mode:output) path (fun ch -> Lwt_io.write ch data)

let assert_str expected got =
  if expected <> got then (
    Fmt.epr "Expected: %S@.Got     : %S@." expected got;
    exit 1
  )

module Test(Store : S.STORE) = struct
  let assert_output expected t id =
    match Store.result t id with
    | None -> Fmt.failwith "%S not in store!" id
    | Some path ->
      let ch = open_in (path / "output") in
      let data = really_input_string ch (in_channel_length ch) in
      close_in ch;
      assert_str expected data

  let test_build t =
    assert (Store.result t "unknown" = None);
    (* Build without a base *)
    Store.delete t "base" >>= fun () ->
    Store.build t ~id:"base" (fun tmpdir -> write ~path:(tmpdir / "output") "ok" >|= Result.ok) >>= fun r ->
    assert (r = Ok ());
    assert_output "ok" t "base";
    (* Build with a base *)
    Store.delete t "sub" >>= fun () ->
    Store.build t ~base:"base" ~id:"sub" (fun tmpdir ->
        read (tmpdir / "output") >>= fun orig ->
        write ~path:(tmpdir / "output") (orig ^ "+") >|= Result.ok
      ) >>= fun r ->
    assert (r = Ok ());
    assert_output "ok+" t "sub";
    (* Test deletion *)
    assert (Store.result t "sub" <> None);
    Store.delete t "sub" >>= fun () ->
    assert (Store.result t "sub" = None);
    (* A failing build isn't saved *)
    Store.delete t "fail" >>= fun () ->
    Store.build t ~id:"fail" (fun _tmpdir -> Lwt_result.fail `Failed) >>= fun r ->
    assert (r = Error `Failed);
    assert (Store.result t "fail" = None);
    Lwt.return_unit

  let test_cache t =
    let uid = Unix.getuid () in
    let gid = Unix.getgid () in
    let user = { Spec.uid = 123; gid = 456 } in
    let id = Spec.cache_id "c1" |> Result.get_ok in
    (* Create a new cache *)
    Store.delete_cache t id >>= fun () ->
    Store.cache ~user t id >>= fun (c, r) ->
    assert ((Unix.lstat c).Unix.st_uid = 123);
    assert ((Unix.lstat c).Unix.st_gid = 456);
    let user = { Spec.uid; gid } in
    Os.exec ["sudo"; "chown"; Printf.sprintf "%d:%d" uid gid; "--"; c] >>= fun () ->
    assert (Sys.readdir c = [| |]);
    write ~path:(c / "data") "v1" >>= fun () ->
    r () >>= fun () ->
    (* Check it worked *)
    Store.cache ~user t id >>= fun (c, r) ->
    read (c / "data") >>= fun data ->
    assert_str "v1" data;
    r () >>= fun () ->
    (* Concurrent updates *)
    Store.cache ~user t id >>= fun (c1, r1) ->
    Store.cache ~user t id >>= fun (c2, r2) ->
    write ~path:(c1 / "data") "v2a" >>= fun () ->
    write ~path:(c2 / "data") "v2b" >>= fun () ->
    r2 () >>= fun () -> (* v2b wins *)
    r1 () >>= fun () ->
    (* Check it worked *)
    Store.cache ~user t id >>= fun (c, r) ->
    read (c / "data") >>= fun data ->
    assert_str "v2b" data;
    r () >>= fun () ->
    (* Concurrent delete *)
    Store.cache ~user t id >>= fun (c, r) ->
    write ~path:(c / "data") "v3" >>= fun () ->
    Store.delete_cache t id >>= fun () ->
    r () >>= fun () -> (* (not saved) *)
    Store.cache ~user t id >>= fun (c, r) ->
    assert (not (Sys.file_exists (c / "data")));
    r () >>= fun () ->
    Lwt.return_unit
end

let stress spec =
  Lwt_main.run begin
    Store_spec.to_store spec >>= fun (Store ((module Store), store)) ->
    let module T = Test(Store) in
    T.test_build store >>= fun () ->
    T.test_cache store >>= fun () ->
    Lwt.return_unit
  end

open Cmdliner

let term_exit (x : unit Term.result) = Term.exit x

let store_t =
  Arg.conv Obuilder.Store_spec.(of_string, pp)

let store =
  Arg.required @@
  Arg.pos 0 Arg.(some store_t) None @@
  Arg.info
    ~doc:"zfs:pool or btrfs:/path for build cache"
    ~docv:"STORE"
    []

let cmd =
  let doc = "Run stress tests." in
  Term.(const stress $ store),
  Term.info "stress" ~doc

let () =
  Logs.(set_level (Some Info));
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ Logs.format_reporter ();
  term_exit @@ Term.eval cmd
