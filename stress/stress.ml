open Lwt.Infix
open Obuilder

let ( / ) = Filename.concat
let strf = Printf.sprintf

let read path =
  Lwt_io.(with_file ~mode:input) path Lwt_io.read

let write ~path data =
  Lwt_io.(with_file ~mode:output) path (fun ch -> Lwt_io.write ch data)

let assert_str expected got =
  if expected <> got then (
    Fmt.epr "Expected: %S@.Got     : %S@." expected got;
    exit 1
  )

module Sandbox = Runc_sandbox
module Fetcher = Docker

module Test(Store : S.STORE) = struct
  let assert_output expected t id =
    match Store.result t id with
    | None -> Fmt.failwith "%S not in store!" id
    | Some path ->
      let ch = open_in (path / "output") in
      let data = really_input_string ch (in_channel_length ch) in
      close_in ch;
      assert_str expected data

  let test_store t =
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
    let id = "c1" in
    (* Create a new cache *)
    Store.delete_cache t id >>= fun x ->
    assert (x = Ok ());
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
    Store.delete_cache t id >>= function
    | Ok () -> (* Btrfs allows deletion here *)
      r () >>= fun () -> (* (not saved) *)
      Store.cache ~user t id >>= fun (c, r) ->
      assert (not (Sys.file_exists (c / "data")));
      r () >>= fun () ->
      Lwt.return_unit
    | Error `Busy -> (* Zfs does not *)
      r () >>= fun () ->
      (* Now it can be deleted. *)
      Store.delete_cache t id >>= fun x ->
      assert (x = Ok ());
      Lwt.return_unit

  module Build = Builder(Store)(Sandbox)(Fetcher)

  let n_steps = 4
  let n_values = 3
  let n_jobs = 100
  let max_running = 10

  let stress_cache = "stress"

  (* A build of [n_steps] where each step appends a random number in 0..!n_values to `output` *)
  let random_build () =
    let rec aux = function
      | 0 -> []
      | i -> Random.int n_values :: aux (i - 1)
    in
    let items = aux n_steps in
    let cache = [ Spec.Cache.v stress_cache ~target:"/mnt" ] in
    let ops = items |> List.map (fun i -> Spec.run ~cache "echo -n %d >> output; echo 'added:%d'" i i) in
    let expected = items |> List.map string_of_int |> String.concat "" in
    let ops = ops @ [Spec.run {|[ `cat output` = %S ] || exit 1|} expected] in
    let check_log data =
      data |> String.split_on_char '\n' |> List.filter_map (fun line ->
          match Astring.String.cut ~sep:":" line with
          | Some ("added", x) -> Some x
          | _ -> None
        )
      |> String.concat ""
      |> fun got ->
      assert_str expected got
    in
    check_log, Spec.stage ~from:"busybox" ops

  let do_build builder =
    let src_dir = "/root" in
    let buf = Buffer.create 100 in
    let log t x =
      (* print_endline x; *)
      match t with
      | `Heading -> Buffer.add_string buf (strf "# %s\n" x)
      | `Note -> Buffer.add_string buf (strf ": %s\n" x)
      | `Output -> Buffer.add_string buf x
    in
    let ctx = Context.v ~shell:["/bin/sh"; "-c"] ~log ~src_dir () in
    let check_log, spec = random_build () in
    Build.build builder ctx spec >>= function
    | Ok _ ->
      check_log (Buffer.contents buf);
      Lwt.return_unit
    | Error (`Msg m) -> failwith m
    | Error `Cancelled -> assert false

  let stress_builds store conf =
    Sandbox.create ~state_dir:(Store.state_dir store / "runc") conf >>= fun sandbox ->
    let builder = Build.v ~store ~sandbox in
    let pending = ref n_jobs in
    let running = ref 0 in
    let cond = Lwt_condition.create () in
    let failures = ref 0 in
    let rec aux () =
      if !running = 0 && !pending = 0 then Lwt.return ()
      else if !running < max_running && !pending > 0 then (
        if !pending mod 10 = 0 then Fmt.pr "%d pending: starting new build@." !pending;
        incr running;
        decr pending;
        let th = do_build builder in
        Lwt.on_any th
          (fun () ->
             decr running;
             Lwt_condition.broadcast cond ()
          )
          (fun ex ->
             Logs.warn (fun f -> f "Build failed: %a" Fmt.exn ex);
             decr running;
             incr failures;
             Lwt_condition.broadcast cond ()
          );
        aux ()
      ) else (
        Lwt_condition.wait cond >>= aux
      )
    in
    let t0 = Unix.gettimeofday () in
    aux () >>= fun () ->
    let time = Unix.gettimeofday () -. t0 in
    Fmt.pr "Ran %d jobs (max %d at once). %d failures. Took %.1f s (%.1f jobs/s)@."
      n_jobs max_running !failures
      time (float n_jobs /. time);
    if !failures > 0 then Fmt.failwith "%d failures!" !failures
    else Lwt.return_unit

  let prune store conf =
    Sandbox.create ~state_dir:(Store.state_dir store / "runc") conf >>= fun sandbox ->
    let builder = Build.v ~store ~sandbox in
    let log id = Logs.info (fun f -> f "Deleting %S" id) in
    let end_time = Unix.(gettimeofday () +. 60.0 |> gmtime) in
    let rec aux () =
      Fmt.pr "Pruning...@.";
      Build.prune ~log builder ~before:end_time 1000 >>= function
      | 0 -> Lwt.return_unit
      | _ -> aux ()
    in
    aux ()
end

let stress spec conf =
  Lwt_main.run begin
    spec >>= fun (Store_spec.Store ((module Store), store)) ->
    let module T = Test(Store) in
    T.test_store store >>= fun () ->
    T.test_cache store >>= fun () ->
    T.stress_builds store conf >>= fun () ->
    T.prune store conf
  end

open Cmdliner

let cmd =
  let doc = "Run stress tests." in
  let info = Cmd.info ~doc "stress" in
  Cmd.v info
    Term.(const stress $ Store_spec.cmdliner $ Sandbox.cmdliner)


let () =
  (* Logs.(set_level (Some Info)); *)
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ Logs.format_reporter ();
  exit @@ Cmd.eval cmd
