open Obuilder

let ( / ) = Filename.concat
let strf = Printf.sprintf

let read path =
  In_channel.with_open_bin path In_channel.input_all

let write ~path data =
  Out_channel.with_open_bin path (fun ch -> Out_channel.output_string ch data)

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

  let test_store t =
    let r = Store.result t "unknown" in assert (r = None);
    (* Build without a base *)
    Store.delete t "base";
    let r = Store.build t ~id:"base" (fun tmpdir -> write ~path:(tmpdir / "output") "ok"; Ok ()) in
    assert (r = Ok ());
    assert_output "ok" t "base";
    (* Build with a base *)
    Store.delete t "sub";
    let r = Store.build t ~base:"base" ~id:"sub" (fun tmpdir ->
        let orig = read (tmpdir / "output") in
        write ~path:(tmpdir / "output") (orig ^ "+"); Ok ()
      ) in
    assert (r = Ok ());
    assert_output "ok+" t "sub";
    (* Test deletion *)
    let r = Store.result t "sub" in assert (r <> None);
    Store.delete t "sub";
    let r = Store.result t "sub" in assert (r = None);
    (* A failing build isn't saved *)
    Store.delete t "fail";
    let r = Store.build t ~id:"fail" (fun _tmpdir -> Error `Failed) in
    assert (r = Error `Failed);
    let r = Store.result t "fail" in assert (r = None)

  let test_cache t =
    let uid = Unix.getuid () in
    let gid = Unix.getgid () in
    let user = `Unix { Spec.uid = 123; gid = 456 } in
    let id = "c1" in
    (* Create a new cache *)
    let x = Store.delete_cache t id in
    assert (x = Ok ());
    let (c, r) = Store.cache ~user t id in
    assert ((Unix.lstat c).Unix.st_uid = 123);
    assert ((Unix.lstat c).Unix.st_gid = 456);
    let user = `Unix { Spec.uid; gid } in
    Os.exec ["sudo"; "chown"; Printf.sprintf "%d:%d" uid gid; "--"; c];
    assert (Sys.readdir c = [| |]);
    write ~path:(c / "data") "v1";
    r ();
    (* Check it worked *)
    let (c, r) = Store.cache ~user t id in
    let data = read (c / "data") in
    assert_str "v1" data;
    r ();
    (* Concurrent updates *)
    let (c1, r1) = Store.cache ~user t id in
    let (c2, r2) = Store.cache ~user t id in
    write ~path:(c1 / "data") "v2a";
    write ~path:(c2 / "data") "v2b";
    r2 (); (* v2b wins *)
    r1 ();
    (* Check it worked *)
    let (c, r) = Store.cache ~user t id in
    let data = read (c / "data") in
    assert_str "v2b" data;
    r ();
    (* Concurrent delete *)
    let (c, r) = Store.cache ~user t id in
    write ~path:(c / "data") "v3";
    match Store.delete_cache t id with
    | Ok () -> (* Btrfs allows deletion here *)
      r (); (* (not saved) *)
      let (c, r) = Store.cache ~user t id in
      assert (not (Sys.file_exists (c / "data")));
      r ()
    | Error `Busy -> (* Zfs does not *)
      r ();
      (* Now it can be deleted. *)
      let x = Store.delete_cache t id in
      assert (x = Ok ())

  type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

  let create_builder ~sw store conf =
    let module Builder = Obuilder.Builder(Store)(Native_sandbox)(Obuilder.Docker_extract) in
    let sandbox = Native_sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf in
    let builder = Builder.v ~sw ~store ~sandbox in
    Builder ((module Builder), builder)

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

  let do_build (Builder ((module Builder), builder)) =
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
    match Builder.build builder ctx spec with
    | Ok _ ->
      check_log (Buffer.contents buf)
    | Error (`Msg m) -> failwith m
    | Error `Cancelled -> assert false

  let stress_builds ~sw store conf =
    let builder = create_builder ~sw store conf in
    let (Builder ((module Builder), _)) = builder in
    let pending = ref n_jobs in
    let running = ref 0 in
    let cond = Eio.Condition.create () in
    let mutex = Eio.Mutex.create () in
    let failures = ref 0 in
    Eio.Switch.run @@ fun sw ->
    let rec aux () =
      if !running = 0 && !pending = 0 then ()
      else if !running < max_running && !pending > 0 then (
        if !pending mod 10 = 0 then Fmt.pr "%d pending: starting new build@." !pending;
        incr running;
        decr pending;
        Eio.Fiber.fork ~sw (fun () ->
          begin
            try do_build builder
            with ex ->
              Logs.warn (fun f -> f "Build failed: %a" Fmt.exn ex);
              incr failures
          end;
          decr running;
          Eio.Condition.broadcast cond
        );
        aux ()
      ) else (
        Eio.Mutex.lock mutex;
        Eio.Condition.await cond mutex;
        Eio.Mutex.unlock mutex;
        aux ()
      )
    in
    let t0 = Unix.gettimeofday () in
    aux ();
    let time = Unix.gettimeofday () -. t0 in
    Fmt.pr "Ran %d jobs (max %d at once). %d failures. Took %.1f s (%.1f jobs/s)@."
      n_jobs max_running !failures
      time (float n_jobs /. time);
    if !failures > 0 then Fmt.failwith "%d failures!" !failures

  let prune ~sw store conf =
    let (Builder ((module Builder), builder)) = create_builder ~sw store conf in
    let log id = Logs.info (fun f -> f "Deleting %S" id) in
    let end_time = Unix.(gettimeofday () +. 60.0 |> gmtime) in
    let rec aux () =
      Fmt.pr "Pruning...@.";
      match Builder.prune ~log builder ~before:end_time 1000 with
      | 0 -> ()
      | _ -> aux ()
    in
    aux ()
end

let stress (sandbox, spec) conf =
  if sandbox = `Docker then begin
    prerr_endline "Cannot stress-test the Docker backend";
    exit 1
  end;
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let (Store_spec.Store ((module Store), store)) = spec in
  let module T = Test(Store) in
  T.test_store store;
  T.test_cache store;
  T.stress_builds ~sw store conf;
  T.prune ~sw store conf

open Cmdliner

let cmd =
  let doc = "Run stress tests." in
  let info = Cmd.info ~doc "stress" in
  Cmd.v info
    Term.(const stress $ Store_spec.cmdliner $ Native_sandbox.cmdliner)

let () =
  (* Logs.(set_level (Some Info)); *)
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ Logs.format_reporter ();
  exit @@ Cmd.eval cmd
