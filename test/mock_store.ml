open Eio

module Os = Obuilder.Os

let ( / ) = Filename.concat

type t = {
  dir : string;
  cond : Condition.t;
  process : Process.t;
  mutable builds : int;
}

let delay_store = ref (Promise.create_resolved ())

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let rm_r path =
  let rm = Unix.create_process "rm" [| "rm"; "-r"; "--"; path |] Unix.stdin Unix.stdout Unix.stderr in
  match waitpid_non_intr rm with
  | _, Unix.WEXITED 0 -> ()
  | _ -> failwith "rm -r failed!"

let build t ?base ~id fn =
  Switch.run @@ fun sw ->
  t.builds <- t.builds + 1;
  Fun.protect
    (fun () ->
       base |> Option.iter (fun base -> assert (not (String.contains base '/')));
       let dir = t.dir / id in
       assert (Os.check_dir dir = `Missing);
       let tmp_dir = dir ^ "-tmp" in
       assert (not (Sys.file_exists tmp_dir));
       begin match base with
         | None -> Os.ensure_dir tmp_dir
         | Some base ->
           match Process.(status @@ spawn ~sw t.process "cp" [ "cp"; "-r"; t.dir / base; tmp_dir ]) with
           | Process.Exited 0 -> ()
           | _ -> failwith "cp failed!"
       end;
       (* ignore (Sys.readdir (tmp_dir / ".." / "..") |> Array.to_list |> String.concat "-" |> failwith); *)
       let r = fn tmp_dir in
       Promise.await !delay_store;
       match r with
       | Ok () ->
         Unix.rename tmp_dir dir;
         Ok ()
       | Error _ as e ->
         rm_r tmp_dir;
         e
    )
    ~finally:(fun () ->
       t.builds <- t.builds - 1;
       Condition.broadcast t.cond
    )

let state_dir t = t.dir / "state"

let path t id = t.dir / id

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing ->
    None

let rec finish t =
  if t.builds > 0 then (
    Logs.info (fun f -> f "Waiting for %d builds to finish" t.builds);
    Condition.await t.cond;
    finish t
  )

let prng = lazy (Random.State.make_self_init ())

let temp_file_name temp_dir prefix suffix =
  let rnd = Random.State.int (Lazy.force prng) 0x1000000 in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let with_store ~dir ~process fn =
  let tmpdir =
    temp_file_name (Filename.get_temp_dir_name ()) "obuilder-runc-" ""
  in
  (try Dir.mkdir ~perm:0o700 dir tmpdir with Dir.Already_exists _ -> failwith "YIKE");
  Dir.with_open_dir dir tmpdir @@ fun _tmp ->
  let t = { dir = tmpdir; process; cond = Condition.create (); builds = 0 } in
  Obuilder.Os.ensure_dir (state_dir t);
  Fun.protect
    (fun () -> fn t)
    ~finally:(fun () -> finish t)

let delete t id =
  match result t id with
  | Some path -> rm_r path
  | None -> ()

let find ~dir ~output t =
  let rec aux = function
    | [] -> None
    | x :: xs ->
      let output_path = t.dir / x / "rootfs" / "output" in
      if Sys.file_exists output_path then (
        let data = Dir.load dir output_path in 
        if data = output then Some x
        else aux xs
      ) else aux xs
  in
  let items = Sys.readdir t.dir |> Array.to_list |> List.sort String.compare in
  aux items

let cache ~user:_ _t _ = assert false

let delete_cache _t _ = assert false

let complete_deletes _t = ()
