open Eio

module Os = Obuilder.Os

let ( / ) = Eio.Path.( / )

type t = {
  dir : Eio.Fs.dir Eio.Path.t;
  cond : Condition.t;
  process : Process.mgr;
  mutable builds : int;
}

let delay_store = ref (Promise.create_resolved ())

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let rm_r path =
  let rm = Unix.create_process "rm" [| "rm"; "-r"; "--"; snd path |] Unix.stdin Unix.stdout Unix.stderr in
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
       let tmp_dir = 
        let cap, path = dir in
        cap, path ^ "-tmp" 
       in
       assert (not (Os.exists tmp_dir));
       begin match base with
         | None -> Os.ensure_dir tmp_dir
         | Some base ->
           match Process.(await @@ spawn ~sw t.process ~executable:"cp" [ "cp"; "-r"; snd (t.dir / base); snd tmp_dir ]) with
           | `Exited 0 -> ()
           | _ -> failwith "cp failed!"
       end;
       (* ignore (Sys.readdir (tmp_dir / ".." / "..") |> Array.to_list |> String.concat "-" |> failwith); *)
       let r = fn tmp_dir in
       Promise.await !delay_store;
       match r with
       | Ok () ->
         Path.rename tmp_dir dir;
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
    Condition.await_no_mutex t.cond;
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
  let tmpdir = dir / tmpdir in
  Path.(mkdir ~perm:0o700 tmpdir);
  let t = { dir = tmpdir; process; cond = Condition.create (); builds = 0 } in
  Obuilder.Os.ensure_dir (state_dir t);
  Fun.protect
    (fun () -> fn t)
    ~finally:(fun () -> finish t)

let delete t id =
  match result t id with
  | Some path -> rm_r path
  | None -> ()

let find ~output t =
  let rec aux = function
    | [] -> None
    | x :: xs ->
      let output_path = t.dir / x / "rootfs" / "output" in
      if Os.exists output_path then (
        let data = Path.(load output_path) in 
        if data = output then Some x
        else aux xs
      ) else aux xs
  in
  let items = Path.read_dir t.dir |> List.sort String.compare in
  aux items

let cache ~user:_ _t _ = assert false

let delete_cache _t _ = assert false

let complete_deletes _t = ()
