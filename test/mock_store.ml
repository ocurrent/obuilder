module Os = Obuilder.Os

let ( / ) = Filename.concat

type t = {
  dir : string;
  cond : Eio.Condition.t;
  mutex : Eio.Mutex.t;
  mutable builds : int;
}

let unix_path path =
  if Sys.win32 then
    String.trim (Os.pread ["cygpath"; "-u"; path])
  else
    path

let already_resolved =
  let p, r = Eio.Promise.create () in
  Eio.Promise.resolve r ();
  p

let delay_store : unit Eio.Promise.t ref = ref already_resolved

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let rm_r path =
  let rm = Unix.create_process "rm" [| "rm"; "-r"; "--"; path |] Unix.stdin Unix.stdout Unix.stderr in
  match waitpid_non_intr rm with
  | _, Unix.WEXITED 0 -> ()
  | _ -> failwith "rm -r failed!"

let build t ?base ~id fn =
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
           let src = unix_path (t.dir / base) in
           let dst = unix_path tmp_dir in
           let cp = Unix.create_process "cp" [| "cp"; "-r"; src; dst |] Unix.stdin Unix.stdout Unix.stderr in
           begin match waitpid_non_intr cp with
             | _, Unix.WEXITED 0 -> ()
             | _ -> failwith "cp failed!"
           end
       end;
       let r = fn tmp_dir in
       Eio.Promise.await !delay_store;
       match r with
       | Ok () ->
         Unix.rename tmp_dir dir;
         Ok ()
       | Error _ as e ->
         let tmp_dir = unix_path tmp_dir in
         rm_r tmp_dir;
         e
    )
    ~finally:(fun () ->
       t.builds <- t.builds - 1;
       Eio.Condition.broadcast t.cond
    )

let state_dir t = t.dir / "state"

let path t id = t.dir / id

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

let log_file t id =
  t.dir / "logs" / (id  ^ ".log")

let rec finish t =
  if t.builds > 0 then (
    Logs.info (fun f -> f "Waiting for %d builds to finish" t.builds);
    Eio.Mutex.lock t.mutex;
    if t.builds > 0 then
      Eio.Condition.await t.cond t.mutex;
    Eio.Mutex.unlock t.mutex;
    finish t
  ) else ()

let with_store fn =
  let dir = Filename.temp_dir "mock-store-" "" in
  let t = { dir; cond = Eio.Condition.create (); mutex = Eio.Mutex.create (); builds = 0 } in
  Obuilder.Os.ensure_dir (state_dir t);
  Obuilder.Os.ensure_dir (t.dir / "logs");
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
      if Sys.file_exists output_path then (
        let data = In_channel.with_open_bin output_path In_channel.input_all in
        if data = output then Some x
        else aux xs
      ) else aux xs
  in
  let items = Sys.readdir t.dir |> Array.to_list |> List.sort String.compare in
  aux items

let cache ~user:_ _t _ = assert false

let delete_cache _t _ = assert false

let complete_deletes _t = ()

let root t = t.dir

let df _ = 100.
