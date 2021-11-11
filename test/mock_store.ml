open Lwt.Infix

module Os = Obuilder.Os

let ( / ) = Filename.concat

type t = {
  dir : string;
  cond : unit Lwt_condition.t;
  mutable builds : int;
}

let delay_store = ref Lwt.return_unit

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
  Lwt.finalize
    (fun () ->
       base |> Option.iter (fun base -> assert (not (String.contains base '/')));
       let dir = t.dir / id in
       assert (Os.check_dir dir = `Missing);
       let tmp_dir = dir ^ "-tmp" in
       assert (not (Sys.file_exists tmp_dir));
       begin match base with
         | None -> Os.ensure_dir tmp_dir; Lwt.return_unit
         | Some base ->
           Lwt_process.exec ("", [| "cp"; "-r"; t.dir / base; tmp_dir |]) >>= function
           | Unix.WEXITED 0 -> Lwt.return_unit
           | _ -> failwith "cp failed!"
       end >>= fun () ->
       fn tmp_dir >>= fun r ->
       !delay_store >>= fun () ->
       match r with
       | Ok () ->
         Unix.rename tmp_dir dir;
         Lwt_result.return ()
       | Error _ as e ->
         rm_r tmp_dir;
         Lwt.return e
    )
    (fun () ->
       t.builds <- t.builds - 1;
       Lwt_condition.broadcast t.cond ();
       Lwt.return_unit
    )

let state_dir t = t.dir / "state"

let path t id = t.dir / id

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Lwt.return_some dir
  | `Missing -> Lwt.return_none

let log_file t id =
  Lwt.return (t.dir / "logs" / (id  ^ ".log"))

let rec finish t =
  if t.builds > 0 then (
    Logs.info (fun f -> f "Waiting for %d builds to finish" t.builds);
    Lwt_condition.wait t.cond >>= fun () ->
    finish t
  ) else Lwt.return_unit

let with_store fn =
  Lwt_io.with_temp_dir ~prefix:"mock-store-" @@ fun dir ->
  let t = { dir; cond = Lwt_condition.create (); builds = 0 } in
  Obuilder.Os.ensure_dir (state_dir t);
  Obuilder.Os.ensure_dir (t.dir / "logs");
  Lwt.finalize
    (fun () -> fn t)
    (fun () -> finish t)

let delete t id =
  result t id >>= function
  | Some path -> rm_r path; Lwt.return_unit
  | None -> Lwt.return_unit

let find ~output t =
  let rec aux = function
    | [] -> Lwt.return_none
    | x :: xs ->
      let output_path = t.dir / x / "rootfs" / "output" in
      if Sys.file_exists output_path then (
        Lwt_io.(with_file ~mode:input) output_path Lwt_io.read >>= fun data ->
        if data = output then Lwt.return_some x
        else aux xs
      ) else aux xs
  in
  let items = Sys.readdir t.dir |> Array.to_list |> List.sort String.compare in
  aux items

let cache ~user:_ _t _ = assert false

let delete_cache _t _ = assert false

let complete_deletes _t = Lwt.return_unit

let root t = t.dir
