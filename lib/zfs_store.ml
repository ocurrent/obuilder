open Lwt.Infix

let strf = Printf.sprintf

let ( >>!= ) = Lwt_result.bind

type t = {
  pool : string;
}

let path t id =
  strf "/%s/%s/.zfs/snapshot/snap" t.pool id

let ( / ) = Filename.concat

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let state_dir t = strf "/%s/state" t.pool

let create ~pool =
  let t = { pool } in
  let state = state_dir t in
  begin match Os.check_dir state with
    | `Present -> Lwt.return_unit
    | `Missing ->
      let ds = strf "%s/state" t.pool in
      Os.exec ["sudo"; "zfs"; "create"; "--"; ds] >>= fun () ->
      Os.exec ["sudo"; "chown"; string_of_int (Unix.getuid ()); "/" ^ ds]
  end >|= fun () ->
  t

let delete_clone_if_exists ~pool id =
  match check_dir (strf "/%s/%s" pool id) with
  | `Missing -> Lwt.return_unit
  | `Present -> Os.exec ["sudo"; "zfs"; "destroy"; strf "%s/%s" pool id]

let build t ?base ~id ~log fn =
  let result = path t id in
  match check_dir result with
  | `Present ->
    Fmt.pr "%a@." (Fmt.styled (`Fg (`Yellow)) (Fmt.fmt "---> using cached result %S")) result;
    let log_file = result / "log" in
    if Sys.file_exists log_file then Os.cat_file log_file ~dst:log;
    Lwt_result.return ()
  | `Missing ->
    delete_clone_if_exists ~pool:t.pool id >>= fun () ->
    let clone = strf "/%s/%s" t.pool id in
    begin match base with
      | None -> Os.exec ["sudo"; "zfs"; "create"; "--"; strf "%s/%s" t.pool id]
      | Some base ->
        let base = strf "%s/%s@snap" t.pool base in
        Os.exec ["sudo"; "zfs"; "clone"; "--"; base; strf "%s/%s" t.pool id] >|= fun () ->
        let log_file = clone / "log" in
        if Sys.file_exists log_file then Unix.unlink log_file
    end
    >>= fun () ->
    Os.exec ["sudo"; "chown"; string_of_int (Unix.getuid ()); clone] >>= fun () ->
    fn clone >>!= fun () ->
    Os.exec ["sudo"; "zfs"; "snapshot"; "--"; strf "%s/%s@snap" t.pool id] >>= fun () ->
    (* ZFS can't delete the clone while the snapshot still exists. So I guess we'll just
       keep it around? *)
    Lwt_result.return () 
