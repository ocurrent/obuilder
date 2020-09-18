open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

type t = {
  root : string;
}

let ( / ) = Filename.concat

let create root =
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "state");
  { root }

let delete_snapshot_if_exists path =
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> Os.exec ["sudo"; "btrfs"; "subvolume"; "delete"; path]

let path t id =
  t.root / "result" / id

let state_dir t =
  t.root / "state"

let build t ?base ~id ~log fn =
  let result = path t id in
  match Os.check_dir result with
  | `Present ->
    Fmt.pr "%a@." (Fmt.styled (`Fg (`Yellow)) (Fmt.fmt "---> using cached result %S")) result;
    let log_file = result / "log" in
    if Sys.file_exists log_file then Os.cat_file log_file ~dst:log;
    Lwt_result.return ()
  | `Missing ->
    let result_tmp = result ^ ".part" in
    delete_snapshot_if_exists result_tmp >>= fun () ->
    begin match base with
      | None -> Os.exec ["btrfs"; "subvolume"; "create"; "--"; result_tmp]
      | Some base ->
        Os.exec ["btrfs"; "subvolume"; "snapshot"; path t base; result_tmp] >|= fun () ->
        let log_file = result_tmp / "log" in
        if Sys.file_exists log_file then Unix.unlink log_file
    end
    >>= fun () ->
    Build_log.with_log (result_tmp / "log") (fun log -> fn ~log result_tmp) >>!= fun () ->
    (* delete_snapshot_if_exists result >>= fun () -> *) (* XXX: just for testing *)
    Os.exec ["btrfs"; "subvolume"; "snapshot"; "-r"; result_tmp; result] >>= fun () ->
    Os.exec ["sudo"; "btrfs"; "subvolume"; "delete"; result_tmp] >>= fun () ->
    Lwt_result.return ()
