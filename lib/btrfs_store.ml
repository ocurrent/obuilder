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

let build t ?base ~id fn =
  let result = path t id in
  match Os.check_dir result with
  | `Present ->
    Fmt.pr "%a@." (Fmt.styled (`Fg (`Yellow)) (Fmt.fmt "---> using cached result %S")) result;
    Lwt_result.return ()
  | `Missing ->
    let result_tmp = result ^ ".part" in
    delete_snapshot_if_exists result_tmp >>= fun () ->
    begin match base with
      | None -> Os.exec ["btrfs"; "subvolume"; "create"; "--"; result_tmp]
      | Some base ->
        Os.exec ["btrfs"; "subvolume"; "snapshot"; path t base; result_tmp]
    end
    >>= fun () ->
    fn result_tmp >>!= fun () ->
    (* delete_snapshot_if_exists result >>= fun () -> *) (* XXX: just for testing *)
    Os.exec ["btrfs"; "subvolume"; "snapshot"; "-r"; result_tmp; result] >>= fun () ->
    Os.exec ["sudo"; "btrfs"; "subvolume"; "delete"; result_tmp] >>= fun () ->
    Lwt_result.return ()

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None
