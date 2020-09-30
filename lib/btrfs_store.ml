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

let btrfs ?(sudo=false) args =
  let args = "btrfs" :: args in
  let args = if sudo then "sudo" :: args else args in
  Os.exec ~stdout:`Dev_null args

let delete_snapshot_if_exists path =
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> btrfs ~sudo:true ["subvolume"; "delete"; path]

let path t id =
  t.root / "result" / id

let state_dir t =
  t.root / "state"

let build t ?base ~id fn =
  let result = path t id in
  let result_tmp = result ^ ".part" in
  delete_snapshot_if_exists result_tmp >>= fun () ->
  begin match base with
    | None -> btrfs ["subvolume"; "create"; "--"; result_tmp]
    | Some base ->
      btrfs ["subvolume"; "snapshot"; "--"; path t base; result_tmp]
  end
  >>= fun () ->
  fn result_tmp >>!= fun () ->
  (* delete_snapshot_if_exists result >>= fun () -> *) (* XXX: just for testing *)
  btrfs ["subvolume"; "snapshot"; "-r"; "--"; result_tmp; result] >>= fun () ->
  btrfs ~sudo:true ["subvolume"; "delete"; "--"; result_tmp] >>= fun () ->
  Lwt_result.return ()

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None
