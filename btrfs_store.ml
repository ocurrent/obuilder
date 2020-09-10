open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

type t = {
  root : string;
}

module ID = struct
  type t = string [@@deriving show]
  let v x = x
end

let ( / ) = Filename.concat

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let create root =
  let results = root / "result" in
  begin match check_dir results with
    | `Present -> ()
    | `Missing -> Unix.mkdir results 0o755
  end;
  { root }

let delete_snapshot_if_exists path =
  match check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> Process.exec ["sudo"; "btrfs"; "subvolume"; "delete"; path]

let path t id =
  t.root / "result" / id

let cat_file path ~dst =
  let ch = open_in path in
  let buf = Bytes.create 4096 in
  let rec aux () =
    match input ch buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n -> output dst buf 0 n; aux ()
  in
  aux ()

let build t ?base ~id ~log fn =
  let result_id = ID.v id in
  let result = path t result_id in
  match check_dir result with
  | `Present ->
    Fmt.pr "---> using cached result %S@." result;
    let log_file = result / "log" in
    if Sys.file_exists log_file then cat_file log_file ~dst:log;
    Lwt_result.return result_id
  | `Missing ->
    let result_tmp = result ^ ".part" in
    delete_snapshot_if_exists result_tmp >>= fun () ->
    begin match base with
      | None -> Process.exec ["btrfs"; "subvolume"; "create"; "--"; result_tmp]
      | Some base -> Process.exec ["btrfs"; "subvolume"; "snapshot"; path t base; result_tmp]
    end
    >>= fun () ->
    fn result_tmp >>!= fun () ->
    (* delete_snapshot_if_exists result >>= fun () -> *) (* XXX: just for testing *)
    Process.exec ["btrfs"; "subvolume"; "snapshot"; "-r"; result_tmp; result] >>= fun () ->
    Process.exec ["sudo"; "btrfs"; "subvolume"; "delete"; result_tmp] >>= fun () ->
    Lwt_result.return result_id
