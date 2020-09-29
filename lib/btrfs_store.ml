open Lwt.Infix

type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;            (* Version counter. *)
}

type t = {
  root : string;
  caches : (Spec.cache_id, cache) Hashtbl.t;
  mutable next : int;
}

let ( / ) = Filename.concat

let create root =
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "cache");
  Os.ensure_dir (root / "cache-tmp");
  { root; caches = Hashtbl.create 10; next = 0 }

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

let delete t id =
  delete_snapshot_if_exists (path t id)

let build t ?base ~id fn =
  let result = path t id in
  let result_tmp = result ^ ".part" in
  (* If we crashed during a previous build then we might have a left-over
     directory. Remove it. *)
  delete_snapshot_if_exists result_tmp >>= fun () ->
  begin match base with
    | None -> btrfs ["subvolume"; "create"; "--"; result_tmp]
    | Some base ->
      btrfs ["subvolume"; "snapshot"; "--"; path t base; result_tmp]
  end
  >>= fun () ->
  fn result_tmp >>= fun r ->
  begin match r with
    | Ok () -> btrfs ["subvolume"; "snapshot"; "-r"; "--"; result_tmp; result]
    | Error _ -> Lwt.return_unit
  end >>= fun () ->
  btrfs ~sudo:true ["subvolume"; "delete"; "--"; result_tmp] >>= fun () ->
  Lwt_result.return ()

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

let get_cache t (name : Spec.cache_id) =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name : (string * (unit -> unit Lwt.t)) Lwt.t =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let gen = cache.gen in
  let tmp = t.root / "cache-tmp" / string_of_int t.next in
  t.next <- t.next + 1;
  let snapshot = t.root / "cache" / (name :> string) in
  begin match Os.check_dir snapshot with
    | `Missing ->
      let { Spec.uid; gid } = user in
      btrfs ["subvolume"; "create"; "--"; snapshot] >>= fun () ->
      Os.exec ["sudo"; "chown"; Printf.sprintf "%d:%d" uid gid; snapshot]
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  delete_snapshot_if_exists tmp >>= fun () ->
  btrfs ~sudo:true ["subvolume"; "snapshot"; "--"; snapshot; tmp] >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        btrfs ~sudo:true ["subvolume"; "delete"; "--"; snapshot] >>= fun () ->
        btrfs ~sudo:true ["subvolume"; "snapshot"; "-r"; "--"; tmp; snapshot]
      ) else Lwt.return_unit
    end >>= fun () ->
    btrfs ~sudo:true ["subvolume"; "delete"; "--"; tmp]
  in
  Lwt.return (tmp, release)
