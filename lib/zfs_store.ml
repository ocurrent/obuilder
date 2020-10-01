open Lwt.Infix

let strf = Printf.sprintf

type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;            (* Version counter. *)
}

type t = {
  pool : string;
  caches : (Spec.cache_id, cache) Hashtbl.t;
  mutable next : int;
}

module Dataset : sig
  type dataset

  val result : S.id -> dataset
  val cache : Spec.cache_id -> dataset
  val cache_tmp : int -> Spec.cache_id -> dataset

  val full_name : t -> dataset -> string
  val path : t -> dataset -> string
end = struct
  type dataset = string

  let result id = "r-" ^ id
  let cache (name : Spec.cache_id) = "c-" ^ (name :> string)
  let cache_tmp i (name : Spec.cache_id) = strf "t-%d-%s" i (name :> string)

  let full_name t ds =
    strf "%s/%s" t.pool ds

  let path t ds =
    strf "/%s/%s" t.pool ds
end

let path t id =
  Filename.concat (Dataset.path t (Dataset.result id)) ".zfs/snapshot/snap"

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let state_dir t = strf "/%s/state" t.pool

let create ~pool =
  let t = { pool; caches = Hashtbl.create 10; next = 0 } in
  let state = state_dir t in
  begin match Os.check_dir state with
    | `Present -> Lwt.return_unit
    | `Missing ->
      let ds = strf "%s/state" t.pool in
      Os.exec ["sudo"; "zfs"; "create"; "--"; ds] >>= fun () ->
      Os.exec ["sudo"; "chown"; string_of_int (Unix.getuid ()); "/" ^ ds]
  end >|= fun () ->
  t

let delete t id =
  let ds = Dataset.result id in
  let path = Dataset.path t ds in
  match check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> Os.exec ["sudo"; "zfs"; "destroy"; "-r"; "--"; Dataset.full_name t ds]

let build t ?base ~id fn =
  let ds = Dataset.result id in
  begin match Os.check_dir (Dataset.path t ds) with
    | `Missing -> Lwt.return_unit
    | `Present ->
      (* We have to create the dataset in its final location because ZFS can't
         rename it while we have the log file open (which we need to do). But
         we don't create the snapshot unless the build succeeds. If we crash
         with a partially written directory, `result` will see there is no
         snapshot and we'll end up here and delete it. *)
      Os.exec ["sudo"; "zfs"; "destroy"; "-r"; "--"; Dataset.full_name t ds]
  end >>= fun () ->
  let clone = Dataset.path t ds in
  begin match base with
    | None -> Os.exec ["sudo"; "zfs"; "create"; "--"; Dataset.full_name t ds]
    | Some base ->
      let base = Dataset.full_name t (Dataset.result base) ^ "@snap" in
      Os.exec ["sudo"; "zfs"; "clone"; "--"; base; Dataset.full_name t ds]
  end
  >>= fun () ->
  Os.exec ["sudo"; "chown"; string_of_int (Unix.getuid ()); clone] >>= fun () ->
  fn clone >>= function
  | Ok () ->
    Os.exec ["sudo"; "zfs"; "snapshot"; "--"; Dataset.full_name t ds ^ "@snap"] >>= fun () ->
    (* ZFS can't delete the clone while the snapshot still exists. So I guess we'll just
       keep it around? *)
    Lwt_result.return ()
  | Error _ as e ->
    Os.exec ["sudo"; "zfs"; "destroy"; "--"; Dataset.full_name t ds] >>= fun () ->
    Lwt.return e

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
  let main_ds = Dataset.cache name in
  let tmp_ds = Dataset.cache_tmp t.next name in
  t.next <- t.next + 1;
  (* Create the cache as an empty directory if it doesn't exist. *)
  begin match Os.check_dir (Dataset.path t main_ds) with
    | `Missing ->
      let { Spec.uid; gid } = user in
      Os.exec ["sudo"; "zfs"; "create"; "--"; Dataset.full_name t main_ds] >>= fun () ->
      Os.exec ["sudo"; "chown"; strf "%d:%d" uid gid; Dataset.path t main_ds] >>= fun () ->
      Os.exec ["sudo"; "zfs"; "snapshot"; "--"; Dataset.full_name t main_ds ^ "@snap"]
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  (* Remove any left-over tmp dir from a previous run. *)
  begin match Os.check_dir (Dataset.path t tmp_ds) with
    | `Missing -> Lwt.return_unit
    | `Present -> Os.exec ["sudo"; "zfs"; "destroy"; "-r"; "--"; Dataset.full_name t tmp_ds]
  end >>= fun () ->
  Os.exec ["sudo"; "zfs"; "clone"; "--"; Dataset.full_name t main_ds ^ "@snap"; Dataset.full_name t tmp_ds] >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    if cache.gen = gen then (
      (* The cache hasn't changed since we cloned it. Update it. *)
      (* todo: check if it has actually changed. *)
      cache.gen <- cache.gen + 1;
      Os.exec ["sudo"; "zfs"; "promote"; Dataset.full_name t tmp_ds] >>= fun () ->
      Os.exec ["sudo"; "zfs"; "destroy"; "--"; Dataset.full_name t main_ds] >>= fun () ->
      Os.exec ["sudo"; "zfs"; "rename"; "--"; Dataset.full_name t tmp_ds; Dataset.full_name t main_ds] >>= fun () ->
      (* Remove the previous snapshot. If other clones are using it, this will defer the deletion until they're done *)
      Os.exec ["sudo"; "zfs"; "destroy"; "-d"; "--"; Dataset.full_name t main_ds ^ "@snap"] >>= fun () ->
      let main_snap = Dataset.full_name t main_ds ^ "@snap" in
      begin match Os.check_dir (strf "%s/.zfs/snapshot/snap" (Dataset.path t main_ds)) with
        | `Present -> 
          (* It's still there. Must be a deferred deletion. Move it out of the way. *)
          let archive_name = strf "%s-%d" main_snap gen in
          Os.exec ["sudo"; "zfs"; "rename"; "--"; main_snap; archive_name]
        | `Missing -> Lwt.return_unit
      end >>= fun () ->
      Os.exec ["sudo"; "zfs"; "snapshot"; "--"; main_snap]
    ) else (
      Os.exec ["sudo"; "zfs"; "destroy"; "--"; Dataset.full_name t tmp_ds]
    )
  in
  Lwt.return (Dataset.path t tmp_ds, release)
