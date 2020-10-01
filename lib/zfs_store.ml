open Lwt.Infix

(* This is rather complicated, because (unlike btrfs):
   - zfs won't let you delete datasets that other datasets are cloned from.
     However, you can "promote" a dataset, so that it switches roles with its parent.
   - Some zfs commands use "--" to separate options from arguments, but others interpret it as an argument!
   - Sometimes we need "datasets" and at other times we need pathnames (the difference is a leading '/')
*)

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

let default_snapshot = "snap"

module Dataset : sig
  type dataset

  val state : dataset
  val of_full_name : t -> string -> dataset
  val result : S.id -> dataset
  val cache : Spec.cache_id -> dataset
  val cache_tmp : int -> Spec.cache_id -> dataset

  val full_name : ?snapshot:string -> t -> dataset -> string
  val path : ?snapshot:string -> t -> dataset -> string

  val exists : ?snapshot:string -> t -> dataset -> bool
  val if_missing : t -> dataset -> (unit -> unit Lwt.t) -> unit Lwt.t
end = struct
  type dataset = string

  let state = "state"

  let of_full_name t x =
    match Astring.String.cut ~sep:"/" x with
    | None -> Fmt.failwith "Not a full DS name %S" x
    | Some (a, b) ->
      if a <> t.pool then Fmt.failwith "Expected pool %S in %S!" t.pool x
      else b

  let result id = "r-" ^ id
  let cache (name : Spec.cache_id) = "c-" ^ (name :> string)
  let cache_tmp i (name : Spec.cache_id) = strf "t-%d-%s" i (name :> string)

  let full_name ?snapshot t ds =
    match snapshot with
    | None -> strf "%s/%s" t.pool ds
    | Some snapshot -> strf "%s/%s@%s" t.pool ds snapshot

  let path ?snapshot t ds =
    match snapshot with
    | None -> strf "/%s/%s" t.pool ds
    | Some snapshot -> strf "/%s/%s/.zfs/snapshot/%s" t.pool ds snapshot

  let exists ?snapshot t ds =
    match Os.check_dir (path ?snapshot t ds) with
    | `Missing -> false
    | `Present -> true

  let if_missing t ds fn =
    if exists t ds then Lwt.return_unit
    else fn ()
end

let user = { Spec.uid = Unix.getuid (); gid = Unix.getgid () }

module Zfs = struct
  let create ?(user=user) t ds =
    let { Spec.uid; gid } = user in
    Os.exec ["sudo"; "zfs"; "create"; "--"; Dataset.full_name t ds] >>= fun () ->
    Os.exec ["sudo"; "chown"; strf "%d:%d" uid gid; Dataset.path t ds]

  let destroy t ds mode =
    let opts =
      match mode with
      | `Only -> []
      | `And_snapshots -> ["-r"]
      | `And_snapshots_and_clones -> ["-R"]
    in
    Os.exec (["sudo"; "zfs"; "destroy"] @ opts @ ["--"; Dataset.full_name t ds])

  let destroy_snapshot ?(defer=false) t ds snapshot =
    let opts = if defer then ["-d"] else [] in
    Os.exec (["sudo"; "zfs"; "destroy"] @ opts @ ["--"; Dataset.full_name t ds ^ "@" ^ snapshot])

  let clone t ~src ~snapshot dst =
    Os.exec ["sudo"; "zfs"; "clone"; "--"; Dataset.full_name t src ~snapshot; Dataset.full_name t dst]

  let snapshot t ds ~snapshot =
    Os.exec ["sudo"; "zfs"; "snapshot"; "--"; Dataset.full_name t ds ~snapshot]

  let promote t ds =
    Os.exec ["sudo"; "zfs"; "promote"; Dataset.full_name t ds]

  let rename t ~old ds =
    Os.exec ["sudo"; "zfs"; "rename"; "--"; Dataset.full_name t old; Dataset.full_name t ds]

  let rename_snapshot t ds ~old snapshot =
    Os.exec ["sudo"; "zfs"; "rename"; "--";
             Dataset.full_name t ds ~snapshot:old;
             Dataset.full_name t ds ~snapshot]

  let clones t ds ~snapshot =
    Os.pread ["sudo"; "zfs"; "list"; "-H"; "-o"; "clones"; Dataset.full_name t ds ~snapshot]
    >|= String.split_on_char '\n'
    >|= List.filter_map (function
        | "" -> None
        | x -> Some (Dataset.of_full_name t x)
      )
end

let delete_if_exists t ds mode =
  if Dataset.exists t ds then Zfs.destroy t ds mode
  else Lwt.return_unit

let state_dir t = Dataset.path t Dataset.state

let create ~pool =
  let t = { pool; caches = Hashtbl.create 10; next = 0 } in
  Dataset.if_missing t Dataset.state (fun () -> Zfs.create t Dataset.state) >|= fun () ->
  t

let delete t id =
  delete_if_exists t (Dataset.result id) `And_snapshots

let build t ?base ~id fn =
  Log.debug (fun f -> f "zfs: build %S" id);
  let ds = Dataset.result id in
  (* We have to create the dataset in its final location because ZFS can't
     rename it while we have the log file open (which we need to do). But
     we don't create the snapshot unless the build succeeds. If we crash
     with a partially written directory, `result` will see there is no
     snapshot and we'll end up here and delete it. *)
  delete_if_exists t ds `And_snapshots >>= fun () ->
  let clone = Dataset.path t ds in
  begin match base with
    | None -> Zfs.create t ds
    | Some base ->
      let src = Dataset.result base in
      Zfs.clone t ~src ~snapshot:default_snapshot ds
  end
  >>= fun () ->
  fn clone >>= function
  | Ok () ->
    Log.debug (fun f -> f "zfs: build %S succeeded" id);
    Zfs.snapshot t ds ~snapshot:default_snapshot >>= fun () ->
    (* ZFS can't delete the clone while the snapshot still exists. So I guess we'll just
       keep it around? *)
    Lwt_result.return ()
  | Error _ as e ->
    Log.debug (fun f -> f "zfs: build %S failed" id);
    Zfs.destroy t ds `Only >>= fun () ->
    Lwt.return e

let result t id =
  let ds = Dataset.result id in
  let path = Dataset.path t ds ~snapshot:default_snapshot in
  if Sys.file_exists path then Some path
  else None

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
  Log.debug (fun f -> f "zfs: get cache %S" (name :> string));
  let gen = cache.gen in
  let main_ds = Dataset.cache name in
  let tmp_ds = Dataset.cache_tmp t.next name in
  t.next <- t.next + 1;
  (* Create the cache as an empty directory if it doesn't exist. *)
  Dataset.if_missing t main_ds (fun  () ->
      Zfs.create ~user t main_ds >>= fun () ->
      Zfs.snapshot t main_ds ~snapshot:default_snapshot
    ) >>= fun () ->
  (* Remove any left-over tmp dir from a previous run. *)
  delete_if_exists t tmp_ds `And_snapshots_and_clones >>= fun () ->
  Zfs.clone t ~src:main_ds ~snapshot:default_snapshot tmp_ds >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    Log.debug (fun f -> f "zfs: release cache %S" (name :> string));
    if cache.gen = gen then (
      (* The cache hasn't changed since we cloned it. Update it. *)
      (* todo: check if it has actually changed. *)
      cache.gen <- cache.gen + 1;
      Zfs.promote t tmp_ds >>= fun () ->
      Zfs.destroy t main_ds `Only >>= fun () ->
      Zfs.rename t ~old:tmp_ds main_ds >>= fun () ->
      (* Remove the previous snapshot. If other clones are using it, this will defer the deletion until they're done *)
      Zfs.destroy_snapshot ~defer:true t main_ds default_snapshot >>= fun () ->
      begin
        if Dataset.exists ~snapshot:default_snapshot t main_ds then (
          (* It's still there. Must be a deferred deletion. Move it out of the way. *)
          let archive_name = strf "old-%d" gen in
          Zfs.rename_snapshot t main_ds ~old:default_snapshot archive_name
        ) else Lwt.return_unit
      end >>= fun () ->
      Zfs.snapshot t main_ds ~snapshot:default_snapshot
    ) else (
      Zfs.destroy t tmp_ds `And_snapshots
    )
  in
  Lwt.return (Dataset.path t tmp_ds, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  Log.debug (fun f -> f "zfs: delete_cache %S" (name :> string));
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let main_ds = Dataset.cache name in
  if Dataset.exists t main_ds then (
    let tmp_ds = Dataset.cache_tmp t.next name in
    t.next <- t.next + 1;
    (* Remove any left-over tmp dir from a previous run. *)
    delete_if_exists t tmp_ds `And_snapshots_and_clones >>= fun () ->
    (* Rename it out of the way *)
    Zfs.rename t ~old:main_ds tmp_ds >>= fun () ->
    (* Check if it's in use *)
    Zfs.clones t tmp_ds ~snapshot:default_snapshot >>= fun clones ->
    begin 
      match clones with
      | [] -> Lwt.return_unit
      | c :: _ ->
        Zfs.promote t c >>= fun () ->
        Zfs.destroy_snapshot ~defer:true t c default_snapshot
    end >>= fun () ->
    Zfs.destroy t tmp_ds `And_snapshots
  ) else Lwt.return_unit
