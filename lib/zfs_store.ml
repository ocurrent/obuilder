(* This is rather complicated, because (unlike btrfs):
   - zfs won't let you delete datasets that other datasets are cloned from.
     However, you can "promote" a dataset, so that it switches roles with its parent.
   - Some zfs commands use "--" to separate options from arguments, but others interpret it as an argument!
   - Sometimes we need "datasets" and at other times we need pathnames (the difference is a leading '/')

   Example of zfs promotion:

   1. Create ds1.
   2. Create snapshots ds1@snap1, ds1@snap2, ds1@snap3.
   3. Create clones of ds1@snap2: clone1, clone2, clone3.

   At this point:
   - ds1 has clones {clone1, clone2, clone3} and snapshots {snap1, snap2, snap3}.

   4. Promote clone2.

   Now:
   - clone2 has clones {clone1, ds1, clone3} and snapshots {snap1, snap2}.
   - ds1 has no clones and snapshots {snap3}.
*)

let strf = Printf.sprintf

type cache = {
  lock : Mutex.t;
  mutable gen : int;            (* Version counter. *)
  mutable n_clones : int;
}

type t = {
  pool : string;
  prefix : string; (* To be prepended to `pool` to give the full path to the pool *)
  caches : (string, cache) Hashtbl.t;
  mutable next : int;
}

let default_snapshot = "snap"

module Dataset : sig
  type dataset

  val state : dataset
  val cache_tmp_group : dataset
  val groups : dataset list

  val result : S.id -> dataset
  val cache : string -> dataset
  val cache_tmp : int -> string -> dataset

  val full_name : ?snapshot:string -> ?subvolume:string -> t -> dataset -> string
  val path : ?snapshot:string -> t -> dataset -> string

  val exists : ?snapshot:string -> t -> dataset -> bool
  val if_missing : ?snapshot:string -> t -> dataset -> (unit -> unit) -> unit
end = struct
  type dataset = string

  let state = "state"
  let result_group = "result"
  let cache_group = "cache"
  let cache_tmp_group = "cache-tmp"

  let groups = [state; result_group; cache_group; cache_tmp_group]

  let result id = "result/" ^ id
  let cache name = "cache/" ^ Escape.cache name
  let cache_tmp i name = strf "cache-tmp/%d-%s" i (Escape.cache name)

  let full_name ?snapshot ?subvolume t ds =
    match snapshot, subvolume with
    | None, None -> strf "%s/%s" t.pool ds
    | Some snapshot, None -> strf "%s/%s@%s" t.pool ds snapshot
    | None, Some subvolume -> strf "%s/%s/%s" t.pool ds subvolume
    | Some snapshot, Some subvolume -> strf "%s/%s/%s@%s" t.pool ds subvolume snapshot

  let path ?snapshot t ds =
    match snapshot with
    | None -> strf "%s%s/%s" t.prefix t.pool ds
    | Some snapshot -> strf "%s%s/%s/.zfs/snapshot/%s" t.prefix t.pool ds snapshot

  let exists ?snapshot t ds =
    let output = Os.pread ["zfs"; "list"; "-p"; "-H"; full_name t ds ?snapshot] in
    match output with
    | "" -> false
    | _ -> true

  let if_missing ?snapshot t ds fn =
    match exists ?snapshot t ds with
    | true -> ()
    | false -> fn ()
end

let user = `Unix { Obuilder_spec.uid = Unix.getuid (); gid = Unix.getgid () }

module Zfs = struct
  let chown ~user t ds =
    let { Obuilder_spec.uid; gid } = match user with `Unix user -> user | `Windows _ -> assert false in
    Os.sudo ["chown"; strf "%d:%d" uid gid; Dataset.path t ds]

  let create t ds =
    Os.sudo ["zfs"; "create"; "--"; Dataset.full_name t ds]

  let destroy t ds mode =
    let opts =
      match mode with
      | `Only -> ["-f"]
      | `And_snapshots -> ["-r"; "-f"]
      | `And_snapshots_and_clones -> ["-R"; "-f"]
    in
    Os.sudo (["zfs"; "destroy"] @ opts @ ["--"; Dataset.full_name t ds])

  let destroy_snapshot t ds snapshot mode =
    let opts =
      match mode with
      | `Defer -> ["-d"]
      | `Recurse -> ["-R"]
      | `Immediate -> []
    in
    Os.sudo (["zfs"; "destroy"] @ opts @ ["--"; Dataset.full_name t ds ^ "@" ^ snapshot])

  let clone t ~src ~snapshot dst =
    Os.sudo ["zfs"; "clone"; "--"; Dataset.full_name t src ~snapshot; Dataset.full_name t dst]

  let mounted ?snapshot t ~ds =
    let s = Os.pread ["zfs"; "get"; "-pH"; "mounted"; Dataset.full_name t ds ?snapshot] in
    match ( Scanf.sscanf s "%s %s %s %s" (fun _ _ yesno _ -> yesno = "yes") ) with
    | state -> state
    | exception Scanf.Scan_failure _ -> false

  let mount ?snapshot t ~ds =
    let m = mounted t ~ds ?snapshot in
    if not m then
      let pp _ ppf = Fmt.pf ppf "zfs mount" in
      let t = Os.sudo_result ~pp:(pp "zfs mount") ~is_success:(fun n -> n = 0 || n = 16) ["zfs"; "mount"; "--"; Dataset.full_name t ds ?snapshot] in
        match t with
        | Ok () -> ()
        | Error (`Msg m) ->
          Log.info (fun f -> f "%s" m)

  let clone_with_children t ~src ~snapshot dst =
    Os.sudo ["zfs"; "clone"; "-o"; "canmount=noauto"; "--"; Dataset.full_name t src ~snapshot; Dataset.full_name t dst];
    Os.sudo ["zfs"; "mount"; Dataset.full_name t dst];
    let vol = Dataset.full_name t src in
    let len = String.length vol in
    let output = Os.pread ["zfs"; "list"; "-H"; "-r"; "-o"; "name"; vol] in
    String.split_on_char '\n' output |> List.map (fun s -> (s, String.length s)) |>
      List.filter (fun (_, l) -> l > len) |> List.map (fun (s, l) -> String.sub s (len + 1) (l - len - 1)) |>
      List.iter (fun subvolume -> Os.sudo ["zfs"; "clone"; "-o"; "mountpoint=none"; "--";
        Dataset.full_name t src ~subvolume ~snapshot; Dataset.full_name t dst ~subvolume])

  let snapshot t ds ~snapshot =
    Os.sudo ["zfs"; "snapshot"; "-r"; "--"; Dataset.full_name t ds ~snapshot]

  let promote t ds =
    Os.sudo ["zfs"; "promote"; Dataset.full_name t ds]

  let rename t ~old ds =
    Os.sudo ["zfs"; "rename"; "--"; Dataset.full_name t old; Dataset.full_name t ds]

  let rename_snapshot t ds ~old snapshot =
    Os.sudo ["zfs"; "rename"; "--";
          Dataset.full_name t ds ~snapshot:old;
          Dataset.full_name t ds ~snapshot]
end

let delete_if_exists t ds mode =
  match Dataset.exists t ds with
  | true -> Zfs.destroy t ds mode
  | false -> ()

let state_dir t = Dataset.path t Dataset.state

let root t = t.pool

let df t =
  let s = Os.pread ["zpool"; "list"; "-Hp"; "-o"; "capacity"; t.pool] in
  match (String.trim s) with
  | "" -> 0.
  | s -> 100. -. float_of_string s

let prefix_and_pool path =
  let pool = Filename.basename path in
  match Filename.chop_suffix_opt ~suffix:pool path with
   | Some "" -> ("/", pool) (* Preserves original behaviour *)
   | Some prefix -> (prefix, pool)
   | None -> failwith ("Failed to get preffix from: " ^ path)

let create ~path =
  let prefix, pool = prefix_and_pool path in
  let t = { pool; prefix; caches = Hashtbl.create 10; next = 0 } in
  (* Ensure any left-over temporary datasets are removed before we start. *)
  delete_if_exists t (Dataset.cache_tmp_group) `And_snapshots_and_clones;
  Dataset.groups |> List.iter (fun group ->
      Dataset.if_missing t group (fun () -> Zfs.create t group);
      Zfs.chown ~user t group
    );
  t

(* The builder will always delete child datasets before their parent.
   It's possible that we crashed after cloning this but before recording that
   in the database. So any clones of this dataset must be unregistered junk. *)
let delete t id =
  delete_if_exists t (Dataset.result id) `And_snapshots_and_clones

(* We start by either creating a new dataset or by cloning base@snap (if [base] is given).
   On success, we snapshot the clone as clone@snap.
   On failure, we destroy the clone. This will always succeed because we can't have
   tagged it or created further clones at this point. *)
let build t ?base ~id fn =
  Log.debug (fun f -> f "zfs: build %S" id);
  let ds = Dataset.result id in
  (* We have to create the dataset in its final location because ZFS can't
     rename it while we have the log file open (which we need to do). But
     we don't create the snapshot unless the build succeeds. If we crash
     with a partially written directory, `result` will see there is no
     snapshot and we'll end up here and delete it. *)
  delete_if_exists t ds `Only;
  let clone = Dataset.path t ds in
  (match base with
    | None ->
      Zfs.create t ds;
      Zfs.chown ~user t ds
    | Some base ->
      let src = Dataset.result base in
      Zfs.clone_with_children t ~src ~snapshot:default_snapshot ds);
  match (try Ok (fn clone) with ex -> Error ex) with
  | Ok (Ok () as e) ->
    Log.debug (fun f -> f "zfs: build %S succeeded" id);
    Zfs.snapshot t ds ~snapshot:default_snapshot;
    (* ZFS can't delete the clone while the snapshot still exists. So I guess we'll just
       keep it around? *)
    e
  | Ok (Error _ as e) ->
    Log.debug (fun f -> f "zfs: build %S failed" id);
    Zfs.destroy t ds `And_snapshots;
    e
  | Error ex ->
    Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
    Zfs.destroy t ds `And_snapshots;
    raise ex

let result t id =
  let ds = Dataset.result id in
  let e = Dataset.exists t ds ~snapshot:default_snapshot in
  if e then (
    Zfs.mount t ~ds;
    Zfs.mount t ~ds ~snapshot:default_snapshot;
    let path = Dataset.path t ds ~snapshot:default_snapshot in
    Some path
  ) else None

let log_file t id =
  match result t id with
  | Some dir -> Filename.concat dir "log"
  | None ->
    let ds = Dataset.result id in
    let clone = Dataset.path t ds in
    Filename.concat clone "log"

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Mutex.create (); gen = 0; n_clones = 0 } in
    Hashtbl.add t.caches name c;
    c

(* Return the name of an unused temporary dataset, based on [name]. *)
let get_tmp_ds t name =
  let tmp_ds = Dataset.cache_tmp t.next name in
  t.next <- t.next + 1;
  tmp_ds

(* Properties you can assume after taking the lock:

   - Either we have a dataset with the latest snapshot of the cache
     (main@snap), or it doesn't exist yet (in which case we create it and
     snapshot immediately).

   - Any other tags on main are marked for deletion, but some clones
     still depend on them. They will all be older than "snap".

   - There may be clones of main. These clones have no snapshots, and no
     further sub-clones.

   We clone main@snap, and then let the user write to that (tmp) with the lock
   released.

   When the user releases tmp, we retake the lock and then either:

   - Destroy tmp, or
   - Replace main with tmp (see comments in code).

   Crash recovery:

   - We might crash before making the main@snap tag. If main is missing this tag,
     it is safe to create it, since we must have been just about to do that.
*)
let cache ~user t name =
  let cache = get_cache t name in
  Mutex.lock cache.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
  Log.debug (fun f -> f "zfs: get cache %S" (name :> string));
  let gen = cache.gen in
  let main_ds = Dataset.cache name in
  let tmp_ds = get_tmp_ds t name in
  (* Create the cache as an empty directory if it doesn't exist. *)
  Dataset.if_missing t main_ds (fun  () -> Zfs.create t main_ds);
  (* Ensure we have the snapshot. This is needed on first creation, and
     also to recover from crashes. *)
  Dataset.if_missing t main_ds ~snapshot:default_snapshot (fun () ->
      Zfs.chown ~user t main_ds;
      Zfs.snapshot t main_ds ~snapshot:default_snapshot
    );
  cache.n_clones <- cache.n_clones + 1;
  Zfs.clone t ~src:main_ds ~snapshot:default_snapshot tmp_ds;
  let release () =
    Mutex.lock cache.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
    Log.debug (fun f -> f "zfs: release cache %S" (name :> string));
    cache.n_clones <- cache.n_clones - 1;
    if cache.gen = gen then (
      (* main_ds hasn't changed since we cloned it. Update it. *)
      (* todo: check if tmp_ds has changed. *)
      cache.gen <- cache.gen + 1;
      (* Rename main to something temporary, so if we crash here then we'll
         just start again with an empty cache next time. *)
      let delete_me = get_tmp_ds t name in
      Zfs.rename t ~old:main_ds delete_me;
      Zfs.promote t tmp_ds;
      (* At this point:
         - All the other clones of main are now clones of tmp_ds.
         - main@snap has moved to tmp@snap.
         - Any other tags were older than snap and so have also moved to tmp. *)
      Zfs.destroy t delete_me `Only;
      (* Move the old @snap tag out of the way. *)
      let archive_name = strf "old-%d" gen in
      (* We know [archive_name] doesn't exist because [gen] is unique for
         this process, and we delete stale tmp dirs from previous runs at start-up,
         which would remove any such deferred tags. *)
      Zfs.rename_snapshot t tmp_ds ~old:default_snapshot archive_name;
      (* Mark the archived snapshot for removal. If other clones are using it,
         this will defer the deletion until they're done *)
      Zfs.destroy_snapshot t tmp_ds archive_name `Defer;
      (* Create the new snapshot and rename this as the new main_ds. *)
      Zfs.snapshot t tmp_ds ~snapshot:default_snapshot;
      Zfs.rename t ~old:tmp_ds main_ds
    ) else (
      (* We have no snapshots or clones here. *)
      (try Zfs.destroy t tmp_ds `Only
       with ex ->
         Log.warn (fun f -> f "Error trying to release cache (will retry): %a" Fmt.exn ex);
         (* XXX: Don't know what's causing this. By the time fuser runs, the problem has disappeared! *)
         Unix.system (strf "fuser -mv %S" (Dataset.path t tmp_ds)) |> ignore;
         Unix.sleepf 10.0;
         Zfs.destroy t tmp_ds `Only)
    ))
  in
  (Dataset.path t tmp_ds, release))

let delete_cache t name =
  let cache = get_cache t name in
  Mutex.lock cache.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
  Log.debug (fun f -> f "zfs: delete_cache %S" (name :> string));
  if cache.n_clones > 0 then Error `Busy
  else
    let main_ds = Dataset.cache name in
    match Dataset.exists t main_ds with
    | true -> Zfs.destroy t main_ds `And_snapshots;
      Ok ()
    | false -> Ok ())

let complete_deletes _t =
  (* The man-page says "Pending changes are generally accounted for within a few seconds" *)
  Unix.sleepf 5.0
