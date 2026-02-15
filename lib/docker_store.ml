(* Represents a persistent cache.
   You must hold a cache's lock when removing or updating its entry in
   "cache", and must assume this may happen at any time when not holding it.
   The generation counter is used to check whether the cache has been updated
   since being cloned. The counter starts from zero when the in-memory cache
   value is created (i.e. you cannot compare across restarts). *)
type cache = {
  lock : Mutex.t;
  mutable gen : int;
}

type t = {
  root : string;        (* The top-level directory (containing `state`, etc). *)
  caches : (string, cache) Hashtbl.t;
  mutable next : int;   (* Used to generate unique temporary IDs. *)
}

let ( / ) = Filename.concat
let strf = Printf.sprintf

module Path = struct
  (* A Docker store contains several subdirectories:

     - state: for sqlite DB, etc
     - log_file: for logs *)

  let empty t            = t.root / "empty"
  let state t            = t.root / "state"
  let log_file t id      = t.root / "logs" / (id ^ ".log")
end

(* The OBuilder persistent cache is implemented using a shared Docker
   volume. As there's no snapshotting in volumes, we implement
   poor-man's snapshots: take a lock and copy the source. If the build
   of the new cache entry succeeds, it replaces the old one.

   For security reasons, each build step should only have access to
   its cache, so we need one volume per cache entry. The copy happens
   in the host filesystem. *)
module Cache : sig
  val cache : string -> [> `Docker_volume of string]
  val cache_tmp : int -> string -> [> `Docker_volume of string]

  val name : [ `Docker_volume of string] -> string

  val exists : [ `Docker_volume of string] -> bool
  val create : [ `Docker_volume of string] -> unit
  val snapshot : src:[ `Docker_volume of string] -> [ `Docker_volume of string] -> unit
  val delete : [`Docker_volume of string] -> unit
end = struct
  let cache name = Docker.docker_volume_cache (Escape.cache name)
  let cache_tmp i name = Docker.docker_volume_cache ~tmp:true (strf "%d-%s" i (Escape.cache name))

  let name (`Docker_volume name) = name

  let exists volume =
    let r = Docker.Cmd.exists volume in
    Result.is_ok r

  let create volume =
    let id = Docker.Cmd.volume ~timeout:5.0 (`Create volume) in
    Log.debug (fun f -> f "Volume id: %s" (String.trim id))

  let snapshot ~src dst =
    Log.debug (fun f -> f "Snapshotting volume %s to %s" (match src with `Docker_volume src -> src) (match dst with `Docker_volume dst -> dst));
    create dst;
    let base = if Sys.win32 then Docker_sandbox.servercore () else `Docker_image "busybox" in
    let r = Docker.cp_between_volumes ~base ~src ~dst in
    Log.debug (fun f -> f "Finished snapshotting");
    match r with Ok () -> () | Error (`Msg msg) -> failwith msg

  let delete volume =
    let _ = Docker.Cmd.volume (`Remove [volume]) in
    ()
end

let root t = t.root

let df t = Os.free_space_percent t.root
let cache_stats _ = 0, 0

let purge () =
  let containers = Docker.Cmd.obuilder_containers () in
  if containers <> [] then Docker.Cmd.rm containers;
  Log.info (fun f -> f "Removing left-over Docker images");
  let images = Docker.Cmd.obuilder_images ~tmp:true () in
  if images <> [] then Docker.Cmd.rmi images;
  Log.info (fun f -> f "Removing left-over Docker volumes");
  let volumes = Docker.Cmd.obuilder_caches_tmp () in
  if volumes <> [] then (let _ = Docker.Cmd.volume (`Remove volumes) in ())

let create root =
  Os.ensure_dir root;
  let hash = Unix.realpath root |> Sha256.string |> Sha256.to_hex in
  let hash = String.sub hash 0 7 in
  Docker.set_prefix (strf "obuilder-%s" hash);
  let t = { root; caches = Hashtbl.create 10; next = 0 } in
  Os.ensure_dir ~mode:0o0 (root / "empty");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "logs");
  purge ();
  t

let build t ?base ~id (fn:(string -> (unit, 'e) result)) : (unit, 'e) result =
  match base with
  | None ->
    (try fn (Path.empty t)
     with ex ->
       Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
       raise ex)
  | Some base ->
    let base = Docker.docker_image base in
    let tmp_image = (Docker.docker_image ~tmp:true id) in
    Docker.Cmd.tag base tmp_image;
    match (try Ok (fn (Path.empty t)) with ex -> Error ex) with
    | Ok r ->
      (* As the cache is cleaned before this, the sandbox must take
         care of committing the container and removing it, otherwise
         the container still has a reference to the cache. *)
      Docker.Cmd.image (`Remove tmp_image);
      r
    | Error ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Docker.Cmd.image (`Remove tmp_image);
      raise ex

let delete t id =
  let image = Docker.docker_image id in
  let exists = Docker.Cmd.exists image in
  (match exists with
    | Ok () -> Docker.Cmd.image (`Remove image)
    | Error _ -> ());
  let log_file = Path.log_file t id in
  if Sys.file_exists log_file then
    Unix.unlink log_file

let result t id =
  let img = Docker.docker_image id in
  let r = Docker.Cmd.exists img in
  match r with
  | Ok () -> Some (Path.empty t)
  | Error _ ->
    None

let log_file t id = Path.log_file t id

let state_dir = Path.state

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name =
  let cache = get_cache t name in
  Mutex.lock cache.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
  let tmp = Cache.cache_tmp t.next name in
  t.next <- t.next + 1;
  let snapshot = Cache.cache name in
  (* Create cache if it doesn't already exist. *)
  let exists = Cache.exists snapshot in
  if not exists then Cache.create snapshot;
  (* Create writeable clone. *)
  let gen = cache.gen in
  Cache.snapshot ~src:snapshot tmp;
  (match user with
    | `Unix { Obuilder_spec.uid; gid } ->
      let tmp = Docker.Cmd.mount_point tmp in
      Os.sudo ["chown"; strf "%d:%d" uid gid; tmp]
    | `Windows _ -> () (* FIXME: does Windows need special treatment? *));
  let release () =
    Mutex.lock cache.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
    (if cache.gen = gen then (
      (* The cache hasn't changed since we cloned it. Update it. *)
      (* todo: check if it has actually changed. *)
      cache.gen <- cache.gen + 1;
      Cache.delete snapshot;
      Cache.snapshot ~src:tmp snapshot));
    Cache.delete tmp)
  in
  (Cache.name tmp, release))

let delete_cache t name =
  let cache = get_cache t name in
  Mutex.lock cache.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock cache.lock) (fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Cache.cache name in
  let exists = Cache.exists snapshot in
  if exists then
    let containers = Docker.Cmd.volume_containers snapshot in
    if containers <> [] then (
      Cache.delete snapshot;
      Ok ())
    else
      Error `Busy
  else Ok ())

let complete_deletes t =
  ignore t;
  (* FIXME: how to implement this? *)
  ()
