open Lwt.Syntax

(* Represents a persistent cache.
   You must hold a cache's lock when removing or updating its entry in
   "cache", and must assume this may happen at any time when not holding it.
   The generation counter is used to check whether the cache has been updated
   since being cloned. The counter starts from zero when the in-memory cache
   value is created (i.e. you cannot compare across restarts). *)
type cache = {
  lock : Lwt_mutex.t;
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

  val exists : [ `Docker_volume of string] -> bool Lwt.t
  val create : [ `Docker_volume of string] -> unit Lwt.t
  val snapshot : src:[ `Docker_volume of string] -> [ `Docker_volume of string] -> unit Lwt.t
  val delete : [`Docker_volume of string] -> unit Lwt.t
end = struct
  let cache name = Docker.docker_volume_cache (Escape.cache name)
  let cache_tmp i name = Docker.docker_volume_cache ~tmp:true (strf "%d-%s" i (Escape.cache name))

  let name (`Docker_volume name) = name

  let exists volume =
    let+ r = Docker.Cmd.exists volume in
    Result.is_ok r

  let create volume =
    let* id = Docker.Cmd.volume ~timeout:5.0 (`Create volume) in
    Log.debug (fun f -> f "Volume id: %s" (String.trim id));
    Lwt.return_unit

  let snapshot ~src dst =
    Log.debug (fun f -> f "Snapshotting volume %s to %s" (match src with `Docker_volume src -> src) (match dst with `Docker_volume dst -> dst));
    let* () = create dst in
    let* base = if Sys.win32 then Docker_sandbox.servercore () else Lwt.return (`Docker_image "busybox") in
    let* r = Docker.cp_between_volumes ~base ~src ~dst in
    Log.debug (fun f -> f "Finished snapshotting");
    match r with Ok () -> Lwt.return_unit | Error (`Msg msg) -> failwith msg

  let delete volume =
    let* _ = Docker.Cmd.volume (`Remove [volume]) in
    Lwt.return_unit
end

let root t = t.root

let df t = Lwt.return (Os.free_space_percent t.root)
let cache_stats _ = 0, 0

let purge () =
  let* containers = Docker.Cmd.obuilder_containers () in
  let* () = if containers <> [] then Docker.Cmd.rm containers else Lwt.return_unit in
  Log.info (fun f -> f "Removing left-over Docker images");
  let* images = Docker.Cmd.obuilder_images ~tmp:true () in
  let* () =  if images <> [] then Docker.Cmd.rmi images else Lwt.return_unit in
  Log.info (fun f -> f "Removing left-over Docker volumes");
  let* volumes = Docker.Cmd.obuilder_caches_tmp () in
  let* _ = if volumes <> [] then Docker.Cmd.volume (`Remove volumes) else Lwt.return "" in
  Lwt.return_unit

let create root =
  Os.ensure_dir root;
  let hash = Unix.realpath root |> Sha256.string |> Sha256.to_hex in
  let hash = String.sub hash 0 7 in
  Docker.set_prefix (strf "obuilder-%s" hash);
  let t = { root; caches = Hashtbl.create 10; next = 0 } in
  Os.ensure_dir ~mode:0o0 (root / "empty");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "logs");
  let* () = purge () in
  Lwt.return t

let build t ?base ~id (fn:(string -> (unit, 'e) Lwt_result.t)) : (unit, 'e) Lwt_result.t =
  match base with
  | None ->
    Lwt.catch
      (fun () -> fn (Path.empty t))
      (fun ex ->
         Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
         Lwt.reraise ex)
  | Some base ->
    let base = Docker.docker_image base in
    let tmp_image = (Docker.docker_image ~tmp:true id) in
    let* () = Docker.Cmd.tag base tmp_image in
    Lwt.try_bind
      (fun () -> fn (Path.empty t))
      (fun r ->
         (* As the cache is cleaned before this, the sandbox must take
            care of committing the container and removing it, otherwise
            the container still has a reference to the cache. *)
         let+ () = Docker.Cmd.image (`Remove tmp_image) in
         r)
      (fun ex ->
         Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
         let* () = Docker.Cmd.image (`Remove tmp_image) in
         Lwt.reraise ex)

let delete t id =
  let image = Docker.docker_image id in
  let* exists = Docker.Cmd.exists image in
  let* () = match exists with
    | Ok () -> Docker.Cmd.image (`Remove image)
    | Error _ -> Lwt.return_unit
  in
  let log_file = Path.log_file t id in
  if Sys.file_exists log_file then
    Lwt_unix.unlink log_file
  else Lwt.return_unit

let result t id =
  let img = Docker.docker_image id in
  let* r = Docker.Cmd.exists img in
  match r with
  | Ok () -> Lwt.return_some (Path.empty t)
  | Error _ ->
    Lwt.return_none

let log_file t id = Lwt.return (Path.log_file t id)

let state_dir = Path.state

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name : (string * (unit -> unit Lwt.t)) Lwt.t =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Cache.cache_tmp t.next name in
  t.next <- t.next + 1;
  let snapshot = Cache.cache name in
  (* Create cache if it doesn't already exist. *)
  let* () =
    let* exists = Cache.exists snapshot in
    if not exists then Cache.create snapshot
    else Lwt.return_unit
  in
  (* Create writeable clone. *)
  let gen = cache.gen in
  let* () = Cache.snapshot ~src:snapshot tmp in
  let+ () = match user with
    | `Unix { Obuilder_spec.uid; gid } ->
      let* tmp = Docker.Cmd.mount_point tmp in
      Os.sudo ["chown"; strf "%d:%d" uid gid; tmp]
    | `Windows _ -> Lwt.return_unit (* FIXME: does Windows need special treatment? *)
  in
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    let* () =
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        let* () = Cache.delete snapshot in
        Cache.snapshot ~src:tmp snapshot
      ) else Lwt.return_unit
    in
    Cache.delete tmp
  in
  Cache.name tmp, release

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Cache.cache name in
  let* exists = Cache.exists snapshot in
  if exists then
    let* containers = Docker.Cmd.volume_containers snapshot in
    if containers <> [] then
      let* () = Cache.delete snapshot in
      Lwt_result.ok Lwt.return_unit
    else
      Lwt_result.fail `Busy
  else Lwt_result.ok Lwt.return_unit

let complete_deletes t =
  ignore t;
  (* FIXME: how to implement this? *)
  Lwt.return_unit
