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
  root : string;        (* The top-level directory (containing `result`, etc). *)
  caches : (string, cache) Hashtbl.t;
  mutable next : int;   (* Used to generate unique temporary IDs. *)
}

let ( / ) = Filename.concat

(* The OBuilder persistent cache is implemented using a shared Docker
   volume. As there's no snapshotting in volumes, we implement
   poor-man's snapshots: take a lock and copy the source. If the build
   of the new cache entry succeeds, it replaces the old one.

   For security reasons, each build step should only have access to
   its cache, so we need one volume per cache entry. The copy happens
   in the host filesystem. *)
module Cache : sig
  val empty : t -> string
  val cache : string -> [> `Docker_volume of string]
  val cache_tmp : int -> string -> [> `Docker_volume of string]

  val name : [< `Docker_volume of string] -> string

  val exists : [< `Docker_volume of string] -> bool Lwt.t
  val create : [< `Docker_volume of string] -> unit Lwt.t
  val snapshot : src:[< `Docker_volume of string] -> [< `Docker_volume of string] -> unit Lwt.t
  val delete : [`Docker_volume of string] -> unit Lwt.t
end = struct
  let volume_prefix = "obuilder-cache-"
  let volume_tmp_prefix = "obuilder-cache-tmp-"

  let empty t = t.root / "empty"
  let cache name = `Docker_volume (volume_prefix ^ Escape.cache name)
  let cache_tmp i name = `Docker_volume (volume_tmp_prefix ^ Printf.sprintf "%d-%s" i (Escape.cache name))

  let name (`Docker_volume name) = name

  let exists volume =
    let* r = Docker.exists volume in
    let b = Result.is_ok r in
    Log.debug (fun f -> f "CACHE EXISTS %s %b" (name volume) b);
    Lwt.return b

  let create volume =
    Log.debug (fun f -> f "CACHE CREATE %s" (name volume));
    let* _ = Docker.volume ["create"] volume in
    Lwt.return_unit

  let snapshot ~src dst =
    Log.debug (fun f -> f "CACHE SNAPSHOT src: %s dst: %s" (name src) (name dst));
    let* () = create dst in
    let* src = Docker.mount_point src in
    let* dst = Docker.mount_point dst in
    if Sys.win32 then
      Os.exec ["robocopy"; src; dst; "/MIR"; "/NFL"; "/NDL"; "/NJH"; "/NJS"; "/NC"; "/NS"; "/NP"]
        ~is_success:(fun n -> n = 0 || n = 1)
    else
      Os.sudo ["cp"; "-a"; "--"; src ^ "/."; dst ]

  let delete volume =
    Log.debug (fun f -> f "CACHE DELETE %s" (name volume));
    let* _ = Docker.volume ["rm"] volume in
    Lwt.return_unit
end

let create root =
  let prefix = Printf.sprintf "obuilder-%d-" (Hashtbl.hash root) in
  Docker.set_prefix prefix;
  Log.debug (fun f -> f "Docker store: create %s %s" root prefix);
  let t = { root; caches = Hashtbl.create 10; next = 0 } in
  Os.ensure_dir root;
  Os.ensure_dir (Cache.empty t);
  Os.ensure_dir (root / "state");
  Lwt.return t

let build t ?base ~id (fn:(string -> (unit, 'e) Lwt_result.t)) : (unit, 'e) Lwt_result.t =
  Log.debug (fun f -> f "Docker store: build base:%s id:%s" (Option.fold ~none:"None" ~some:(fun base -> base) base) id);
  let result = Docker.result t.root id in
  assert (not (Sys.file_exists result));
  Os.ensure_dir result;
  Log.debug (fun f -> f "Docker store: build t.root:%s id:%s result:%s" t.root id result);
  match base with
  | None ->
     Lwt.try_bind
       (fun () -> fn result)
       (fun r ->
         begin match r with
         | Ok (()) -> ()
         | Error _ -> ()
         end; Lwt.return r)
       (fun exn -> Lwt.fail exn)
  | Some base ->
     let base = Docker.docker_image base in
     let tmp_image = (Docker.docker_image ~tmp:true id) in
     let cleanup () =
       Docker.image "rm" tmp_image
     in
     let* () = Docker.tag base tmp_image in
     Lwt.try_bind
       (fun () ->
         Log.debug (fun f -> f "Docker store: running build");
         let* r = fn result in
         Log.debug (fun f -> f "Docker store: build finished");
         Lwt.return r)
       (fun r ->
         Log.debug (fun f -> f "Docker store: build cleanup");
         (* As the cache is cleaned before this, the sandbox must take
            care of committing the container and removing it, otherwise
            the container still has a reference to the volume. *)
         let* () = cleanup () in
         Lwt.return r)
       (fun exn ->
         Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn exn);
         let* () = cleanup () in
         Lwt.fail exn)

let delete t id =
  Log.debug (fun f -> f "Docker store: delete %s" id);
  let path = Docker.result t.root id in
  let* () = match Os.check_dir path with
    | `Present -> Os.delete_recursively path
    | `Missing -> Lwt.return_unit
  in
  let image = Docker.docker_image id in
  let* exists = Docker.exists image in
  match exists with
  | Ok () -> Docker.image "rm" image
  | Error _ -> Lwt.return_unit

let result t id =
  Log.debug (fun f -> f "Docker store: result %s" id);
  let dir = Docker.result t.root id in
  let* r = Docker.exists (Docker.docker_image id) in
  match r, Os.check_dir dir with
  | Ok (_), `Present  -> Lwt.return_some dir
  | _ -> Lwt.return_none

let state_dir t =
  Log.debug (fun f -> f "Docker store: state_dir %s" t.root);
  Filename.concat t.root "state"

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name : (string * (unit -> unit Lwt.t)) Lwt.t =
  ignore user;
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
  Lwt.return (Cache.name tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Cache.cache name in
  let* exists = Cache.exists snapshot in
  if exists then
    let* () = Cache.delete snapshot in
    Lwt_result.return ()
  else Lwt_result.return ()

let complete_deletes t =
  ignore t;
  Log.debug (fun f -> f "Docker store: complete_deletes");
  (* XXX: how to implement this? *)
  Lwt.return_unit
