open Eio

let strf = Printf.sprintf

let running_as_root = Unix.getuid () = 0

(* Represents a persistent cache.
   You must hold a cache's lock when removing or updating its entry in
   "cache", and must assume this may happen at any time when not holding it.
   The generation counter is used to check whether the cache has been updated
   since being cloned. The counter starts from zero when the in-memory cache
   value is created (i.e. you cannot compare across restarts). *)
type cache = {
  lock : Eio.Mutex.t;
  mutable gen : int;
}

type t = {
  root : string;        (* The top-level directory (containing `result`, etc). *)
  process : Process.t;
  caches : (string, cache) Hashtbl.t;
  mutable next : int;   (* Used to generate unique temporary IDs. *)
}

let ( / ) = Filename.concat

module Btrfs = struct
  let btrfs ?(sudo=false) t args =
    let args = "btrfs" :: args in
    let args = if sudo && not running_as_root then "sudo" :: args else args in
    Switch.run @@ fun sw ->
    Os.exec ~sw ~process:t.process args

  let subvolume_create t path =
    assert (not (Sys.file_exists path));
    btrfs t ["subvolume"; "create"; "--"; path]

  let subvolume_delete t path =
    btrfs t ~sudo:true ["subvolume"; "delete"; "--"; path]

  let subvolume_sync t path =
    btrfs ~sudo:true t ["subvolume"; "sync"; "--"; path]

  let subvolume_snapshot mode t ~src dst =
    assert (not (Sys.file_exists dst));
    let readonly =
      match mode with
      | `RO -> ["-r"]
      | `RW -> []
    in
    btrfs ~sudo:true t (["subvolume"; "snapshot"] @ readonly @ ["--"; src; dst])
end

let delete_snapshot_if_exists t path =
  match Os.check_dir path with
  | `Missing -> ()
  | `Present -> Btrfs.subvolume_delete t path

module Path = struct
  (* A btrfs store contains several subdirectories:

     - result: completed builds, named by ID
     - result-tmp: in-progress builds
     - state: for sqlite DB, etc
     - cache: the latest version of each cache, by cache ID
     - cache-tmp: in-progress updates to caches

     result-tmp and cache-tmp are wiped at start-up. *)

  let result t id        = t.root / "result" / id
  let result_tmp t id    = t.root / "result-tmp" / id
  let state t            = t.root / "state"
  let cache t name       = t.root / "cache" / Escape.cache name
  let cache_tmp t i name = t.root / "cache-tmp" / strf "%d-%s" i (Escape.cache name)
end

let delete t id =
  delete_snapshot_if_exists t (Path.result t id)

let purge t path =
  Sys.readdir path |> Array.to_list |> Fiber.iter (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Btrfs.subvolume_delete t item
    )

let check_kernel_version process =
  let kver = Switch.run @@ fun sw -> Os.pread ~sw ~process ["uname"; "-r"] in
  match String.split_on_char '.' kver with
  | maj :: min :: _ ->
      begin match int_of_string_opt maj, int_of_string_opt min with
      | Some maj, Some min when (maj, min) >= (5, 8) ->
          ()
      | Some maj, Some min ->
          failwith
            (Fmt.str
               "You need at least linux 5.8 to use the btrfs backend, \
                but current kernel version is '%d.%d'"
               maj min)
      | _, _ ->
          Fmt.failwith "Could not parse kernel version %S" kver
      end
  | _ ->
      Fmt.failwith "Could not parse output of 'uname -r' (%S)" kver

let create process root =
  check_kernel_version process;
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "result-tmp");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "cache");
  Os.ensure_dir (root / "cache-tmp");
  let t = { root; process; caches = Hashtbl.create 10; next = 0 } in 
  purge t (root / "result-tmp");
  purge t (root / "cache-tmp");
  t

let build t ?base ~id fn =
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  assert (not (Sys.file_exists result));        (* Builder should have checked first *)
  begin match base with
    | None -> Btrfs.subvolume_create t result_tmp
    | Some base -> Btrfs.subvolume_snapshot `RW t ~src:(Path.result t base) result_tmp
  end;
  try
    let r = fn result_tmp in
    begin match r with
      | Ok () -> Btrfs.subvolume_snapshot `RO t ~src:result_tmp result
      | Error _ -> ()
    end;
    Btrfs.subvolume_delete t result_tmp;
    r
  with ex ->
    Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
    Btrfs.subvolume_delete t result_tmp;
    raise ex

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name : (string * (unit -> unit)) =
  let cache = get_cache t name in
  Mutex.use_ro cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let snapshot = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  begin match Os.check_dir snapshot with
    | `Missing -> Btrfs.subvolume_create t snapshot
    | `Present -> ()
  end;
  (* Create writeable clone. *)
  let gen = cache.gen in
  Btrfs.subvolume_snapshot `RW t ~src:snapshot tmp;
  let { Obuilder_spec.uid; gid } = user in
  Switch.run @@ fun sw ->
  Os.sudo ~sw ~process:t.process ["chown"; Printf.sprintf "%d:%d" uid gid; tmp];
  let release () =
    Mutex.use_ro cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        Btrfs.subvolume_delete t snapshot;
        Btrfs.subvolume_snapshot `RO t ~src:tmp snapshot
      ) else ()
    end;
    Btrfs.subvolume_delete t tmp
  in
  (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Mutex.use_ro cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
    Btrfs.subvolume_delete t snapshot;
    Ok ()
  ) else Ok ()

let state_dir = Path.state

let complete_deletes t =
  Btrfs.subvolume_sync t t.root
