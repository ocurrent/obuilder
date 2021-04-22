open Lwt.Infix

let strf = Printf.sprintf

let running_as_root = Unix.getuid () = 0

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

module Btrfs = struct
  let btrfs ?(sudo=false) args =
    let args = "btrfs" :: args in
    let args = if sudo && not running_as_root then "sudo" :: args else args in
    Os.exec ~stdout:`Dev_null args

  let subvolume_create path =
    assert (not (Sys.file_exists path));
    btrfs ["subvolume"; "create"; "--"; path]

  let subvolume_delete path =
    btrfs ~sudo:true ["subvolume"; "delete"; "--"; path]

  let subvolume_sync path =
    btrfs ~sudo:true ["subvolume"; "sync"; "--"; path]

  let subvolume_snapshot mode ~src dst =
    assert (not (Sys.file_exists dst));
    let readonly =
      match mode with
      | `RO -> ["-r"]
      | `RW -> []
    in
    btrfs ~sudo:true (["subvolume"; "snapshot"] @ readonly @ ["--"; src; dst])
end

let delete_snapshot_if_exists path =
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> Btrfs.subvolume_delete path

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
  delete_snapshot_if_exists (Path.result t id)

let purge path =
  Sys.readdir path |> Array.to_list |> Lwt_list.iter_s (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Btrfs.subvolume_delete item
    )

let check_kernel_version () =
  Os.pread ["uname"; "-r"] >>= fun kver ->
  match String.split_on_char '.' kver with
  | maj :: min :: _ ->
      begin match int_of_string_opt maj, int_of_string_opt min with
      | Some maj, Some min when (maj, min) >= (5, 8) ->
          Lwt.return_unit
      | Some maj, Some min ->
          Lwt.fail_with
            (Fmt.str
               "You need at least linux 5.8 to use the btrfs backend, \
                but current kernel version is '%d.%d'"
               maj min)
      | _, _ ->
          Fmt.failwith "Could not parse kernel version %S" kver
      end
  | _ ->
      Fmt.failwith "Could not parse output of 'uname -r' (%S)" kver

let create root =
  check_kernel_version () >>= fun () ->
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "result-tmp");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "cache");
  Os.ensure_dir (root / "cache-tmp");
  purge (root / "result-tmp") >>= fun () ->
  purge (root / "cache-tmp") >>= fun () ->
  Lwt.return { root; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id fn =
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  assert (not (Sys.file_exists result));        (* Builder should have checked first *)
  begin match base with
    | None -> Btrfs.subvolume_create result_tmp
    | Some base -> Btrfs.subvolume_snapshot `RW ~src:(Path.result t base) result_tmp
  end
  >>= fun () ->
  Lwt.try_bind
    (fun () -> fn result_tmp)
    (fun r ->
       begin match r with
         | Ok () -> Btrfs.subvolume_snapshot `RO ~src:result_tmp result
         | Error _ -> Lwt.return_unit
       end >>= fun () ->
       Btrfs.subvolume_delete result_tmp >>= fun () ->
       Lwt.return r
    )
  (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Btrfs.subvolume_delete result_tmp >>= fun () ->
      Lwt.fail ex
  )

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

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
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let snapshot = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  begin match Os.check_dir snapshot with
    | `Missing -> Btrfs.subvolume_create snapshot
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  (* Create writeable clone. *)
  let gen = cache.gen in
  Btrfs.subvolume_snapshot `RW ~src:snapshot tmp >>= fun () ->
  let { Obuilder_spec.uid; gid } = user in
  Os.sudo ["chown"; Printf.sprintf "%d:%d" uid gid; tmp] >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        Btrfs.subvolume_delete snapshot >>= fun () ->
        Btrfs.subvolume_snapshot `RO ~src:tmp snapshot
      ) else Lwt.return_unit
    end >>= fun () ->
    Btrfs.subvolume_delete tmp
  in
  Lwt.return (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
    Btrfs.subvolume_delete snapshot >>= fun () ->
    Lwt_result.return ()
  ) else Lwt_result.return ()

let state_dir = Path.state

let complete_deletes t =
  Btrfs.subvolume_sync t.root
