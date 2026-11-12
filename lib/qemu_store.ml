open Lwt.Infix

let strf = Printf.sprintf

let running_as_root = Unix.getuid () = 0

(* Represents a persistent cache.
   You must hold a cache's lock when removing or updating its entry in
   "cache". *)
type cache = {
  lock : Lwt_mutex.t;
  mutable children : int;
}

type t = {
  root : string;        (* The top-level directory (containing `result`, etc). *)
  caches : (string, cache) Hashtbl.t;
  mutable next : int;   (* Used to generate unique temporary IDs. *)
}

let ( / ) = Filename.concat

module Path = struct
  (* A qemu store contains several subdirectories:

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
  let image path         = path / "rootfs" / "image.qcow2"
end

module Qemu_img = struct
  let qemu_img ?(sudo=false) args =
    let args = "qemu-img" :: args in
    let args = if sudo && not running_as_root then "sudo" :: args else args in
    Os.exec ~stdout:`Dev_null args

  let snapshot ~src dst =
    Os.ensure_dir dst;
    Os.ensure_dir (dst / "rootfs");
    qemu_img (["create"; "-f"; "qcow2"; "-b"; Path.image src ; "-F"; "qcow2"; Path.image dst; "40G"])

  let create dst =
    Os.ensure_dir dst;
    Os.ensure_dir (dst / "rootfs");
    qemu_img (["create"; "-f"; "qcow2"; Path.image dst; "40G"])
end

let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present -> Os.rm ~directory:path

let purge path =
  Sys.readdir path |> Array.to_list |> Lwt_list.iter_s (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Os.rm ~directory:item
    )

let root t = t.root

module Stats = Map.Make (String)

let df t = Lwt.return (Os.free_space_percent t.root)

let create ~root =
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
    | None -> Lwt.return (Os.ensure_dir result_tmp)
    | Some base -> Qemu_img.snapshot ~src:(Path.result t base) result_tmp
  end
  >>= fun () ->
  Lwt.try_bind
    (fun () -> fn result_tmp)
    (fun r ->
       begin match r with
         | Ok () -> Os.mv ~src:result_tmp result
         | Error _ -> Os.rm ~directory:result_tmp
       end >>= fun () ->
       Lwt.return r
    )
  (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Os.rm ~directory:result_tmp >>= fun () ->
      Lwt.reraise ex
  )

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Lwt.return_some dir
  | `Missing -> Lwt.return_none

let log_file t id =
  result t id >|= function
  | Some dir -> dir / "log"
  | None -> (Path.result_tmp t id) / "log"

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); children = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ?(shared=false) ~user:_ t name : (string * (unit -> unit Lwt.t)) Lwt.t =
  let cache = get_cache t name in
  let master = Path.cache t name in
  if shared then
    (* Shared mode: return the actual cache directory, no copy-on-write *)
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    (* Create cache if it doesn't already exist. *)
    (match Os.check_dir master with
      | `Missing -> Qemu_img.create master
      | `Present -> Lwt.return ()) >>= fun () ->
    let release () = Lwt.return_unit in  (* No-op for shared caches *)
    Lwt.return (master, release)
  else
    (* Non-shared mode: existing copy behavior *)
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    let tmp = Path.cache_tmp t t.next name in
    t.next <- t.next + 1;
    (* Create cache if it doesn't already exist. *)
    (match Os.check_dir master with
      | `Missing -> Qemu_img.create master
      | `Present -> Lwt.return ()) >>= fun () ->
    cache.children <- cache.children + 1;
    let () = Os.ensure_dir tmp in
    Os.cp ~src:master tmp >>= fun () ->
    let release () =
      Lwt_mutex.with_lock cache.lock @@ fun () ->
      cache.children <- cache.children - 1;
      let cache_stat = Unix.stat (Path.image master) in
      let tmp_stat = Unix.stat (Path.image tmp) in
      (if tmp_stat.st_size > cache_stat.st_size then
        Os.cp ~src:tmp master
      else
        Lwt.return ()) >>= fun () ->
      Os.rm ~directory:tmp
    in
    Lwt.return (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  if cache.children > 0
  then Lwt_result.fail `Busy
  else
    let snapshot = Path.cache t name in
    if Sys.file_exists snapshot then (
      Os.rm ~directory:snapshot >>= fun () ->
      Lwt_result.return ()
    ) else Lwt_result.return ()

let state_dir = Path.state

let complete_deletes _ =
  Lwt.return_unit
