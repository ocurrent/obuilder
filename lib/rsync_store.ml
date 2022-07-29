(* The rsync backend is intended for stability, portability and testing. It
   is not supposed to be fast nor is it supposed to be particularly memory
   efficient. *)
open Lwt.Infix

(* The caching approach (and much of the code) is copied from the btrfs 
   implementation *)
type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type mode =
  | Copy
  | Hardlink
  | Hardlink_unsafe

type t = {
    path : string;
    mode : mode;
    caches : (string, cache) Hashtbl.t;
    mutable next : int;
}

let ( / ) = Filename.concat

module Rsync = struct
  let create dir = Lwt.return @@ Os.ensure_dir dir

  let delete dir =
    Os.sudo [ "rm"; "-r"; dir ]

  let rsync = [ "rsync"; "-aHq" ]

  let rename ~src ~dst =
    let cmd = [ "mv"; src; dst ] in
    Os.sudo cmd

  let rename_with_sharing ~mode ~base ~src ~dst = match mode, base with
    | Copy, _ | _, None -> rename ~src ~dst
    | _, Some base ->
        (* Attempt to hard-link existing files shared with [base] *)
        let safe = match mode with
          | Hardlink -> ["--checksum"]
          | _ -> []
        in
        let cmd = rsync @ safe @ ["--link-dest=" ^ base; src ^ "/"; dst ] in
        Os.ensure_dir dst;
        Os.sudo cmd >>= fun () ->
        delete src

  let copy_children ?chown ~src ~dst () =
    let chown = match chown with
      | Some uid_gid -> [ "--chown"; uid_gid ]
      | None -> []
    in
    let cmd = rsync @ chown @ [ src ^ "/"; dst ] in
    Os.ensure_dir dst;
    Os.sudo cmd
end

module Path = struct
  let state_dirname = "state"
  let cache_dirname = "cache"
  let cache_tmp_dirname = "cache-tmp"

  let result_dirname = "result"
  let result_tmp_dirname = "result-tmp"

  let dirs root =
    List.map ((/) root)
    [ state_dirname; cache_dirname; cache_tmp_dirname; result_dirname; result_tmp_dirname ]

  let result t id = t.path / result_dirname / id
  let cache t id = t.path / cache_dirname / id

  let cache_tmp t n id = t.path / cache_tmp_dirname / Printf.sprintf "%i-%s" n id

  let result_tmp t id = t.path / result_tmp_dirname / id
end

let create ~path ?(mode = Copy) () =
  Rsync.create path >>= fun () ->
  Lwt_list.iter_s Rsync.create (Path.dirs path) >|= fun () ->
  { path; mode; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id fn =
  Log.debug (fun f -> f "rsync: build %S" id);
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  let base = Option.map (Path.result t) base in
  begin match base with
  | None -> Rsync.create result_tmp
  | Some src -> Rsync.copy_children ~src ~dst:result_tmp ()
  end
  >>= fun () ->
  Lwt.try_bind
      (fun () -> fn result_tmp)
      (fun r ->
      begin match r with
          | Ok () -> Rsync.rename_with_sharing ~mode:t.mode ~base ~src:result_tmp ~dst:result
          | Error _ -> Lwt.return_unit
      end >>= fun () ->
      Lwt.return r
      )
  (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Rsync.delete result_tmp >>= fun () ->
      Lwt.fail ex
  )

let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
    | `Present -> Rsync.delete path
    | `Missing -> Lwt.return_unit

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

let state_dir t = t.path / Path.state_dirname

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let snapshot = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  begin match Os.check_dir snapshot with
      | `Missing -> Rsync.create snapshot
      | `Present -> Lwt.return_unit
  end >>= fun () ->
  (* Create writeable clone. *)
  let gen = cache.gen in
  let { Obuilder_spec.uid; gid } = user in
  Rsync.copy_children ~chown:(Printf.sprintf "%d:%d" uid gid) ~src:snapshot ~dst:tmp () >>= fun () ->
  let release () =
      Lwt_mutex.with_lock cache.lock @@ fun () ->
      begin
      if cache.gen = gen then (
          (* The cache hasn't changed since we cloned it. Update it. *)
          (* todo: check if it has actually changed. *)
          cache.gen <- cache.gen + 1;
          Rsync.delete snapshot >>= fun () ->
          Rsync.rename ~src:tmp ~dst:snapshot
      ) else Lwt.return_unit
      end
  in
  Lwt.return (tmp, release)


let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
      Rsync.delete snapshot >>= fun () ->
      Lwt_result.return ()
  ) else Lwt_result.return ()

(* Don't think this applies to rsync *)
let complete_deletes _t = Lwt.return ()
