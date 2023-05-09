(* The rsync backend is intended for stability, portability and testing. It
   is not supposed to be fast nor is it supposed to be particularly memory
   efficient. *)
open Eio

let ( / ) = Eio.Path.(/)

(* The caching approach (and much of the code) is copied from the btrfs 
   implementation *)
type cache = {
  lock : Mutex.t;
  mutable gen : int;
}

type mode =
  | Copy
  | Hardlink
  | Hardlink_unsafe

type t = {
    path : Eio.Fs.dir Eio.Path.t;
    mode : mode;
    process : Eio.Process.mgr;
    caches : (string, cache) Hashtbl.t;
    mutable next : int;
}

module Rsync = struct
  let create dir = Os.ensure_dir dir

  let delete dir =
    Os.sudo [ "rm"; "-r"; snd dir ]

  let rsync = [ "rsync"; "-aHq" ]

  let rename ~src ~dst =
    let cmd = [ "mv"; snd src; snd dst ] in
    Os.sudo cmd

  let rename_with_sharing ~process ~mode ~base ~src ~dst = 
    match mode, base with
    | Copy, _ | _, None -> rename ~process ~src ~dst
    | _, Some base ->
        (* Attempt to hard-link existing files shared with [base] *)
        let safe = match mode with
          | Hardlink -> ["--checksum"]
          | _ -> []
        in
        let cmd = rsync @ safe @ ["--link-dest=" ^ snd base; snd src ^ "/"; snd dst ] in
        Os.ensure_dir dst;
        Os.sudo ~process cmd;
        delete ~process src

  let copy_children ?chown ~src ~dst () =
    let chown = match chown with
      | Some uid_gid -> [ "--chown"; uid_gid ]
      | None -> []
    in
    let cmd = rsync @ chown @ [ snd src ^ "/"; snd dst ] in
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

let create ~process ~path ?(mode = Copy) () =
  Rsync.create path;
  List.iter Rsync.create (Path.dirs path);
  { path; process; mode; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id fn =
  Log.debug (fun f -> f "rsync: build %S" id);
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  let base = Option.map (Path.result t) base in
  begin match base with
  | None -> Rsync.create result_tmp
  | Some src -> Rsync.copy_children ~process:t.process ~src ~dst:result_tmp ()
  end;
  try
    let r = fn result_tmp in
    begin 
      match r with
      | Ok () -> Rsync.rename_with_sharing ~process:t.process ~mode:t.mode ~base ~src:result_tmp ~dst:result
      | Error _ -> ()
    end;
    r
  with ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Rsync.delete ~process:t.process result_tmp;
      raise ex

let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
    | `Present -> Rsync.delete ~process:t.process path
    | `Missing -> ()

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
    let c = { lock = Mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name =
  let cache = get_cache t name in
  Mutex.use_ro cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let snapshot = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  begin match Os.check_dir snapshot with
      | `Missing -> Rsync.create snapshot
      | `Present -> ()
  end;
  (* Create writeable clone. *)
  let gen = cache.gen in
  let { Obuilder_spec.uid; gid } = user in
  Rsync.copy_children ~process:t.process ~chown:(Printf.sprintf "%d:%d" uid gid) ~src:snapshot ~dst:tmp ();
  let release () =
      Mutex.use_ro cache.lock @@ fun () ->
      begin
      if cache.gen = gen then (
          (* The cache hasn't changed since we cloned it. Update it. *)
          (* todo: check if it has actually changed. *)
          cache.gen <- cache.gen + 1;
          Rsync.delete ~process:t.process snapshot;
          Rsync.rename ~process:t.process ~src:tmp ~dst:snapshot
      ) else ()
      end
  in
  (tmp, release)


let delete_cache t name =
  let cache = get_cache t name in
  Mutex.use_ro cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Path.cache t name in
  if Os.exists snapshot then (
      Rsync.delete ~process:t.process snapshot
  );
  Ok ()

(* Don't think this applies to rsync *)
let complete_deletes _t = ()
