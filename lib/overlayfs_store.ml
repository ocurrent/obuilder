(*
  Overlayfs creates a writable layer on top of an existing file system. e.g.

  mkdir {lower,upper,work,merge}
  mount -t overlay overlay -olowerdir=./lower,upperdir=./upper,workdir=./work ./merge

  ./lower would be our base image, ./upper is overlayed on top of ./lower resulting in ./merge.
  ./merge is r/w, with the writes held in ./upper.
  ./work is a temporary working directory.

  Overlayfs supports lowerdir being another overlayfs.  Sadly, the kernel source limits the depth of the stack to 2.

  #define FILESYSTEM_MAX_STACK_DEPTH 2

  However, lowerdir maybe a colon seperated list of lower layers which are stacked left to right.

  mount -t overlay overlay -olowerdir=./l1:./l2:./l3,upperdir=./upper,workdir=./work ./merge

  l1 being the lowerest layer with l2 being the middle layer with l3 on top.

  The layer stacking order is maintained using a symlink "parent" created in each lowerdir pointing to the parent.

  Overlayfs can be used on top of many other filesystem including ext4, xfs and tmpfs.  128GB tmpfs could be created as follows:

  mount -t tmpfs -o size=128g tmpfs /var/cache/obuilder
  ocluster-worker ... --obuilder-store=overlayfs:/var/cache/obuilder
 *)

open Lwt.Infix

type cache = {
  lock : Lwt_mutex.t;
  mutable children : int;
}

type t = {
  path : string;
  caches : (string, cache) Hashtbl.t;
  mutable next : int;
}

let ( / ) = Filename.concat

module Overlayfs = struct
  let create ?mode ?user dirs =
    match mode with
    | None -> Os.exec ([ "mkdir"; "-p" ] @ dirs)
    | Some mode -> Os.exec ([ "mkdir"; "-p"; "-m"; mode ] @ dirs) >>= fun () ->
      match user with
      | None -> Lwt.return_unit
      | Some `Unix user ->
        let { Obuilder_spec.uid; gid } = user in
        Os.sudo ([ "chown"; Printf.sprintf "%d:%d" uid gid; ] @ dirs)
      | Some `Windows _ -> assert false (* overlayfs not supported on Windows *)

  let delete dirs =
    match dirs with
    | [] -> Lwt.return_unit
    | d -> Os.sudo ([ "rm"; "-rf" ] @ d)

  let rename ~src ~dst =
    Os.sudo [ "mv"; src; dst ]

  let overlay ~lower ~upper ~work ~merged =
    Os.sudo [ "mount"; "-t"; "overlay"; "overlay"; "-olowerdir=" ^ lower ^ ",upperdir=" ^ upper ^ ",workdir=" ^ work; merged; ]

  let cp ~src ~dst = Os.sudo [ "cp"; "-plRduTf"; src; dst ]
  (*
   * -p     same as --preserve=mode,ownership,timestamps
   * -l     hard link files instead of copying
   * -R     copy directories recursively
   * -d     same as --no-dereference --preserve=links
   * -u     copy only when the SOURCE file is newer than the destination file or when the destination file is missing
   * -T     treat DEST as a normal file
   *)

  let umount ~merged = Os.sudo [ "umount"; merged ]
end

module Path = struct
  let state_dirname = "state"
  let cache_dirname = "cache"
  let cache_result_dirname = "cache-result"
  let cache_work_dirname = "cache-work"
  let cache_merged_dirname = "cache-merged"
  let result_dirname = "result"
  let in_progress_dirname = "in-progress"
  let merged_dirname = "merged"
  let work_dirname = "work"

  let dirs root =
    List.map (( / ) root)
      [ state_dirname;
        cache_dirname;
        cache_result_dirname;
        cache_work_dirname;
        cache_merged_dirname;
        result_dirname;
        in_progress_dirname;
        merged_dirname;
        work_dirname; ]

  let result t id = t.path / result_dirname / id
  let in_progress t id = t.path / in_progress_dirname / id
  let merged t id = t.path / merged_dirname / id
  let work t id = t.path / work_dirname / id

  let cache t name = t.path / cache_dirname / name
  let cache_result t n name =
    ( t.path / cache_result_dirname / name ^ "-" ^ Int.to_string n,
      t.path / cache_work_dirname / name ^ "-" ^ Int.to_string n,
      t.path / cache_merged_dirname / name ^ "-" ^ Int.to_string n)
end

let root t = t.path

let df t =
  Lwt_process.pread ("", [| "df"; "-k"; "--output=used,size"; t.path |])
  >>= fun output ->
  let used, blocks =
    String.split_on_char '\n' output
    |> List.filter_map (fun s ->
       match Scanf.sscanf s " %Ld %Ld " (fun used blocks -> (used, blocks)) with
       | used, blocks -> Some (Int64.to_float used, Int64.to_float blocks)
       | (exception Scanf.Scan_failure _) | (exception End_of_file) -> None)
    |> List.fold_left (fun (used, blocks) (u, b) -> (used +. u, blocks +. b)) (0., 0.)
  in
  Lwt.return (100. -. (100. *. (used /. blocks)))

let create ~path =
  Overlayfs.create (Path.dirs path) >>= fun () ->
  let parse_mtab s =
    match Scanf.sscanf s "%s %s %s %s %s %s" (fun _ mp _ _ _ _ -> mp) with
    | x -> Some x
    | (exception Scanf.Scan_failure _) | (exception End_of_file) -> None
  in
  let mounts =
    Os.read_lines "/etc/mtab" parse_mtab
    |> List.filter_map (function
      | Some x ->
         if String.length x > String.length path
             && String.starts_with ~prefix:path x
         then Some x
         else None
      | None -> None)
  in
  Lwt_list.iter_s
    (fun merged ->
      Log.warn (fun f -> f "Unmounting left-over folder %S" merged);
      Overlayfs.umount ~merged)
    mounts
  >>= fun () ->
  Lwt_list.iter_s
    (fun path ->
      Sys.readdir path |> Array.to_list
      |> List.map (Filename.concat path)
      |> Overlayfs.delete)
    [ path / Path.in_progress_dirname;
      path / Path.merged_dirname;
      path / Path.cache_result_dirname;
      path / Path.cache_work_dirname;
      path / Path.cache_merged_dirname;
      path / Path.work_dirname; ]
  >|= fun () -> { path; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id fn =
  Log.debug (fun f -> f "overlayfs: build %S" id);
  let result = Path.result t id in
  let in_progress = Path.in_progress t id in
  let merged = Path.merged t id in
  let work = Path.work t id in
  Overlayfs.create [ in_progress; work; merged ] >>= fun () ->
  let _ = Option.map (Path.in_progress t) base in
  (match base with
  | None ->
      Lwt.return_unit
  | Some src ->
      let src = Path.result t src in
      Unix.symlink src (in_progress / "parent");
      Unix.symlink (src / "env") (in_progress / "env");
      let rec ancestors src = src :: (match Os.read_link (src / "parent") with
          | Some p -> ancestors p
          | None -> [])
      in
      let lower = ancestors src |> String.concat ":" in
      Overlayfs.overlay ~lower ~upper:in_progress ~work ~merged)
  >>= fun () ->
  Lwt.try_bind
    (fun () -> match base with
      | None -> fn in_progress
      | Some _ -> fn merged)
    (fun r ->
      (match base with
      | None -> Lwt.return_unit
      | Some _ -> Overlayfs.umount ~merged)
      >>= fun () ->
      (match r with
      | Ok () ->
        Overlayfs.rename ~src:in_progress ~dst:result >>= fun () ->
        Overlayfs.delete [ merged; work ]
      | Error _ -> Overlayfs.delete [ merged; work; in_progress ])
      >>= fun () -> Lwt.return r)
    (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Overlayfs.delete [ merged; work; in_progress ] >>= fun () ->
      Lwt.reraise ex)

let delete t id =
  let path = Path.result t id in
  let results = t.path / Path.result_dirname in
  let rec decendants parent =
    Sys.readdir results
      |> Array.to_list
      |> List.map (Filename.concat results)
      |> List.filter (fun dir ->
        match Os.read_link (dir / "parent") with
        | Some p -> p = parent
        | None -> false)
      |> List.map decendants
      |> List.flatten
      |> List.append [ parent ]
  in decendants path
      |> Overlayfs.delete

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Lwt.return_some dir
  | `Missing -> Lwt.return_none

let log_file t id =
  result t id >|= function
  | Some dir -> dir / "log"
  | None -> Path.in_progress t id / "log"

let state_dir t = t.path / Path.state_dirname

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); children = 0 } in
      Hashtbl.add t.caches name c;
      c

let cache ~user t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let result, work, merged = Path.cache_result t t.next name in
  t.next <- t.next + 1;
  let master = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  (match Os.check_dir master with
  | `Missing -> Overlayfs.create ~mode:"1777" ~user [ master ]
  | `Present -> Lwt.return_unit)
  >>= fun () ->
  cache.children <- cache.children + 1;
  Overlayfs.create ~mode:"1777" ~user [ result; work; merged ] >>= fun () ->
  let lower = String.split_on_char ':' master |> String.concat "\\:" in
  Overlayfs.overlay ~lower ~upper:result ~work ~merged >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
       cache.children <- cache.children - 1;
       Overlayfs.umount ~merged >>= fun () ->
       Overlayfs.cp ~src:result ~dst:master >>= fun () ->
       Overlayfs.delete [ result; work; merged ]
  in
  Lwt.return (merged, release)

let delete_cache t name =
  let () = Printf.printf "0\n" in
  let cache = get_cache t name in
  let () = Printf.printf "1\n" in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let () = Printf.printf "2\n" in
  (* Ensures in-progress writes will be discarded *)
  if cache.children > 0
  then Lwt_result.fail `Busy
  else
    Overlayfs.delete [ Path.cache t name ] >>= fun () ->
    let () = Printf.printf "3\n" in
    Lwt.return (Ok ())

let complete_deletes _t = Lwt.return_unit
