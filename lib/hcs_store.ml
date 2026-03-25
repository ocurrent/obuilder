open Lwt.Infix

let strf = Printf.sprintf

type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type t = {
  root : string;
  caches : (string, cache) Hashtbl.t;
  mutable next : int;
}

let ( / ) = Filename.concat

module Path = struct
  let result t id        = t.root / "result" / id
  let result_tmp t id    = t.root / "result-tmp" / id
  let state t            = t.root / "state"
  let cache t name       = t.root / "cache" / Escape.cache name
  let cache_tmp t i name = t.root / "cache-tmp" / strf "%d-%s" i (Escape.cache name)
end

module Ctr = struct
  let ctr_with_output args =
    if Sys.win32 then
      Os.win32_pread ("ctr" :: args)
    else begin
      let pp f = Os.pp_cmd f ("", "ctr" :: args) in
      Os.pread_all ~pp ("ctr" :: args) >>= fun (exit_code, stdout, stderr) ->
      if exit_code = 0 then
        Lwt_result.return stdout
      else begin
        Log.warn (fun f -> f "ctr %s failed (exit %d): stdout=%s stderr=%s"
          (String.concat " " args) exit_code stdout stderr);
        Lwt.return (Fmt.error_msg "ctr %s failed with exit status %d: %s"
          (String.concat " " args) exit_code stderr)
      end
    end

  let ctr args =
    ctr_with_output args >|= Result.map (fun _ -> ())

  let ctr_pread args =
    ctr_with_output args

  (* Prepare a writable snapshot from an optional parent.
     Uses --mounts to get JSON mount info in the output. *)
  let snapshot_prepare ~key ?parent () =
    let parent_args = match parent with
      | Some p -> [p]
      | None -> []
    in
    ctr_pread (["snapshot"; "prepare"; "--mounts"; key] @ parent_args)

  let snapshot_commit ~key ~committed_key () =
    ctr (["snapshot"; "commit"; committed_key; key])

  let snapshot_rm ~key () =
    ctr (["snapshot"; "rm"; key])

  let image_pull image =
    ctr (["image"; "pull"; image])
end

let snapshot_key id = "obuilder-" ^ id


let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present ->
    let rootfs = path / "rootfs" in
    let key =
      if Sys.file_exists (Hcs.layerinfo_path rootfs) then
        (Hcs.read_layerinfo rootfs).snapshot_key
      else if Sys.file_exists (Hcs.layerinfo_path path) then
        (Hcs.read_layerinfo path).snapshot_key
      else
        snapshot_key id
    in
    Log.info (fun f -> f "Deleting snapshot %s for result %s" key id);
    (Ctr.snapshot_rm ~key () >>= function
     | Ok () -> Lwt.return_unit
     | Error (`Msg m) ->
       Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
       Lwt.return_unit) >>= fun () ->
    (* Also try to remove the committed snapshot *)
    let committed_key = key ^ "-committed" in
    (Ctr.snapshot_rm ~key:committed_key () >>= function
     | Ok () -> Lwt.return_unit
     | Error (`Msg _) -> Lwt.return_unit) >>= fun () ->
    Os.rm ~directory:path

let purge path =
  Sys.readdir path |> Array.to_list |> Lwt_list.iter_s (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Os.rm ~directory:item
    )

let root t = t.root

let df t = Lwt.return (Os.free_space_percent t.root)

let create ~root =
  Os.ensure_dir root;
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
  assert (not (Sys.file_exists result));
  let key = snapshot_key id in
  begin match base with
    | None ->
      (* No parent — this is a base image import.
         The fetcher (fn) will handle snapshot preparation and write layerinfo.
         We just need to create the result_tmp directory. *)
      Os.ensure_dir result_tmp;
      Lwt.return_unit
    | Some base_id ->
      (* Build step with a parent — prepare a snapshot from the parent's committed snapshot. *)
      let parent_dir = Path.result t base_id in
      let parent_rootfs = parent_dir / "rootfs" in
      let parent_key =
        if Sys.file_exists (Hcs.layerinfo_path parent_rootfs) then
          (Hcs.read_layerinfo parent_rootfs).snapshot_key
        else if Sys.file_exists (Hcs.layerinfo_path parent_dir) then
          (Hcs.read_layerinfo parent_dir).snapshot_key
        else
          snapshot_key base_id
      in
      let parent = parent_key ^ "-committed" in
      (* Clean up any existing snapshot with this key (for idempotency) *)
      (Ctr.snapshot_rm ~key () >>= function
       | Ok () -> Log.info (fun f -> f "Removed existing snapshot %s" key); Lwt.return_unit
       | Error _ -> Lwt.return_unit) >>= fun () ->
      Log.info (fun f -> f "Preparing snapshot from parent %s" parent);
      Ctr.snapshot_prepare ~key ~parent () >>= function
      | Ok mounts_json ->
        let source, parent_layer_paths = Hcs.parse_mount_json mounts_json in
        Os.ensure_dir result_tmp;
        Hcs.write_layerinfo ~dir:result_tmp { snapshot_key = key; source; parent_layer_paths }
      | Error (`Msg m) ->
        Fmt.failwith "Failed to prepare snapshot %s: %s" key m
  end
  >>= fun () ->
  Lwt.try_bind
    (fun () -> fn result_tmp)
    (fun r ->
       begin match r with
         | Ok () ->
           let rootfs = result_tmp / "rootfs" in
           let snap_key =
             if Sys.file_exists (Hcs.layerinfo_path rootfs) then
               (Hcs.read_layerinfo rootfs).snapshot_key
             else if Sys.file_exists (Hcs.layerinfo_path result_tmp) then
               (Hcs.read_layerinfo result_tmp).snapshot_key
             else
               key
           in
           Log.info (fun f -> f "Snapshot key is %s" snap_key);
           let committed_key = snap_key ^ "-committed" in
           Log.info (fun f -> f "Committing snapshot %s -> %s" snap_key committed_key);
           (* Remove any existing committed snapshot first (idempotency) *)
           (Ctr.snapshot_rm ~key:committed_key () >>= function
            | Ok () -> Log.info (fun f -> f "Removed existing committed snapshot %s" committed_key); Lwt.return_unit
            | Error _ -> Lwt.return_unit) >>= fun () ->
           (Ctr.snapshot_commit ~key:snap_key ~committed_key () >>= function
            | Ok () -> Lwt.return_unit
            | Error (`Msg m) ->
              Fmt.failwith "Failed to commit snapshot %s: %s" snap_key m) >>= fun () ->
           (* On Windows, Sys.rename cannot replace an existing directory.
              Remove the target first if it exists (idempotency for retried builds). *)
           (if Sys.win32 && Sys.file_exists result then
              Os.rm ~directory:result
            else Lwt.return_unit) >>= fun () ->
           Os.mv ~src:result_tmp result
         | Error _ ->
           (* Clean up snapshot if we created one *)
           (if base <> None then
              Ctr.snapshot_rm ~key () >>= function
              | Ok () -> Lwt.return_unit
              | Error (`Msg m) ->
                Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
                Lwt.return_unit
            else Lwt.return_unit) >>= fun () ->
           Os.rm ~directory:result_tmp
       end >>= fun () ->
       Lwt.return r
    )
    (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      (if base <> None then
         Ctr.snapshot_rm ~key () >>= function
         | Ok () -> Lwt.return_unit
         | Error (`Msg m) ->
           Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
           Lwt.return_unit
       else Lwt.return_unit) >>= fun () ->
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

let state_dir = Path.state

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user:_ t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let master = Path.cache t name in
  begin match Os.check_dir master with
    | `Missing ->
      Os.ensure_dir master;
      Lwt.return_unit
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  let gen = cache.gen in
  Os.ensure_dir tmp;
  Os.cp ~src:master tmp >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        cache.gen <- cache.gen + 1;
        Os.rm ~directory:master >>= fun () ->
        Os.mv ~src:tmp master
      ) else
        Os.rm ~directory:tmp
    end
  in
  Lwt.return (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
    Os.rm ~directory:snapshot >>= fun () ->
    Lwt_result.return ()
  ) else Lwt_result.return ()

(* After pruning, try to remove obuilder-base-* containerd snapshots that
   are truly orphaned (no corresponding obuilder result directory).
   We must not remove base snapshots whose result directory still exists,
   as that would leave a stale cache entry that causes build failures. *)
let complete_deletes t =
  Ctr.ctr_pread ["snapshot"; "ls"] >>= function
  | Error _ -> Lwt.return_unit
  | Ok output ->
    let lines = String.split_on_char '\n' output in
    let base_keys = List.filter_map (fun line ->
      let parts = Astring.String.cuts ~empty:false ~sep:" " (String.trim line) in
      match parts with
      | key :: _ when Astring.String.is_prefix ~affix:"obuilder-base-" key -> Some key
      | _ -> None
    ) lines in
    Lwt_list.iter_s (fun key ->
      (* Extract the id from the snapshot key (strip "obuilder-base-" prefix
         and any "-committed" suffix) *)
      let id_part =
        let s = Astring.String.drop ~max:14 key in (* drop "obuilder-base-" *)
        match Astring.String.cut ~sep:"-committed" s with
        | Some (id, _) -> id
        | None -> s
      in
      let result_dir = Path.result t id_part in
      if Sys.file_exists result_dir then (
        Log.info (fun f -> f "Keeping base snapshot %s (result dir exists)" key);
        Lwt.return_unit
      ) else
        Ctr.snapshot_rm ~key () >>= function
        | Ok () ->
          Log.info (fun f -> f "Removed orphaned base snapshot %s" key);
          Lwt.return_unit
        | Error _ -> Lwt.return_unit
    ) base_keys
