open Lwt.Infix

let ( / ) = Filename.concat

let store = Store.create "/var/lib/docker/tal/"
let runc_state_dir = "/var/lib/docker/tal/state"
let context = "/var/lib/docker/tal/context"

let hostname = "builder"

let write_config config dir =
  Lwt_io.(with_file ~mode:output) (dir / "config.json") @@ fun ch ->
  Lwt_io.write ch (Yojson.Safe.pretty_to_string config ^ "\n")

let run ~base ~workdir ~user ~env cmd =
  let hash =
    Digest.string
      ([%derive.show: string * string * (string * string) list * string]
         (Store.Tree.hash base, workdir, env, cmd))
    |> Digest.to_hex
  in
  Store.build store ~base ~hash (fun result_tmp ->
      let argv = [ "bash"; "-c"; cmd ] in
      let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env in
      write_config config result_tmp >>= fun () ->
      Process.exec ~cwd:result_tmp ["sudo"; "runc"; "--root"; runc_state_dir; "run"; hash]
    )

module Manifest = struct
  type t = [
    | `File of (string * string)
    | `Dir of (string * t list)
  ] [@@deriving show]

  let rec generate src : t =
    (* TODO: sanitise src *)
    let path = context / src in
    match Unix.lstat path with
    | Unix.{ st_kind = S_DIR; _ } ->
      let items =
        Sys.readdir path
        |> Array.to_list
        |> List.filter (( <> ) ".git")   (* TODO: .dockerignore *)
        |> List.map (fun item -> generate (src / item))
      in
      `Dir (src, items)
    | Unix.{ st_kind = S_REG; _ } ->
      let hash = Digest.file path in
      `File (src, hash)
    | _ -> Fmt.failwith "Unsupported file type for %S" src
    | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
      Fmt.failwith "File %S not found in context" src

  let digest t =
    Digest.to_hex (Digest.string (show t))
end

type copy_details = {
  base : Store.Tree.t;
  src_manifest : Manifest.t list;
  user : Dockerfile.user;
  dst : string;
} [@@deriving show]

let copy_file ~src ~dst ~user =
  Fmt.pr "Copy %S -> %S as %d@." src dst user.Dockerfile.uid;
  match Unix.lstat dst with
  | _ ->
    Fmt.pr "WARNING: Destination already exists: %S" dst; (* TODO: overwrite if safe *)
    Lwt.return_unit
  | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
    (* TODO: just run as root *)
    let { Dockerfile.uid; gid } = user in
    Process.exec ["sudo"; "cp"; "--"; src; dst] >>= fun () ->
    Process.exec ["sudo"; "chown"; Printf.sprintf "%d:%d" uid gid; dst]

let rec copy_dir ~context ~src ~dst ~user ~(items:(Manifest.t list))  =
  Fmt.pr "Copy dir %S -> %S as %d@." src dst user.Dockerfile.uid;
  begin 
    match Unix.lstat dst with
    | Unix.{ st_kind = S_DIR; _ } -> Lwt.return_unit
    | _ -> Fmt.failwith "Exists, but is not a directory: %S" dst
    | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
      (* TODO: just run as root *)
      let { Dockerfile.uid; gid } = user in
      Process.exec ["sudo"; "mkdir"; "--"; dst] >>= fun () ->
      Process.exec ["sudo"; "chown"; Printf.sprintf "%d:%d" uid gid; dst]
  end >>= fun () ->
  items |> Lwt_list.iter_s (function
      | `File (src, _) ->
        let src = context / src in
        let dst = dst / Filename.basename src in
        copy_file ~src ~dst ~user
      | `Dir (src, items) ->
        let dst = dst / Filename.basename src in
        copy_dir ~context ~src ~dst ~user ~items
    )

let do_copy ~context ~src_manifest ~dst ~user =
  src_manifest |> Lwt_list.iter_s (function
      | `File (path, _) ->
        let src = context / path in
        let dst = dst / (Filename.basename path) in       (* Maybe don't copy Docker's bad design here? *)
        copy_file ~src ~dst ~user
      | `Dir (src, items) ->
        copy_dir ~context ~src ~dst ~user ~items
    )

let copy ~base ~workdir ~user { Dockerfile.src; dst } =
  let dst = if Filename.is_relative dst then workdir / dst else dst in
  let src_manifest = List.map Manifest.generate src in
  let details = {
    base;
    src_manifest;
    user;
    dst;
  } in
  (* Fmt.pr "COPY: %a@." pp_copy_details details; *)
  let hash = Digest.to_hex (Digest.string (show_copy_details details)) in
  Store.build store ~base ~hash (fun result_tmp ->
    do_copy ~context ~src_manifest ~dst:(result_tmp / "rootfs" / dst) ~user
    )

let rec run_steps ~workdir ~user ~env ~base = function
  | [] -> Lwt.return base
  | op :: ops ->
    Fmt.pr "%s: %a@." workdir Dockerfile.pp_op op;
    let k = run_steps ops in
    match op with
    | `Comment _ -> k ~base ~workdir ~user ~env
    | `Workdir workdir -> k ~base ~workdir ~user ~env
    | `User user -> k ~base ~workdir ~user ~env
    | `Run cmd ->
      run ~base ~workdir ~user ~env cmd >>= fun base ->
      k ~base ~workdir ~user ~env
    | `Copy x ->
      copy ~base ~workdir ~user x >>= fun base ->
      k ~base ~workdir ~user ~env
    | `Env e ->
      k ~base ~workdir ~user ~env:(e :: env)

let get_base base =
  let hash = Digest.to_hex (Digest.string base) in
  Store.build store ~hash (fun tmp ->
      Fmt.pr "Base image not present; importing %S...@." base;
      let rootfs = tmp / "rootfs" in
      Unix.mkdir rootfs 0o755;
      (* Lwt_process.exec ("", [| "docker"; "pull"; "--"; base |]) >>= fun _ -> *)
      Lwt_process.pread_line ("", [| "docker"; "create"; "--"; base |]) >>= fun cid ->
      Fmt.pr "FROM %S -> %s@." base cid;
      let r, w = Unix.pipe () in
      let exporter = Lwt_process.open_process_none ~stdout:(`FD_move w) ("", [| "docker"; "export"; "--"; cid |]) in
      let tar = Lwt_process.open_process_none ~stdin:(`FD_move r) ("", [| "sudo"; "tar"; "-C"; rootfs; "-xf"; "-" |]) in
      exporter#status >>= fun _ ->
      tar#status >>= fun _ ->
      Process.exec ["docker"; "rm"; "--"; cid]
    )

let env = [
  "OPAMYES", "true";
  "PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
  "TERM", "xterm";
]

let build { Dockerfile.from = base; ops } =
  get_base base >>= fun template_dir ->
  run_steps ~base:template_dir ~workdir:"/" ~user:Dockerfile.root ~env ops

let () =
  Lwt_main.run begin
    build Example.dockerfile >>= fun x ->
    Fmt.pr "Got: %a@." Store.Tree.pp x;
    Lwt.return_unit
  end
