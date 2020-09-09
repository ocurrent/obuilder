open Lwt.Infix

let ( / ) = Filename.concat

let store = Store.create "/var/lib/docker/tal/"
let runc_state_dir = "/var/lib/docker/tal/state"
let context = "/var/lib/docker/tal/context"

let hostname = "builder"

let write_config config dir =
  Lwt_io.(with_file ~mode:output) (dir / "config.json") @@ fun ch ->
  Lwt_io.write ch (Yojson.Safe.pretty_to_string config ^ "\n")

let with_open_out path fn =
  Lwt_unix.openfile path Unix.[O_RDWR; O_CREAT] 0o644 >>= fun fd ->
  Lwt.finalize
    (fun () -> fn fd)
    (fun () -> Lwt_unix.close fd)

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)
  )

let tee ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n ->
      output stdout buf 0 n;
      flush stdout;
      write_all dst buf 0 n >>= aux
  in
  aux ()

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
      let log_file = result_tmp / "log" in
      with_open_out log_file (fun log ->
          let out_r, out_w = Lwt_unix.pipe () in
          let out_w_closed = ref false in
          Lwt.finalize
            (fun () ->
               let cmd = ["sudo"; "runc"; "--root"; runc_state_dir; "run"; hash] in
               let stdout = `FD_copy (Lwt_unix.unix_file_descr out_w) in
               let stderr = stdout in
               let copy_log = tee ~src:out_r ~dst:log in
               let proc = Process.exec ~cwd:result_tmp ~stdout ~stderr cmd in
               Lwt_unix.close out_w >>= fun () ->
               out_w_closed := true;
               proc >>= fun () ->
               copy_log
            )
            (fun () ->
               Lwt_unix.close out_r >>= fun () ->
               if !out_w_closed then Lwt.return_unit
               else Lwt_unix.close out_w
            )
        )
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

module Tar_lwt_unix = struct
  include Tar_lwt_unix

  (* Copied from tar_lwt_unix.ml (ISC license). Not sure why this isn't exposed.

     ## ISC License

     Copyright (c) 2012-2018 The ocaml-tar contributors

     Permission to use, copy, modify, and/or distribute this software for any
     purpose with or without fee is hereby granted, provided that the above
     copyright notice and this permission notice appear in all copies.

     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
     WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
     ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
     WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
     OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *)

  module Writer = struct
    type out_channel = Lwt_unix.file_descr
    type 'a t = 'a Lwt.t
    let really_write fd = Lwt_cstruct.(complete (write fd))
  end

  module HW = Tar.HeaderWriter(Lwt)(Writer)

  let write_block (header: Tar.Header.t) (body: Lwt_unix.file_descr -> unit Lwt.t) (fd : Lwt_unix.file_descr) =
    HW.write header fd
    >>= fun () ->
    body fd >>= fun () ->
    Writer.really_write fd (Tar.Header.zero_padding header)

  let write_end (fd: Lwt_unix.file_descr) =
    Writer.really_write fd Tar.Header.zero_block >>= fun () ->
    Writer.really_write fd Tar.Header.zero_block
end

let copy_to ~dst src =
  let len = 4096 in
  let buf = Bytes.create len in
  let rec aux () =
    Lwt_io.read_into src buf 0 len >>= function
    | 0 -> Lwt.return_unit
    | n -> write_all dst buf 0 n >>= aux
  in
  aux ()

let copy_file ~src ~dst ~to_untar =
  Lwt_unix.LargeFile.stat src >>= fun stat ->
  let hdr = Tar.Header.make
      ~file_mode:(if stat.Lwt_unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644)
      ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
      dst stat.Lwt_unix.LargeFile.st_size (* TODO: symlinks? *)
  in
  Tar_lwt_unix.write_block hdr (fun ofd ->
      Lwt_io.(with_file ~mode:input) src (copy_to ~dst:ofd)
    ) to_untar

let tar_send_files ~context ~src_manifest ~dst ~to_untar =
  src_manifest |> Lwt_list.iter_s (function
      | `File (path, _) ->
        let src = context / path in
        let dst = dst / (Filename.basename path) in       (* maybe don't copy docker's bad design here? *)
        copy_file ~src ~dst ~to_untar
      | `Dir (_src, _items) ->
        failwith "TODO"
        (*         copy_dir ~context ~src ~dst ~items *)
    )
  >>= fun () ->
  Tar_lwt_unix.write_end to_untar

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
      let argv = ["tar"; "-xvf"; "-"] in
      let config = Config.v ~cwd:"/" ~argv ~hostname ~user ~env:["PATH", "/bin:/usr/bin"] in
      write_config config result_tmp >>= fun () ->
      (* do_copy ~context ~src_manifest ~dst:(result_tmp / "rootfs" / dst) ~user *)

      let closed_r = ref false in
      let closed_w = ref false in
      let r, w = Lwt_unix.pipe_out () in
      Lwt.finalize
        (fun () ->
           Lwt_unix.set_close_on_exec w;
           let cmd = ["sudo"; "runc"; "--root"; runc_state_dir; "run"; hash] in
           let stdin = `FD_copy r in
           let proc = Process.exec ~cwd:result_tmp ~stdin cmd in
           Unix.close r;
           closed_r := true;
           let send =
             (* If the sending thread finishes (or fails), close the writing socket
                immediately so that the tar process finishes too. *)
             Lwt.finalize
               (fun () -> tar_send_files ~context ~src_manifest ~dst ~to_untar:w)
               (fun () ->
                  Lwt_unix.close w >|= fun () ->
                  closed_w := true;
               )
           in
           proc >>= fun () ->
           send
        )
        (fun () ->
           if not !closed_r then Unix.close r;
           if !closed_w then Lwt.return_unit
           else Lwt_unix.close w
        )
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
