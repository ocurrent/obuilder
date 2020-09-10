open Lwt.Infix

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

let write_config config dir =
  Lwt_io.(with_file ~mode:output) (dir / "config.json") @@ fun ch ->
  Lwt_io.write ch (Yojson.Safe.pretty_to_string config ^ "\n")

module Context = struct
  type t = {
    env : (string * string) list;       (* Environment in which to run commands. *)
    src_dir : string;                   (* Directory with files for copying. *)
    user : Dockerfile.user;             (* Container user to run as. *)
    workdir : string;                   (* Directory in the container namespace for cwd. *)
  }

  let default_env = [
    "PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
    "TERM", "xterm";
  ]

  let v ?(env=default_env) ?(user=Dockerfile.root) ?(workdir="/") ~src_dir () =
    { env; src_dir; user; workdir }
end

module Make (Store : S.STORE) = struct
  type t = {
    store : Store.t;
    runc_state_dir : string;
  }

  let run t ~base ~context cmd =
    let { Context.workdir; user; env; src_dir = _ } = context in
    let id =
      Digest.string
        ([%derive.show: string * string * (string * string) list * string]
           (Store.ID.show base, workdir, env, cmd))
      |> Digest.to_hex
    in
    Store.build t.store ~base ~id ~log:stdout (fun result_tmp ->
        let argv = [ "bash"; "-c"; cmd ] in
        let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env in
        write_config config result_tmp >>= fun () ->
        let log_file = result_tmp / "log" in
        Os.with_open_out log_file (fun log ->
            let out_r, out_w = Lwt_unix.pipe () in
            let out_w_closed = ref false in
            Lwt.finalize
              (fun () ->
                 let cmd = ["sudo"; "runc"; "--root"; t.runc_state_dir; "run"; id] in
                 let stdout = `FD_copy (Lwt_unix.unix_file_descr out_w) in
                 let stderr = stdout in
                 let copy_log = Os.tee ~src:out_r ~dst:log in
                 let proc = Os.exec ~cwd:result_tmp ~stdout ~stderr cmd in
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
          ) >|= fun () ->
        Ok ()
      )

  type copy_details = {
    base : Store.ID.t;
    src_manifest : Manifest.t list;
    user : Dockerfile.user;
    dst : string;
  } [@@deriving show]

  let copy t ~context ~base { Dockerfile.src; dst } =
    let { Context.src_dir; workdir; user; env = _ } = context in
    let dst = if Filename.is_relative dst then workdir / dst else dst in
    let src_manifest = List.map (Manifest.generate ~src_dir) src in
    let details = {
      base;
      src_manifest;
      user;
      dst;
    } in
    (* Fmt.pr "COPY: %a@." pp_copy_details details; *)
    let id = Digest.to_hex (Digest.string (show_copy_details details)) in
    Store.build t.store ~base ~id ~log:stdout (fun result_tmp ->
        let argv = ["tar"; "-xvf"; "-"] in
        let config = Config.v ~cwd:"/" ~argv ~hostname ~user ~env:["PATH", "/bin:/usr/bin"] in
        write_config config result_tmp >>= fun () ->
        let closed_r = ref false in
        let closed_w = ref false in
        let r, w = Lwt_unix.pipe_out () in
        Lwt.finalize
          (fun () ->
             Lwt_unix.set_close_on_exec w;
             let cmd = ["sudo"; "runc"; "--root"; t.runc_state_dir; "run"; id] in
             let stdin = `FD_copy r in
             let proc = Os.exec ~cwd:result_tmp ~stdin cmd in
             Unix.close r;
             closed_r := true;
             let send =
               (* If the sending thread finishes (or fails), close the writing socket
                  immediately so that the tar process finishes too. *)
               Lwt.finalize
                 (fun () -> Tar_transfer.send_files ~src_dir ~src_manifest ~dst ~to_untar:w)
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
        >|= Result.ok
      )

  let rec run_steps t ~(context:Context.t) ~base = function
    | [] -> Lwt_result.return base
    | op :: ops ->
      Fmt.pr "%s: %a@." context.workdir Dockerfile.pp_op op;
      let k = run_steps t ops in
      match op with
      | `Comment _ -> k ~base ~context
      | `Workdir workdir -> k ~base ~context:{context with workdir}
      | `User user -> k ~base ~context:{context with user}
      | `Run cmd ->
        run t ~base ~context cmd >>!= fun base ->
        k ~base ~context
      | `Copy x ->
        copy t ~context ~base x >>!= fun base ->
        k ~base ~context
      | `Env ((key, _) as e) ->
        let env = e :: (List.remove_assoc key context.env) in
        k ~base ~context:{context with env}

  let get_base t base =
    let id = Digest.to_hex (Digest.string base) in
    Store.build t.store ~id ~log:stdout (fun tmp ->
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
        Os.exec ["docker"; "rm"; "--"; cid] >|= Result.ok
      )

  let build t context { Dockerfile.from = base; ops } =
    get_base t base >>!= fun template_dir ->
    run_steps t ~context ~base:template_dir ops

  let v ~store ~runc_state_dir =
    { store; runc_state_dir }
end
