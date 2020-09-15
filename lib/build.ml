open Lwt.Infix

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

module Context = struct
  type t = {
    env : Os.env;                       (* Environment in which to run commands. *)
    src_dir : string;                   (* Directory with files for copying. *)
    user : Spec.user;                   (* Container user to run as. *)
    workdir : string;                   (* Directory in the container namespace for cwd. *)
  }

  let default_env = [
    "PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
    "TERM", "xterm";
  ]

  let v ?(env=default_env) ?(user=Spec.root) ?(workdir="/") ~src_dir () =
    { env; src_dir; user; workdir }
end

module Make (Store : S.STORE) (Sandbox : S.SANDBOX) = struct
  type t = {
    store : Store.t;
    sandbox : Sandbox.t;
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
        let config = Sandbox.Config.v ~cwd:workdir ~argv ~hostname ~user ~env in
        Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
        Lwt_unix.close close_me >>= fun () ->
        Sandbox.run ~stdin t.sandbox config result_tmp
      )

  type copy_details = {
    base : Store.ID.t;
    src_manifest : Manifest.t list;
    user : Spec.user;
    dst : string;
  } [@@deriving show]

  let copy t ~context ~base { Spec.src; dst } =
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
        let argv = ["tar"; "-xf"; "-"] in
        let config = Sandbox.Config.v ~cwd:"/" ~argv ~hostname ~user ~env:["PATH", "/bin:/usr/bin"] in
        Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
        let proc = Sandbox.run ~stdin:from_us t.sandbox config result_tmp in
        let send =
          (* If the sending thread finishes (or fails), close the writing socket
             immediately so that the tar process finishes too. *)
          Lwt.finalize
            (fun () -> Tar_transfer.send_files ~src_dir ~src_manifest ~dst ~to_untar)
            (fun () -> Lwt_unix.close to_untar)
        in
        proc >>= fun result ->
        send >>= fun () ->
        Lwt.return result
      )

  let pp_op ~(context:Context.t) f op =
    let sexp = Spec.sexp_of_op op in
    Fmt.pf f "@[<v2>%s: %a@]" context.workdir Sexplib.Sexp.pp_hum sexp

  let rec run_steps t ~(context:Context.t) ~base = function
    | [] -> Lwt_result.return base
    | op :: ops ->
      Fmt.pr "%a@." (Fmt.styled (`Fg (`Hi `Blue)) (pp_op ~context)) op;
      let k = run_steps t ops in
      match op with
      | `Comment _ -> k ~base ~context
      | `Workdir workdir -> k ~base ~context:{context with workdir}
      | `User user -> k ~base ~context:{context with user}
      | `Run { shell = cmd } ->
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

  let build t context { Spec.from = base; ops } =
    Fmt.pr "%a@." (Fmt.styled (`Fg (`Hi `Blue)) (Fmt.fmt "FROM %S")) base;
    get_base t base >>!= fun template_dir ->
    run_steps t ~context ~base:template_dir ops

  let v ~store ~sandbox =
    { store; sandbox }
end
