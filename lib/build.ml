open Lwt.Infix

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

module Context = struct
  type t = {
    switch : Lwt_switch.t option;
    env : Os.env;                       (* Environment in which to run commands. *)
    src_dir : string;                   (* Directory with files for copying. *)
    user : Spec.user;                   (* Container user to run as. *)
    workdir : string;                   (* Directory in the container namespace for cwd. *)
    shell : string list;
    log : S.logger;
  }

  let v ?switch ?(env=[]) ?(user=Spec.root) ?(workdir="/") ?(shell=["/bin/bash"; "-c"]) ~log ~src_dir () =
    { switch; env; src_dir; user; workdir; shell; log }
end

module Saved_context = struct
  type t = {
    env : Os.env;
  } [@@deriving sexp]
end

module Make (Raw_store : S.STORE) (Sandbox : S.SANDBOX) = struct
  module Store = Db_store.Make(Raw_store)

  type t = {
    store : Store.t;
    sandbox : Sandbox.t;
  }

  let run t ~base ~context cmd =
    let { Context.switch; workdir; user; env; shell; log; src_dir = _ } = context in
    let id =
      Digest.string
        ([%derive.show: string * string * (string * string) list * string]
           (base, workdir, env, cmd))
      |> Digest.to_hex
    in
    Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
        let argv = shell @ [cmd] in
        let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env in
        Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
        Lwt_unix.close close_me >>= fun () ->
        Sandbox.run ~cancelled ~stdin ~log t.sandbox config result_tmp
      )

  type copy_details = {
    base : S.id [@printer Fmt.string];
    src_manifest : Manifest.t list;
    user : Spec.user;
    dst : string;
  } [@@deriving show]

  let copy t ~context ~base { Spec.src; dst; exclude } =
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _ } = context in
    let dst = if Filename.is_relative dst then workdir / dst else dst in
    let src_manifest = List.map (Manifest.generate ~exclude ~src_dir) src in
    let details = {
      base;
      src_manifest;
      user;
      dst;
    } in
    (* Fmt.pr "COPY: %a@." pp_copy_details details; *)
    let id = Digest.to_hex (Digest.string (show_copy_details details)) in
    Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
        let argv = ["tar"; "-xf"; "-"] in
        let config = Config.v ~cwd:"/" ~argv ~hostname ~user ~env:["PATH", "/bin:/usr/bin"] in
        Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
        let proc = Sandbox.run ~cancelled ~stdin:from_us ~log t.sandbox config result_tmp in
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
      context.log `Heading Fmt.(strf "%a" (pp_op ~context) op);
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
      | `Shell shell ->
        k ~base ~context:{context with shell}

  let pread argv =
    Os.with_pipe_from_child @@ fun ~r ~w ->
    let child = Os.exec ~stdout:(`FD_copy w.raw) argv in
    Os.close w;
    let r = Lwt_io.(of_fd ~mode:input) r in
    Lwt.finalize
      (fun () -> Lwt_io.read r)
      (fun () -> Lwt_io.close r)
    >>= fun line ->
    child >>= fun () ->
    Lwt.return line

  let export_env base : Os.env Lwt.t =
    pread ["docker"; "image"; "inspect";
           "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
           "--"; base] >|= fun env ->
    String.split_on_char '\x00' env
    |> List.filter_map (function
        | "\n" -> None
        | kv ->
          match Astring.String.cut ~sep:"=" kv with
          | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
          | Some _ as pair -> pair
      )

  let get_base t ~log base =
    log `Heading (Fmt.strf "FROM %s" base);
    let id = Digest.to_hex (Digest.string base) in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log:_ tmp ->
        Fmt.pr "Base image not present; importing %S...@." base;
        let rootfs = tmp / "rootfs" in
        Unix.mkdir rootfs 0o755;
        (* Lwt_process.exec ("", [| "docker"; "pull"; "--"; base |]) >>= fun _ -> *)
        pread ["docker"; "create"; "--"; base] >>= fun cid ->
        let cid = String.trim cid in
        let r, w = Unix.pipe ~cloexec:true () in
        let exporter, tar =
          Fun.protect
            (fun () ->
               let exporter = Os.exec ~stdout:(`FD_copy w) ["docker"; "export"; "--"; cid] in
               let tar = Os.exec ~stdin:(`FD_copy r) ["sudo"; "tar"; "-C"; rootfs; "-xf"; "-"] in
               exporter, tar
            )
            ~finally:(fun () ->
                Unix.close r;
                Unix.close w
              )
        in
        exporter >>= fun () ->
        tar >>= fun () ->
        Os.exec ["docker"; "rm"; "--"; cid] >>= fun () ->
        export_env base >>= fun env ->
        Os.write_file ~path:(tmp / "env")
          (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id ->
    let path = Option.get (Store.result t.store id) in
    let { Saved_context.env } = Saved_context.t_of_sexp (Sexplib.Sexp.load_sexp (path / "env")) in
    Lwt_result.return (id, env)

  let build t context { Spec.from = base; ops } =
    get_base t ~log:context.Context.log base >>!= fun (id, env) ->
    let context = { context with env = context.env @ env } in
    run_steps t ~context ~base:id ops

  let v ~store ~sandbox =
    let store = Store.wrap store in
    { store; sandbox }
end
