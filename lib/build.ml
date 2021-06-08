open Lwt.Infix
open Sexplib.Std

let ( / ) = Filename.concat
let ( // ) p1 p2 = if Sys.win32 then p1 ^ "/" ^ p2 else Filename.concat p1 p2
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

let healthcheck_base = if Sys.win32 then "mcr.microsoft.com/windows/servercore:20H2" else "busybox"
let healthcheck_ops =
  let open Obuilder_spec in
  [
    shell (if Sys.win32 then ["cmd"; "/S"; "/C"] else ["/bin/sh"; "-c"]);
    run "echo healthcheck"
  ]

module Scope = Map.Make(String)

module Context = struct
  type t = {
    switch : Lwt_switch.t option;
    env : Config.env;                   (* Environment in which to run commands. *)
    src_dir : string;                   (* Directory with files for copying. *)
    user : Obuilder_spec.user;          (* Container user to run as. *)
    workdir : string;                   (* Directory in the container namespace for cwd. *)
    shell : string list;
    log : S.logger;
    scope : string Scope.t;             (* Nested builds that are in scope. *)
    secrets : (string * string) list;
  }

  let v ?switch ?(env=[]) ?(user=Obuilder_spec.root) ?workdir ?shell ?(secrets=[]) ~log ~src_dir () =
    let workdir = Option.value ~default:(if Sys.win32 then {|C:\|} else "/") workdir in
    let shell = Option.value ~default:(if Sys.win32 then ["cmd"; "/S"; "/C"] else ["/bin/bash"; "-c"]) shell in
    { switch; env; src_dir; user; workdir; shell; log; scope = Scope.empty; secrets }

  let with_binding name value t =
    { t with scope = Scope.add name value t.scope }
end

module Saved_context = struct
  type t = {
    env : Config.env;
  } [@@deriving sexp]
end

module Make (Raw_store : S.STORE) (Sandbox : S.SANDBOX) (Fetch : S.FETCHER) = struct
  module Store = Db_store.Make(Raw_store)

  type t = {
    store : Store.t;
    sandbox : Sandbox.t;
  }

  (* Inputs to run that should affect the hash. i.e. if anything in here changes
     then we need a fresh build. *)
  type run_input = {
    base : S.id;
    workdir : string;
    user : Obuilder_spec.user;
    env : Config.env;
    cmd : string;
    shell : string list;
    network : string list;
    mount_secrets : Config.Secret.t list;
  } [@@deriving sexp_of]

  let docker_teardown_sandbox id ~commit =
    let container = Docker.docker_container id in
    let base_image = Docker.docker_image ~tmp:true id in
    let target_image = Docker.docker_image id in
    begin if commit then Docker.commit base_image container target_image else Lwt.return_unit end >>= fun () ->
    Docker.rm [container]

  let run t ~switch ~log ~cache run_input =
    let id =
      sexp_of_run_input run_input
      |> Sexplib.Sexp.to_string_mach
      |> Sha256.string
      |> Sha256.to_hex
    in
    let { base; workdir; user; env; cmd; shell; network; mount_secrets } = run_input in
    Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
        let to_release = ref [] in
        Lwt.finalize
          (fun () ->
             cache |> Lwt_list.map_s (fun { Obuilder_spec.Cache.id; target; buildkit_options = _ } ->
                 Store.cache ~user t.store id >|= fun (src, release) ->
                 to_release := release :: !to_release;
                 { Config.Mount.src; dst = target; readonly = false }
               )
             >>= fun mounts ->
             let argv = shell @ [cmd] in
             let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env ~mounts ~mount_secrets ~network () in
             Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
             Lwt_unix.close close_me >>= fun () ->
             Lwt_result.bind_lwt
               (Sandbox.run ~cancelled ~stdin ~log t.sandbox config result_tmp)
               (fun () -> if Sandbox.backend = `Docker then docker_teardown_sandbox id ~commit:true
                          else Lwt.return_unit)
          )
          (fun () ->
             !to_release |> Lwt_list.iter_s (fun f -> f ())
          )
      )

  type copy_details = {
    base : S.id;
    user : Obuilder_spec.user;
    op : [`Copy_items of Manifest.t list * string | `Copy_item of Manifest.t * string];
  } [@@deriving sexp_of]

  let rec sequence = function
    | [] -> Ok []
    | Error e :: _ -> Error e
    | Ok x :: xs ->
      match sequence xs with
      | Ok xs -> Ok (x :: xs)
      | e -> e

  let to_copy_op ~dst = function
    | [] -> Fmt.error_msg "No source items for copy!"
    | items when dst.[String.length dst - 1] = '/' -> Ok (`Copy_items (items, dst))
    | [item] -> Ok (`Copy_item (item, dst))
    | _ -> Fmt.error_msg "When copying multiple items, the destination must end with '/'"

  let mount_point_inside_unix = if Sys.win32 then "/cygdrive/c" else "/var/lib/obuilder"
  let mount_point_inside_native = if Sys.win32 then {|C:\|} else mount_point_inside_unix

  let docker_manifest_from_build ~base ~exclude src user =
    let obuilder_volume = Docker.obuilder_volume in
    let docker_argv = [
        "--mount"; Printf.sprintf "type=volume,src=%s,dst=%s,readonly" obuilder_volume (mount_point_inside_native / obuilder_volume);
        "--entrypoint"; if Sys.win32 then mount_point_inside_native / obuilder_volume / "bash.exe" else "bash";
        "--env"; Printf.sprintf "PATH=%s" (if Sys.win32 then mount_point_inside_unix // obuilder_volume else "/bin:/usr/bin");
        "--workdir"; if Sys.win32 then {|C:\|} else "/";
        "--user"; match user with `Unix { Obuilder_spec.uid; gid } -> Printf.sprintf "%d:%d" uid gid | `Windows { Obuilder_spec.name } -> name
        (* FIXME: we don't have access to isolation type here. *)
    ] in
    let manifest_sh =
      (* FIXME: does Filename.quote always use Bash quoting rules? *)
      Printf.sprintf "exec %s %S %S %d %s %d %s"
        (mount_point_inside_unix // Docker.obuilder_volume // "manifest.sh")
        "."
        "/"
        (List.length exclude)
        (String.concat " " (List.map Filename.quote exclude))
        (List.length src)
        (String.concat " " (List.map Filename.quote src))
    in
    let argv = [ "--login"; "-c"; manifest_sh ] in
    let pp f = Os.pp_cmd f ["Generating source manifest"] in
    Docker.run_pread_result ~pp ~rm:true docker_argv (Docker.docker_image base) argv >>!= fun manifests ->
    match Parsexp.Many.parse_string manifests with
    | Ok ts -> List.rev_map Manifest.t_of_sexp ts |> Lwt.return |> Lwt_result.ok
    | Error e -> Lwt_result.fail (`Msg (Parsexp.Parse_error.message e))

  let docker_tarball_from_context ~src_dir mount_point op user =
    let name = if Sys.win32 then mount_point / "archive.tar" else Filename.temp_file "obuilder-" "" in
    Log.debug (fun f -> f "docker_tarball_from_context name:%s" name);
    Lwt_unix.openfile name [O_WRONLY; O_CREAT] 0o600 >>= fun to_untar ->
    (* If the sending thread finishes (or fails), close the writing socket
       immediately so that the tar process finishes too. *)
    Lwt.finalize
      (fun () ->
        Lwt.try_bind
          (fun () ->
            match op with
            | `Copy_items (src_manifest, dst_dir) ->
              Tar_transfer.send_files ~src_dir ~src_manifest ~dst_dir ~to_untar ~user
            | `Copy_item (src_manifest, dst) ->
              Tar_transfer.send_file ~src_dir ~src_manifest ~dst ~to_untar ~user)
          (fun () ->
            Lwt_unix.close to_untar >>= fun () ->
            if not (Sys.win32) then Os.sudo ["cp"; "--"; name; mount_point / "archive.tar"]
            else Lwt.return_unit)
          (fun exn -> Lwt_unix.close to_untar >>= fun () -> Lwt.fail exn))
      (fun () -> if not (Sys.win32) then Lwt_unix.unlink name else Lwt.return_unit)

  let docker_tar_transfer ~dst op ch =
    let copy_root dst acc t =
      (* FIXME: escape dst, src, path for regexp? *)
      begin match t with
      | `File (path, _) | `Symlink (path, _) ->
        Lwt_io.fprint ch path >>= fun () ->
        Lwt.return ("--transform" :: (Printf.sprintf "s,^%s$,%s," path dst) :: acc)
      | `Dir (src, _) as dir ->
        Lwt_io.fprint ch (String.concat "\n" (Manifest.to_list dir)) >>= fun () ->
        Lwt.return ("--transform" :: (Printf.sprintf "s,^%s,%s," (Filename.dirname src) dst) :: acc)
      end >>= fun tar_argv -> Lwt_io.fprint ch "\n" >>= fun () -> Lwt.return tar_argv
    in
    match op with
    | `Copy_items (src_manifest, _) -> begin Lwt_list.fold_left_s (copy_root dst) [] src_manifest end
    | `Copy_item (src_manifest, _) -> begin copy_root dst [] (src_manifest:Manifest.t) end

  let docker_tarball_from_build t ~log ~dst ~from (`Docker_volume volume) mount_point op user =
    Log.debug (fun f -> f "docker_tarball_from_build");
    begin
      let name = mount_point / "manifest" in
      if Sys.win32 then Lwt_io.with_file ~mode:Output name (docker_tar_transfer ~dst op)
      else Lwt_io.with_temp_file (fun (tmp, ch) ->
        docker_tar_transfer ~dst op ch >>= fun tar_argv ->
        Os.sudo ["cp"; "-a"; "--"; tmp; name] >>= fun () ->
        Lwt.return tar_argv)
    end >>= fun tar_argv ->
    let argv =
      ["--login"; "-c";
       String.concat " "
         ([ if Sys.win32 then "tar.exe" else "tar";
            "-cf"; mount_point_inside_unix // ("obuilder-" ^ volume) // "archive.tar";
            "--absolute-names"; "--no-recursion";
            "--show-transformed-names"; "-v"; (* for debugging *)
            "--files-from"; mount_point_inside_unix // ("obuilder-" ^ volume) // "manifest" ]
          @ tar_argv)]
    in
    let config =
      Config.v
        ~cwd:(if Sys.win32 then {|C:\|} else "/")
        ~argv
        ~hostname
        ~user
        ~env:["PATH", if Sys.win32 then mount_point_inside_unix // Docker.obuilder_volume else "/bin:/usr/bin"]
        ~mount_secrets:[]
        ~mounts:Config.Mount.[
          {src = volume; dst = mount_point_inside_native / ("obuilder-" ^ volume); readonly = false};
          {src = Docker.obuilder_volume; dst = mount_point_inside_native / Docker.obuilder_volume; readonly = true}]
        ~network:[]
        ~entrypoint:(if Sys.win32 then mount_point_inside_native / Docker.obuilder_volume / "bash.exe" else "/bin/bash")
        ()
    in
    let from_tmp = Docker.docker_image ~tmp:true from in
    Docker.tag (Docker.docker_image from) from_tmp >>= fun () ->
    (* Bypass db_store. *)
    let cancelled, set_cancelled = Lwt.wait () in
    Lwt.finalize
      (fun () ->
        Lwt_result.bind_lwt
          (Sandbox.run ~cancelled ~log t.sandbox config from)
          (fun () -> docker_teardown_sandbox from ~commit:false))
      (fun () ->
        Docker.image "rm" from_tmp >>= fun () ->
        Lwt.wakeup_later set_cancelled ();
        Lwt.return_unit)

  let copy_docker t ?switch ~base ~log ~exclude ~src_dir ~dst src user =
    begin match src_dir with
      | `Context src_dir -> sequence (List.map (Manifest.generate ~exclude ~src_dir) src) |> Lwt.return
      | `Build (id, _) -> docker_manifest_from_build ~base:id ~exclude src user
    end >>= fun src_manifest ->
    match Result.bind src_manifest (to_copy_op ~dst) with
    | Error _ as e -> Lwt.return e
    | Ok op ->
       let details = {
         base;
         op;
         user;
       } in
       Log.debug (fun f -> f "COPY: %a@." Sexplib.Sexp.pp_hum (sexp_of_copy_details details));
       let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
       Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
           let volume = `Docker_volume id in
           Docker.volume ["create"] volume >>= fun _ ->
           Lwt.finalize
             (fun () ->
               Docker.mount_point volume >>= fun mount_point ->
               Log.debug (fun f -> f "Generating tarball");
               begin match src_dir with
               | `Context src_dir -> docker_tarball_from_context ~src_dir mount_point op user |> Lwt_result.ok
               | `Build (from, _) -> docker_tarball_from_build t ~log ~dst ~from volume mount_point op user
               end >>!= fun () ->
                 let argv = ["-xf"; mount_point_inside_unix // id // "archive.tar"; "--absolute-names";
                             "-v" (* for debugging *) ]
                   |> if not Sys.win32 then List.cons "tar" else Fun.id
               in
               let config =
                 let mounts =
                   [Config.Mount.{ src = id; dst = mount_point_inside_native / id; readonly = false }]
                   |> if Sys.win32 then List.cons Config.Mount.{
                          src = Docker.obuilder_volume;
                          dst = mount_point_inside_native / Docker.obuilder_volume;
                          readonly = true; }
                      else Fun.id
                 in
                 let entrypoint =
                   if Sys.win32 then Printf.sprintf {|C:\%s\tar.exe|} Docker.obuilder_volume
                   else "/usr/bin/env" in
                 Config.v
                   ~cwd:(if Sys.win32 then {|C:\|} else "/")
                   ~argv
                   ~hostname
                   ~user:Obuilder_spec.root
                   ~env:[]
                   ~mount_secrets:[]
                   ~mounts
                   ~network:[]
                   ~entrypoint
                   ()
               in
               Log.debug (fun f -> f "Sandbox is running");
               Lwt_result.bind_lwt
                 (Sandbox.run ~cancelled ~log t.sandbox config result_tmp)
                 (fun () -> docker_teardown_sandbox id ~commit:true))
             (fun () -> Docker.volume ["rm"] volume >>= fun _ -> Lwt.return_unit))

  let copy_runc t ?switch ~base ~log ~exclude ~src_dir ~dst src user =
    let src_dir = match src_dir with `Context src_dir -> src_dir | `Build (_, src_dir) -> src_dir in
    let src_manifest = sequence (List.map (Manifest.generate ~exclude ~src_dir) src) in
    match Result.bind src_manifest (to_copy_op ~dst) with
    | Error _ as e -> Lwt.return e
    | Ok op ->
      let details = {
        base;
        op;
        user;
      } in
      Log.debug (fun f -> f "COPY: %a@." Sexplib.Sexp.pp_hum (sexp_of_copy_details details));
      let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
      Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
          let argv = ["tar"; "-xf"; "-"] in
          let config = Config.v
              ~cwd:"/"
              ~argv
              ~hostname
              ~user:Obuilder_spec.root
              ~env:["PATH", "/bin:/usr/bin"]
              ~mount_secrets:[]
              ~mounts:[]
              ~network:[]
              ()
          in
          Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
          let proc = Sandbox.run ~cancelled ~stdin:from_us ~log t.sandbox config result_tmp in
          let send =
            (* If the sending thread finishes (or fails), close the writing
               socket immediately so that the tar process finishes too. *)
            Lwt.finalize
              (fun () ->
                match op with
                | `Copy_items (src_manifest, dst_dir) ->
                   Tar_transfer.send_files ~src_dir ~src_manifest ~dst_dir ~to_untar ~user
                | `Copy_item (src_manifest, dst) ->
                   Tar_transfer.send_file ~src_dir ~src_manifest ~dst ~to_untar ~user
              )
              (fun () -> Lwt_unix.close to_untar)
          in
          proc >>= fun result ->
          send >>= fun () ->
          Lwt.return result
      )

  let copy t ~context ~base { Obuilder_spec.from; src; dst; exclude } =
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _; scope; secrets = _ } = context in
    let dst = if Filename.is_relative dst then workdir / dst else dst in
    begin
      match from with
      | `Context -> Lwt_result.return (`Context src_dir)
      | `Build name ->
        match Scope.find_opt name scope with
        | None -> Fmt.failwith "Unknown build %S" name   (* (shouldn't happen; gets caught earlier) *)
        | Some id ->
          Store.result t.store id >>= function
          | None ->
            Lwt_result.fail (`Msg (Fmt.str "Build result %S not found" id))
          | Some dir ->
            Lwt_result.return (`Build (id, dir / "rootfs"))
    end >>!= fun src_dir ->
    match Sandbox.backend with
    | `Runc -> copy_runc t ?switch ~base ~log ~exclude ~src_dir ~dst src user
    | `Docker -> copy_docker t ?switch ~base ~log ~exclude ~src_dir ~dst src user
    | _ -> assert false

  let pp_op ~(context:Context.t) f op =
    Fmt.pf f "@[<v2>%s: %a@]" context.workdir Obuilder_spec.pp_op op

  let update_workdir ~(context:Context.t) path =
    let workdir =
      if Astring.String.is_prefix ~affix:(if Sys.win32 then {|C:\|} else "/") path then path
      else Filename.concat context.workdir path
    in
    { context with workdir }

  let mount_secret (values : (string * string) list) (secret: Obuilder_spec.Secret.t) =
    match List.assoc_opt secret.id values with
    | None -> Error (`Msg ("Couldn't find value for requested secret '"^secret.id^"'") )
    | Some value -> Ok Config.Secret.{value; target=secret.target}

  let resolve_secrets (values : (string * string) list) (secrets: Obuilder_spec.Secret.t list) =
    let (>>=) = Result.bind in
    let (>>|) x y = Result.map y x in
    List.fold_left (fun result secret ->
      result >>= fun result ->
      mount_secret values secret >>| fun resolved_secret ->
      (resolved_secret :: result) ) (Ok []) secrets

  let rec run_steps t ~(context:Context.t) ~base = function
    | [] -> Lwt_result.return base
    | op :: ops ->
      context.log `Heading Fmt.(strf "%a" (pp_op ~context) op);
      let k = run_steps t ops in
      match op with
      | `Comment _ -> k ~base ~context
      | `Workdir workdir -> k ~base ~context:(update_workdir ~context workdir)
      | `User user -> k ~base ~context:{context with user}
      | `Run { shell = cmd; cache; network; secrets = mount_secrets } ->
        let result =
          let { Context.switch; workdir; user; env; shell; log; src_dir = _; scope = _; secrets } = context in
          resolve_secrets secrets mount_secrets |> Result.map @@ fun mount_secrets ->
          (switch, { base; workdir; user; env; cmd; shell; network; mount_secrets }, log)
        in
        Lwt.return result >>!= fun (switch, run_input, log) ->
        run t ~switch ~log ~cache run_input >>!= fun base ->
        k ~base ~context
      | `Copy x ->
        copy t ~context ~base x >>!= fun base ->
        k ~base ~context
      | `Env ((key, _) as e) ->
        let env = e :: (List.remove_assoc key context.env) in
        k ~base ~context:{context with env}
      | `Shell shell ->
        k ~base ~context:{context with shell}

  let tag_base base id =
    if Sandbox.backend = `Docker then
      Docker.tag (`Docker_image base) (Docker.docker_image id)
    else
      Lwt.return_unit

  let get_base t ~log base =
    log `Heading (Fmt.strf "(from %a)" Sexplib.Sexp.pp_hum (Atom base));
    let id = Sha256.to_hex (Sha256.string base) in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log tmp ->
        Log.info (fun f -> f "Base image not present; importing %S..." base);
        let rootfs = tmp / "rootfs" in
        Os.sudo ["mkdir"; "--mode=755"; "-p"; "--"; rootfs] >>= fun () ->
        Fetch.fetch ~log ~rootfs base >>= fun env ->
        Os.write_file ~path:(tmp / "env")
          (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
        tag_base base id >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id -> Store.result t.store id
    >|= Option.get >>= fun path ->
    Lwt_unix.file_exists (path / "env") >>= fun b -> begin
    if b then Saved_context.t_of_sexp (Sexplib.Sexp.load_sexp (path / "env"))
    else { Saved_context.env = [] } end |> Lwt.return
    >>= fun { Saved_context.env } ->
    Lwt_result.return (id, env)

  let rec build ~scope t context { Obuilder_spec.child_builds; from = base; ops } =
    let rec aux context = function
      | [] -> Lwt_result.return context
      | (name, child_spec) :: child_builds ->
        context.Context.log `Heading Fmt.(strf "(build %S ...)" name);
        build ~scope t context child_spec >>!= fun child_result ->
        context.Context.log `Note Fmt.(strf "--> finished %S" name);
        let context = Context.with_binding name child_result context in
        aux context child_builds
    in
    aux context child_builds >>!= fun context ->
    get_base t ~log:context.Context.log base >>!= fun (id, env) ->
    let context = { context with env = context.env @ env } in
    run_steps t ~context ~base:id ops

  let build t context spec =
    let r = build ~scope:[] t context spec in
    (r : (string, [ `Cancelled | `Msg of string ]) Lwt_result.t :> (string, [> `Cancelled | `Msg of string ]) Lwt_result.t)

  let delete ?log t id =
    Store.delete ?log t.store id

  let prune ?log t ~before limit =
    Store.prune ?log t.store ~before limit

  let log_to buffer tag x =
    match tag with
    | `Heading | `Note -> Buffer.add_string buffer (x ^ "\n")
    | `Output -> Buffer.add_string buffer x

  let healthcheck ?(timeout=30.0) t =
    Os.with_pipe_from_child (fun ~r ~w ->
        let pp f = Fmt.string f "docker version" in
        let result = Os.exec_result ~pp ~stdout:`Dev_null ~stderr:(`FD_move_safely w) ["docker"; "version"] in
        let r = Lwt_io.(of_fd ~mode:input) r ~close:Lwt.return in
        Lwt_io.read r >>= fun err ->
        result >>= function
        | Ok () -> Lwt_result.return ()
        | Error (`Msg m) -> Lwt_result.fail (`Msg (Fmt.str "%s@.%s" m (String.trim err)))
      ) >>!= fun () ->
    let buffer = Buffer.create 1024 in
    let log = log_to buffer in
    (* Get the base image first, before starting the timer. *)
    let switch = Lwt_switch.create () in
    let context = Context.v ~switch ~log ~src_dir:"/tmp" () in
    get_base t ~log healthcheck_base >>= function
    | Error (`Msg _) as x -> Lwt.return x
    | Error `Cancelled -> failwith "Cancelled getting base image (shouldn't happen!)"
    | Ok (id, env) ->
      let context = { context with env } in
      (* Start the timer *)
      Lwt.async (fun () ->
          Lwt_unix.sleep timeout >>= fun () ->
          Lwt_switch.turn_off switch
        );
      run_steps t ~context ~base:id healthcheck_ops >>= function
      | Ok id -> Store.delete t.store id >|= Result.ok
      | Error (`Msg msg) as x ->
        let log = String.trim (Buffer.contents buffer) in
        if log = "" then Lwt.return x
        else Lwt.return (Fmt.error_msg "%s@.%s" msg log)
      | Error `Cancelled -> Lwt.return (Fmt.error_msg "Timeout running healthcheck")

  let v ~store ~sandbox =
    let store = Store.wrap store in
    { store; sandbox }
end
