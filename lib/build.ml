open Lwt.Infix
open Sexplib.Std

let ( / ) = Filename.concat
let ( // ) p1 p2 = if Sys.win32 then p1 ^ "/" ^ p2 else Filename.concat p1 p2
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

let healthcheck_base () =
  if Sys.win32 then
    Docker_sandbox.servercore () >>= fun (`Docker_image servercore) ->
    Lwt.return servercore
  else Lwt.return "busybox"

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

  let v ?switch ?(env=[]) ?(user=Obuilder_spec.root) ?workdir ?(secrets=[]) ~shell ~log ~src_dir () =
    let workdir = Option.value ~default:(if Sys.win32 then {|C:/|} else "/") workdir in
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
                 { Config.Mount.ty = `Bind; src; dst = target; readonly = false }
               )
             >>= fun mounts ->
             let argv = shell @ [cmd] in
             let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env ~mounts ~mount_secrets ~network () in
             Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
             Lwt_unix.close close_me >>= fun () ->
             Sandbox.run ~cancelled ~stdin ~log t.sandbox config result_tmp
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

  let copy t ~context ~base { Obuilder_spec.from; src; dst; exclude } =
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _; scope; secrets = _ } = context in
    let dst = if Filename.is_relative dst then workdir / dst else dst in
    begin
      match from with
      | `Context -> Lwt_result.return src_dir
      | `Build name ->
        match Scope.find_opt name scope with
        | None -> Fmt.failwith "Unknown build %S" name   (* (shouldn't happen; gets caught earlier) *)
        | Some id ->
          Store.result t.store id >>= function
          | None ->
            Lwt_result.fail (`Msg (Fmt.str "Build result %S not found" id))
          | Some dir ->
            Lwt_result.return (dir / "rootfs")
    end >>!= fun src_dir ->
    let src_manifest = sequence (List.map (Manifest.generate ~exclude ~src_dir) src) in
    match Result.bind src_manifest (to_copy_op ~dst) with
    | Error _ as e -> Lwt.return e
    | Ok op ->
      let details = {
        base;
        op;
        user;
      } in
      (* Fmt.pr "COPY: %a@." Sexplib.Sexp.pp_hum (sexp_of_copy_details details); *)
      let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
      Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
          let argv = Sandbox.tar t.sandbox in
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
            (* If the sending thread finishes (or fails), close the writing socket
               immediately so that the tar process finishes too. *)
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

  let pp_op ~(context:Context.t) f op =
    Fmt.pf f "@[<v2>%s: %a@]" context.workdir Obuilder_spec.pp_op op

  let update_workdir ~(context:Context.t) path =
    let workdir =
      if Astring.String.is_prefix ~affix:"/" path then path
      else context.workdir ^ "/" ^ path
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
    | [] -> Sandbox.finished () >>= fun () -> Lwt_result.return base
    | op :: ops ->
      context.log `Heading Fmt.(str "%a" (pp_op ~context) op);
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

  let get_base t ~log base =
    log `Heading (Fmt.str "(from %a)" Sexplib.Sexp.pp_hum (Atom base));
    let id = Sha256.to_hex (Sha256.string base) in
    let root = Store.root t.store in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log tmp ->
        Log.info (fun f -> f "Base image not present; importing %S…" base);
        let rootfs = tmp / "rootfs" in
        Os.sudo ["mkdir"; "-m"; "755"; "--"; rootfs] >>= fun () ->
        Fetch.fetch ~log ~root ~rootfs base >>= fun env ->
        Os.write_file ~path:(tmp / "env")
          (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id -> Store.result t.store id
    >|= Option.get >>= fun path ->
    let { Saved_context.env } = Saved_context.t_of_sexp (Sexplib.Sexp.load_sexp (path / "env")) in
    Lwt_result.return (id, env)

  let rec build t context { Obuilder_spec.child_builds; from = base; ops } =
    let rec aux context = function
      | [] -> Lwt_result.return context
      | (name, child_spec) :: child_builds ->
        context.Context.log `Heading Fmt.(str "(build %S …)" name);
        build t context child_spec >>!= fun child_result ->
        context.Context.log `Note Fmt.(str "--> finished %S" name);
        let context = Context.with_binding name child_result context in
        aux context child_builds
    in
    aux context child_builds >>!= fun context ->
    get_base t ~log:context.Context.log base >>!= fun (id, env) ->
    let context = { context with env = context.env @ env } in
    run_steps t ~context ~base:id ops

  let build t context spec =
    let r = build t context spec in
    (r : (string, [ `Cancelled | `Msg of string ]) Lwt_result.t :> (string, [> `Cancelled | `Msg of string ]) Lwt_result.t)

  let delete ?log t id =
    Store.delete ?log t.store id

  let prune ?log t ~before limit =
    Store.prune ?log t.store ~before limit

  let count t =
    Store.count t.store

  let df t =
    Store.df t.store

  let shell t =
    Sandbox.shell t.sandbox

  let root t =
    Store.root t.store

  let cache_stats t =
    Store.cache_stats t.store

  let log_to buffer tag x =
    match tag with
    | `Heading | `Note -> Buffer.add_string buffer (x ^ "\n")
    | `Output -> Buffer.add_string buffer x

  let healthcheck ?(timeout=300.0) t =
    Os.with_pipe_from_child (fun ~r ~w ->
        let result = Docker.Cmd.version ~stderr:(`FD_move_safely w) () in
        let r = Lwt_io.(of_fd ~mode:input) r ~close:Lwt.return in
        Lwt_io.read r >>= fun err ->
        result >>= function
        | Ok _desc -> Lwt_result.return ()
        | Error (`Msg m) -> Lwt_result.fail (`Msg (Fmt.str "%s@.%s" m (String.trim err)))
      ) >>!= fun () ->
    let buffer = Buffer.create 1024 in
    let log = log_to buffer in
    (* Get the base image first, before starting the timer. *)
    let switch = Lwt_switch.create () in
    let context = Context.v ~shell:(Sandbox.shell t.sandbox) ~switch ~log ~src_dir:"/tmp" () in
    healthcheck_base () >>= function healthcheck_base ->
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

  let finish t =
    Store.unwrap t.store;
    Lwt.return_unit
end

module Make_Docker (Raw_store : S.STORE) = struct
  module Store = Db_store.Make(Raw_store)

  type t = {
    store : Store.t;
    sandbox : Docker_sandbox.t;
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

  let run t ~switch ~log ~cache run_input =
    let id =
      sexp_of_run_input run_input
      |> Sexplib.Sexp.to_string_mach
      |> Sha256.string
      |> Sha256.to_hex
    in
    let { base; workdir; user; env; cmd; shell; network; mount_secrets } = run_input in
    Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log _ ->
        let to_release = ref [] in
        Lwt.finalize
          (fun () ->
             cache |> Lwt_list.map_s (fun { Obuilder_spec.Cache.id; target; buildkit_options = _ } ->
                 Store.cache ~user t.store id >|= fun (src, release) ->
                 to_release := release :: !to_release;
                 { Config.Mount.ty = `Volume; src; dst = target; readonly = false }
               )
             >>= fun mounts ->
             let entrypoint, argv = Docker.setup_command ~entp:shell ~cmd:[cmd] in
             let config = Config.v ~cwd:workdir ~entrypoint ~argv ~hostname ~user ~env ~mounts ~mount_secrets ~network () in
             Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
             Lwt_unix.close close_me >>= fun () ->
             Lwt_result.bind_lwt
               (Docker_sandbox.run ~cancelled ~stdin ~log t.sandbox config id)
               (fun () -> Docker_sandbox.teardown ~log ~commit:true id)
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

  let copy t ~context ~base { Obuilder_spec.from; src; dst; exclude } =
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _; scope; secrets = _ } = context in
    let dst = if Filename.is_relative dst then workdir // dst else dst in
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
            Lwt_result.return (`Build (id, dir))
    end >>!= fun src_dir ->
    begin match src_dir with
      | `Context src_dir -> sequence (List.map (Manifest.generate ~exclude ~src_dir) src) |> Lwt.return
      | `Build (id, _) -> Docker_sandbox.manifest_from_build t.sandbox ~base:id ~exclude src workdir user
    end >>= fun src_manifest ->
    match Result.bind src_manifest (to_copy_op ~dst) with
    | Error _ as e -> Lwt.return e
    | Ok op ->
      let details = {
        base;
        op;
        user;
      } in
      let dst_dir = match op with `Copy_items (_, dst_dir) when Sys.win32 -> Some dst_dir | _ -> None in
      (* Fmt.pr "COPY: %a@." Sexplib.Sexp.pp_hum (sexp_of_copy_details details); *)
      let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
      Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log _ ->
          match src_dir with
          | `Context src_dir ->
            Docker_sandbox.copy_from_context t.sandbox ~cancelled ~log op ~user ~src_dir ?dst_dir id
          | `Build (from_id, _) ->
            Docker_sandbox.copy_from_build t.sandbox ~cancelled ~log op ~user ~workdir ?dst_dir ~from_id id
        )

  let pp_op ~(context:Context.t) f op =
    Fmt.pf f "@[<v2>%s: %a@]" context.workdir Obuilder_spec.pp_op op

  let update_workdir ~(context:Context.t) path =
    let workdir =
      if Astring.String.is_prefix ~affix:"/" path then (if Sys.win32 then "C:" ^ path else path)
      else context.workdir ^ "/" ^ path
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
      context.log `Heading Fmt.(str "%a" (pp_op ~context) op);
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
        (* Unspecified, but consistent with copy stanza *)
        let shell = match shell with
          | hd :: tl when not Sys.unix && hd.[0] = '/' -> ("C:" ^ hd) :: tl
          | _ -> shell
        in
        k ~base ~context:{context with shell}

  let get_base t ~log base =
    log `Heading (Fmt.str "(from %a)" Sexplib.Sexp.pp_hum (Atom base));
    let id = Sha256.to_hex (Sha256.string base) in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log:_ _ ->
        Log.info (fun f -> f "Base image not present; importing %S…" base);
        Docker.Cmd.pull (`Docker_image base) >>= fun () ->
        Docker.Cmd.tag (`Docker_image base) (Docker.docker_image id) >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id ->
    Lwt_result.return (id, [])

  let rec build ~scope t context { Obuilder_spec.child_builds; from = base; ops } =
    let rec aux context = function
      | [] -> Lwt_result.return context
      | (name, child_spec) :: child_builds ->
        context.Context.log `Heading Fmt.(str "(build %S …)" name);
        build ~scope t context child_spec >>!= fun child_result ->
        context.Context.log `Note Fmt.(str "--> finished %S" name);
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

  let count t =
    Store.count t.store

  let df t =
    Store.df t.store

  let shell t =
    Docker_sandbox.shell t.sandbox

  let root t =
    Store.root t.store

  let cache_stats t =
    Store.cache_stats t.store

  let log_to buffer tag x =
    match tag with
    | `Heading | `Note -> Buffer.add_string buffer (x ^ "\n")
    | `Output -> Buffer.add_string buffer x

  let healthcheck ?(timeout=if Sys.win32 then 300.0 else 300.0) t =
    Os.with_pipe_from_child (fun ~r ~w ->
        let result = Docker.Cmd.version ~stderr:(`FD_move_safely w) () in
        let r = Lwt_io.(of_fd ~mode:input) r ~close:Lwt.return in
        Lwt_io.read r >>= fun err ->
        result >>= function
        | Ok _desc -> Lwt_result.return ()
        | Error (`Msg m) -> Lwt_result.fail (`Msg (Fmt.str "%s@.%s" m (String.trim err)))
      ) >>!= fun () ->
    let buffer = Buffer.create 1024 in
    let log = log_to buffer in
    (* Get the base image first, before starting the timer. *)
    let switch = Lwt_switch.create () in
    let src_dir = if Sys.win32 then {|C:\TEMP|} else "/tmp" in
    let context = Context.v ~shell:(Docker_sandbox.shell t.sandbox) ~switch ~log ~src_dir () in
    healthcheck_base () >>= function healthcheck_base ->
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

  let finish t =
    Store.unwrap t.store;
    Lwt.return_unit
end
