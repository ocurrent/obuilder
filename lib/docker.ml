open Lwt.Syntax

type ids = [
  | `Docker_image of string | `Docker_container of string
  | `Docker_volume of string | `Obuilder_id of string
]

let prefix = ref "obuilder"
let set_prefix prefix' = prefix := prefix'

let image_prefix () = !prefix ^ "-image-"
let container_prefix () = !prefix ^ "-container-"
let cache_prefix () = !prefix ^ "-cache-"
let volume_prefix () = !prefix ^ "-copy-"

let obuilder_libexec () = !prefix ^ "-libexec"
let image_name ?(tmp=false) name = image_prefix () ^ (if tmp then "tmp-" else "") ^ name
let container_name name = container_prefix () ^ name
let volume_cache_name ?(tmp=false) name = cache_prefix () ^ (if tmp then "tmp-" else "") ^ name
let volume_copy_name ?(tmp=false) name = volume_prefix () ^ (if tmp then "tmp-" else "") ^ name

let docker_image ?(tmp=false) id = `Docker_image (image_name ~tmp id)
let docker_container id = `Docker_container (container_name id)
let docker_volume_cache ?(tmp=false) id = `Docker_volume (volume_cache_name ~tmp id)
let docker_volume_copy ?(tmp=false) id = `Docker_volume (volume_copy_name ~tmp id)

let ( / ) = Filename.concat
let mount_point_inside_unix = if Sys.win32 then "/cygdrive/c" else "/var/lib/obuilder"
let mount_point_inside_native = if Sys.win32 then {|C:/|} else mount_point_inside_unix

let obuilder_libexec_volume ?(readonly=true) () =
  Config.Mount.{
      ty = `Volume;
      src = obuilder_libexec ();
      dst = mount_point_inside_native / obuilder_libexec ();
      readonly;
  }

let bash_entrypoint obuilder_libexec =
  [if Sys.win32 then mount_point_inside_native / obuilder_libexec / "bash.exe" else "bash"; "-c"]

let default_entrypoint =
  if Sys.win32 then [{|C:\Windows\System32\cmd.exe|}; "/S"; "/C"]
  else ["/bin/sh"; "-c"]

let rec setup_command ~entp ~cmd =
  match entp with
  | hd :: tl -> hd, tl @ cmd
  | [] -> setup_command ~entp:default_entrypoint ~cmd

let extract_name = function `Docker_image name | `Docker_container name | `Docker_volume name -> name

let pread ?timeout ?stderr argv =
  let stderr = Option.value ~default:(`FD_move_safely Os.stderr) stderr in
  Os.pread ?timeout  ~stderr ("docker" :: argv)

let pread_result ?stdin ?stderr argv =
  let cmd = "docker" :: argv in
  let pp f = Os.pp_cmd f ("", cmd) in
  let stdin = Option.value ~default:`Dev_null stdin in
  let stderr = Option.value ~default:(`FD_move_safely Os.stderr) stderr in
  Os.pread_result ~pp ~stdin ~stderr cmd

let exec' ?stdin ?stdout ?stderr ?is_success argv =
  let stdin = Option.value ~default:`Dev_null stdin in
  let stdout = Option.value ~default:(`FD_move_safely Os.stdout) stdout in
  let stderr = Option.value ~default:(`FD_move_safely Os.stderr) stderr in
  Os.exec ~stdin ~stdout ~stderr ?is_success ("docker" :: argv)

let exec_result' ?stdin ?stdout ?stderr ?is_success argv =
  let cmd = "docker" :: argv in
  let pp f = Os.pp_cmd f ("", cmd) in
  let stdin = Option.value ~default:`Dev_null stdin in
  let stdout = Option.value ~default:(`FD_move_safely Os.stdout) stdout in
  let stderr = Option.value ~default:(`FD_move_safely Os.stderr) stderr in
  Os.exec_result ~stdin ~stdout ~stderr ?is_success ~pp cmd

module Cmd = struct
  type 'a log = ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                'a
  type 'a logerr = ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                   'a

  let version ?stderr () =
    pread_result ?stderr (["version"])

  let create ?stderr (`Docker_image base) =
    pread ?stderr ("create" :: ["--"; base])

  let export ?stdout ?stderr (`Docker_container id) =
    exec' ?stdout ?stderr ["export"; "--"; id]

  let image ?stdout ?stderr (`Remove (`Docker_image id)) =
    exec' ?stdout ?stderr ["image"; "rm"; id]

  let rm ?stdout ?stderr containers =
    exec' ?stdout ?stderr ("rm" :: "--force" :: "--" :: (List.rev_map extract_name containers))

  let tag ?stdout ?stderr (`Docker_image source) (`Docker_image target) =
    exec' ?stdout ?stderr ["tag"; source; target]

  let commit ?stdout ?stderr (`Docker_image base_image) (`Docker_container container) (`Docker_image target_image) =
    (* Restore CMD and ENTRYPOINT *)
    let* entrypoint = pread ["inspect"; "--type=image"; "--format={{json .Config.Entrypoint }}"; "--"; base_image] in
    let* cmd = pread ["inspect"; "--type=image"; "--format={{json .Config.Cmd }}"; "--"; base_image] in
    let entrypoint, cmd = String.trim entrypoint, String.trim cmd in
    let argv = [ "--"; container; target_image ] in
    let argv = if entrypoint = "null" then argv else ("--change=ENTRYPOINT " ^ entrypoint) :: argv in
    let argv = if cmd = "null" then argv else ("--change=CMD " ^ cmd) :: argv in
    exec' ?stdout ?stderr ("commit" :: argv)

  let pull ?stdout ?stderr (`Docker_image base) =
    exec' ?stdout ?stderr ["pull"; base]

  let exists ?(stdout=`Dev_null) ?stderr id =
    let argv = match id with
      | `Docker_container id -> ["inspect"; "--type=container"; "--"; id]
      | `Docker_image id -> ["inspect"; "--type=image"; "--"; id]
      | `Docker_volume id -> ["volume"; "inspect"; "--"; id]
    in
    exec_result' ~stdout ?stderr argv

  let build ?stdout ?stderr docker_argv (`Docker_image image) context_path =
    exec' ?stdout ?stderr ("build" :: docker_argv @ ["-t"; image; context_path])

  let run_argv ?stdin ?name ~rm ~docker_argv image argv =
    let docker_argv = if rm then "--rm" :: docker_argv else docker_argv in
    let docker_argv = match name with
      | Some (`Docker_container name) -> "--name" :: name :: docker_argv
      | None -> docker_argv in
    let docker_argv = match stdin with
      | Some (`FD_move_safely _) -> "-i" :: docker_argv
      | _ -> docker_argv in
    "run" :: docker_argv @ image :: argv

  let run ?stdin ?stdout ?stderr ?is_success ?name ?(rm=false) docker_argv (`Docker_image image) argv =
    let argv = run_argv ?stdin ?name ~rm ~docker_argv image argv in
    exec' ?stdin ?stdout ?stderr ?is_success argv

  let run_result ?stdin ?stdout ?stderr  ?name ?(rm=false) docker_argv (`Docker_image image) argv =
    let argv = run_argv ?stdin ?name ~rm ~docker_argv image argv in
    exec_result' ?stdin ?stdout ?stderr argv

  let run_pread_result ?stdin ?stderr ?name ?(rm=false) docker_argv (`Docker_image image) argv =
    let argv = run_argv ?name ~rm ~docker_argv image argv in
    pread_result ?stdin ?stderr argv

  let run' = run
  let run_result' = run_result

  let stop ?stdout ?stderr (`Docker_container name) =
    exec_result' ?stdout ?stderr ["stop"; name]

  let volume ?stderr ?timeout = function
    | `Create (`Docker_volume name) ->
      pread ?timeout ("volume" :: "create" :: "--" :: name :: [])
    | `Inspect (volumes, `Mountpoint) ->
      let volumes = List.rev_map extract_name volumes in
      let format = "{{ .Mountpoint }}" in
      pread ?stderr ("volume" :: "inspect" :: "--format" :: format :: "--" :: volumes)
    | `List (filter) ->
      let filter = match filter with None -> [] | Some filter -> ["--filter"; filter] in
      pread ?stderr ("volume" :: "ls" :: "--quiet" :: filter)
    | `Remove volumes ->
      let volumes = List.rev_map extract_name volumes in
      pread ("volume" :: "rm" :: "--" :: volumes)

  let volume_containers ?stderr (`Docker_volume name) =
    let+ names = pread ?stderr (["ps"; "-a"; "--filter"; "volume=" ^ name; "--format={{ .Names }}"]) in
    names |> String.trim |> String.split_on_char '\n'
    |> List.map (fun id -> `Docker_container id)

  let mount_point ?stderr name =
    let* s = volume ?stderr (`Inspect ([name], `Mountpoint)) in
    Lwt.return (String.trim s)

  let rmi ?stdout ?stderr images =
    exec' ?stdout ?stderr ("rmi" :: (List.rev_map extract_name images))

  let manifest ?stdout ?stderr = function
    | `Create (`Docker_image name, manifests) ->
      exec_result' ?stdout ?stderr ("manifest" :: "create" :: name :: (List.rev_map extract_name manifests))
    | `Inspect (`Docker_image name) ->
      exec_result' ?stdout ?stderr ["manifest"; "inspect"; name]
    | `Remove manifests ->
      exec_result' ?stdout ?stderr ("manifest" :: "rm" :: (List.rev_map extract_name manifests))

  let obuilder_images ?stderr ?tmp () =
    let* images = pread ?stderr ["images"; "--format={{ .Repository }}"; image_name ?tmp "*"] in
    String.split_on_char '\n' images
    |> List.filter_map (function "" -> None | id -> Some (`Docker_image id))
    |> Lwt.return

  let obuilder_containers ?stderr () =
    let* containers = pread ?stderr ["container"; "ls"; "--all"; "--filter"; "name=^" ^ !prefix; "-q"] in
    String.split_on_char '\n' containers
    |> List.filter_map (function "" -> None | id -> Some (`Docker_container id))
    |> Lwt.return

  let obuilder_volumes ?stderr ?(prefix=(!prefix)) () =
    let* volumes = volume ?stderr (`List (Some ("name=^" ^ prefix))) in
    String.split_on_char '\n' volumes
    |> List.filter_map (function "" -> None | id -> Some (`Docker_volume id))
    |> Lwt.return

  let obuilder_caches_tmp ?stderr () =
    obuilder_volumes ?stderr ~prefix:(cache_prefix () ^ "tmp-") ()
end


module Cmd_log = struct

  type 'a log = log:Build_log.t -> 'a
  type 'a logerr = log:Build_log.t -> 'a

  let with_stderr_log ~log fn =
    Os.with_pipe_from_child @@ fun ~r:err_r ~w:err_w ->
    let stderr = `FD_move_safely err_w in
    let copy_log = Build_log.copy ~src:err_r ~dst:log in
    let* r = fn ~stderr in
    let+ () = copy_log in
    r

  let with_log ~log fn =
    Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
    let stdout = `FD_move_safely out_w in
    let stderr = stdout in
    let copy_log = Build_log.copy ~src:out_r ~dst:log in
    let* r = fn ~stdout ~stderr in
    let+ () = copy_log in
    r

  let version ~log () =
    with_stderr_log ~log (fun ~stderr -> Cmd.version ~stderr ())

  let pull ~log base =
    with_log ~log (fun ~stdout ~stderr -> Cmd.pull ~stdout ~stderr base)

  let export ~log container =
    with_log ~log (fun ~stdout ~stderr -> Cmd.export ~stdout ~stderr container)

  let image ~log cmd =
    with_log ~log (fun ~stdout ~stderr -> Cmd.image ~stdout ~stderr cmd)

  let rm ~log containers =
    with_log ~log (fun ~stdout ~stderr -> Cmd.rm ~stdout ~stderr containers)

  let rmi ~log images =
    with_log ~log (fun ~stdout ~stderr -> Cmd.rmi ~stdout ~stderr images)

  let tag ~log source target =
    with_log ~log (fun ~stdout ~stderr -> Cmd.tag ~stdout ~stderr source target)

  let commit ~log base_image container target_image =
    with_log ~log (fun ~stdout ~stderr ->
        Cmd.commit ~stdout ~stderr base_image container target_image)

  let volume ~log ?timeout cmd =
    with_stderr_log ~log (fun ~stderr -> Cmd.volume ~stderr ?timeout cmd)

  let volume_containers ~log volume =
    with_stderr_log ~log (fun ~stderr -> Cmd.volume_containers ~stderr volume)

  let mount_point ~log volume =
    with_stderr_log ~log (fun ~stderr -> Cmd.mount_point ~stderr volume)

  let build ~log docker_argv image context_path =
    with_log ~log (fun ~stdout ~stderr ->
        Cmd.build ~stdout ~stderr docker_argv image context_path)

  let stop ~log name =
    with_log ~log (fun ~stdout ~stderr -> Cmd.stop ~stdout ~stderr name)

  let manifest ~log cmd =
    with_log ~log (fun ~stdout ~stderr -> Cmd.manifest ~stdout ~stderr cmd)

  let exists ~log cmd =
    with_log ~log (fun ~stdout ~stderr -> Cmd.exists ~stdout ~stderr cmd)

  let run ?stdin ~log ?is_success ?name ?rm docker_argv image argv =
    with_log ~log (fun ~stdout ~stderr ->
        Cmd.run ?stdin ~stdout ~stderr ?is_success ?name ?rm docker_argv image argv)

  let run' ?stdin ?stdout ~log ?is_success ?name ?rm docker_argv image argv =
    with_stderr_log ~log (fun ~stderr ->
        Cmd.run' ?stdin ?stdout ~stderr ?is_success ?name ?rm docker_argv image argv)

  let run_result ?stdin ~log ?name ?rm docker_argv image argv =
    with_log ~log (fun ~stdout ~stderr ->
        Cmd.run_result ?stdin ~stdout ~stderr ?name ?rm docker_argv image argv)

  let run_result' ?stdin ?stdout ~log ?name ?rm docker_argv image argv =
    with_stderr_log ~log (fun ~stderr ->
        Cmd.run_result' ?stdin ?stdout ~stderr ?name ?rm docker_argv image argv)

  let run_pread_result ?stdin ~log ?name ?rm docker_argv image argv =
    with_stderr_log ~log (fun ~stderr ->
        Cmd.run_pread_result ?stdin ~stderr ?name ?rm docker_argv image argv)

  let obuilder_images ~log ?tmp () =
    with_stderr_log ~log (fun ~stderr -> Cmd.obuilder_images ~stderr ?tmp ())

  let obuilder_containers ~log () =
    with_stderr_log ~log (fun ~stderr -> Cmd.obuilder_containers ~stderr ())

  let obuilder_volumes ~log ?prefix () =
    with_stderr_log ~log (fun ~stderr -> Cmd.obuilder_volumes ~stderr ?prefix ())

  let obuilder_caches_tmp ~log () =
    with_stderr_log ~log (fun ~stderr -> Cmd.obuilder_caches_tmp ~stderr ())
end

let root = Fpath.v (if Sys.win32 then {|C:\|} else "/")

let mount_args (mount:Config.Mount.t) =
  (* Unspecified, but consistent with copy stanza *)
  let dst = if not Sys.unix && mount.Config.Mount.dst.[0] = '/' then "C:" ^ mount.dst else mount.dst in
  [ "--mount"; Printf.sprintf "type=%s,src=%s,dst=%s%s"
                 (match mount.ty with `Bind -> "bind" | `Volume -> "volume")
                 mount.src dst (if mount.readonly then ",readonly" else "") ]

let relativize dst =
  match Fpath.(relativize ~root (v dst)) with Some dst -> Fpath.to_string dst | None -> dst

let cp_to_volume ~base ~volume ~src ~dst =
  let `Docker_image base = base in
  assert (not (volume.Config.Mount.readonly));
  let open Lwt_result.Syntax in
  let* id = pread_result ("container" :: "create" :: (mount_args volume) @ [base]) in
  let id = String.trim id in
  Lwt.finalize (fun () ->
      exec_result' ["cp"; src; Printf.sprintf "%s:%s" id (relativize dst)])
    (fun () -> Cmd.rm [`Docker_container id])

let cp_between_volumes ~base ~src ~dst =
  let (`Docker_volume src) = src and (`Docker_volume dst) = dst in
  let root = Fpath.to_string root in
  let mounts_proc = Config.Mount.{ty = `Volume; src = dst; dst = root / "dst"; readonly = false }
  and mounts_send = Config.Mount.{ty = `Volume; src = src; dst = root / "src"; readonly = true } in
  let mounts_args mount = mount :: (if Sys.win32 then [obuilder_libexec_volume ()] else [])
                          |> List.concat_map mount_args in
  let mounts_send = mounts_args mounts_send and mounts_proc = mounts_args mounts_proc in
  let tar = if Sys.win32 then mount_point_inside_native / obuilder_libexec () / "tar.exe"
            else "tar" in
  let root = if Sys.win32 then {|/cygdrive/c/|} else "/" in
  Os.with_pipe_between_children @@ fun ~r ~w ->
  let proc = Cmd.run_result' ~stdin:(`FD_move_safely r) ~rm:true mounts_proc base [tar; "x"; "-C"; root ^ "dst"; "-f"; "-"]
  and send = Cmd.run_result' ~stdout:(`FD_move_safely w) ~rm:true mounts_send base [tar; "c"; "-C"; root ^ "src"; "-f"; "-"; "."] in
  let open Lwt_result.Syntax in
  let* () = proc in
  let+ () = send in
  ()

let with_container ~log base fn =
  let* cid = Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = Build_log.copy ~src:r ~dst:log in
      let* cid = Cmd.create ~stderr:(`FD_move_safely w) (`Docker_image base) in
      let+ () = copy in
      String.trim cid
    )
  in
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> Cmd.rm ~stdout:`Dev_null [`Docker_container cid])

module Extract = struct
  let export_env base : Config.env Lwt.t =
    let+ env =
      pread ["image"; "inspect";
              "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
              "--"; base] in
    String.split_on_char '\x00' env
    |> List.filter_map (function
        | "\n" -> None
        | kv ->
          match Astring.String.cut ~sep:"=" kv with
          | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
          | Some _ as pair -> pair
      )

  let fetch ~log ~rootfs base =
    let* () = with_container ~log base (fun cid ->
        Os.with_pipe_between_children @@ fun ~r ~w ->
        let exporter = Cmd.export ~stdout:(`FD_move_safely w) (`Docker_container cid) in
        let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
        let* () = exporter in
        tar
      )
    in
    export_env base
end
