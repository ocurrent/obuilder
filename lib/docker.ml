open Lwt.Syntax

type ids = [
  | `Docker_image of string | `Docker_container of string
  | `Docker_volume of string | `Obuilder_id of string
]

let prefix = "obuilder-"
let image_prefix = prefix ^ "image-"
let image_tmp_prefix = prefix ^ "tmp-image-"
let container_prefix = prefix ^ "container-"

let obuilder_volume = prefix ^ "volume"
let image_name ?(tmp=false) name = (if tmp then image_tmp_prefix else image_prefix) ^ name
let container_name name = container_prefix ^ name

let result root id = Filename.concat root id

let docker_image ?(tmp=false) id = `Docker_image (image_name ~tmp id)
let docker_container id = `Docker_container (container_name id)

let extract_name = function `Docker_image name | `Docker_container name -> name

let pread' ?stderr argv =
  Os.pread ?stderr ("docker" :: argv)

let pread_result' ~pp ?stderr argv =
  Os.pread_result ~pp ?stderr ("docker" :: argv)

let exec' ?stdin ?stdout ?stderr argv =
  Os.exec ?stdin ?stdout ?stderr ("docker" :: argv)

let exec_result' ?stdin ?stdout ?stderr ~pp argv =
  Os.exec_result ?stdin ?stdout ?stderr ~pp ("docker" :: argv)

let create ?stderr (`Docker_image base) =
  pread' ?stderr ("create" :: ["--"; base])

let export ?stdout (`Docker_container id) =
  exec' ?stdout ["export"; "--"; id]

let image ?stdout cmd (`Docker_image id) =
  exec' ?stdout ["image"; cmd; id]

let rm ?stdout containers =
  exec' ?stdout ("rm" :: "--force" :: "--" :: (List.map extract_name containers))

let tag ?stdout ?stderr (`Docker_image source) (`Docker_image target) =
  exec' ?stdout ?stderr ["tag"; source; target]

let commit ?stdout (`Docker_image base_image) (`Docker_container container) (`Docker_image target_image) =
  (* Restore CMD and ENTRYPOINT *)
  let* entrypoint = pread' ["inspect"; "--type=image"; "--format={{json .Config.Entrypoint }}"; "--"; base_image] in
  let* cmd = pread' ["inspect"; "--type=image"; "--format={{json .Config.Cmd }}"; "--"; base_image] in
  let entrypoint, cmd = String.trim entrypoint, String.trim cmd in
  let argv = [ "--"; container; target_image ] in
  let argv = if entrypoint = "null" then argv else ("--change=ENTRYPOINT " ^ entrypoint) :: argv in
  let argv = if cmd = "null" then argv else ("--change=CMD " ^ cmd) :: argv in
  exec' ?stdout ("commit" :: argv)

let pull ?stderr (`Docker_image base) =
  exec' ?stderr ["pull"; base]

let exists id =
  let pp f = Fmt.string f "docker inspect" in
  let pp' f = Fmt.string f "docker volume inspect" in
  let pp, argv = match id with
    | `Docker_container id -> pp, ["inspect"; "--type=container"; "--"; id]
    | `Docker_image id -> pp, ["inspect"; "--type=image"; "--"; id]
    | `Docker_volume id -> pp', ["volume"; "inspect"; "--"; id]
  in
  exec_result' ~stdout:`Dev_null ~stderr:`Dev_null ~pp argv

let build docker_argv (`Docker_image image) context_path =
  exec' ("build" :: docker_argv @ ["-t"; image; context_path])

let run ?stdin ?stdout ?stderr ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let docker_argv = if rm then "--rm" :: docker_argv else docker_argv in
  let docker_argv = Option.fold ~none:docker_argv ~some:(fun (`Docker_container name) -> "--name" :: name :: docker_argv) name in
  let argv = docker_argv @ image :: argv in
  exec' ?stdin ?stdout ?stderr ("run" :: argv)

let run_result ?stdin ~pp ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let docker_argv = if rm then "--rm" :: docker_argv else docker_argv in
  let docker_argv = Option.fold ~none:docker_argv ~some:(fun (`Docker_container name) -> "--name" :: name :: docker_argv) name in
  let docker_argv = if Option.is_some stdin then "-i" :: docker_argv else docker_argv in
  let argv = docker_argv @ image :: argv in
  exec_result' ?stdin ~pp ("run" :: argv)

let run_pread_result ?stderr ~pp ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let docker_argv = if rm then "--rm" :: docker_argv else docker_argv in
  let docker_argv = Option.fold ~none:docker_argv ~some:(fun (`Docker_container name) -> "--name" :: name :: docker_argv) name in
  let argv = docker_argv @ image :: argv in
  pread_result' ?stderr ~pp ("run" :: argv)

let stop ~pp (`Docker_container name) =
  exec_result' ~pp ["stop"; name]

let volume cmd (`Docker_volume name) =
  pread' ("volume" :: cmd @ ["--"; name])

let mount_point name =
  let* s = volume ["inspect"; "--format"; "{{ .Mountpoint }}"] name in
  Lwt.return (String.trim s)

let rmi ?stdout images =
  (* let images = List.map (fun (`Docker_image image) -> image) in *)
  exec' ?stdout ("rmi" :: (List.map extract_name images))

let obuilder_images () =
  let* images = pread' ["images"; "--format={{ .Repository }}"; prefix ^ "*"] in
  String.split_on_char '\n' images
  |> List.filter_map (function "" -> None | id -> Some (`Docker_image id))
  |> Lwt.return

let obuilder_containers () =
  let* containers = pread' ["container"; "ls"; "--all"; "--filter"; "name=^" ^ prefix; "-q"] in
  String.split_on_char '\n' containers
  |> List.filter_map (function "" -> None | id -> Some (`Docker_container id))
  |> Lwt.return

let with_container ~log base fn =
  let* cid = Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = Build_log.copy ~src:r ~dst:log in
      let* cid = create ~stderr:(`FD_move_safely w) (`Docker_image base) in
      let+ () = copy in
      String.trim cid
    )
  in
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> rm ~stdout:`Dev_null [`Docker_container cid])

module Extract = struct
  let export_env base : Config.env Lwt.t =
    let+ env =
      pread' ["image"; "inspect";
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
    Log.debug (fun f -> f "Docker fetcher extract rootfs:%s base:%s" rootfs base);
    let* () = with_container ~log base (fun cid ->
        Os.with_pipe_between_children @@ fun ~r ~w ->
        let exporter = export ~stdout:(`FD_move_safely w) (`Docker_container cid) in
        let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
        let* () = exporter in
        tar
      )
    in
    export_env base
end

module Pull = struct
  let fetch ~log ~rootfs base =
    ignore rootfs;
    ignore log;
    Log.debug (fun f -> f "Docker fetcher pull rootfs:%s base:%s" rootfs base);
    let* () = pull (`Docker_image base) in
    Lwt.return_nil
end
