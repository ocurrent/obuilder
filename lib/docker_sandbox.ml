open Lwt.Syntax
open Sexplib.Conv

let backend = `Docker

let ( / ) = Filename.concat

type isolation = [ `HyperV | `Process | `Default ] [@@deriving sexp]
let isolations : (isolation * string) list = [(`HyperV, "hyperv"); (`Process, "process"); (`Default, "default")]

type t = {
  state_dir : string;
  docker_cpus : int;
  docker_isolation : isolation;
}

type config = {
  docker_cpus : int;
  docker_isolation : isolation;
} [@@deriving sexp]

let secrets_guest_root = if Sys.win32 then {|C:\ProgramData\obuilder\|} else "/run/secrets/obuilder"
let secret_dir id = "secrets" / string_of_int id

module Docker_config = struct
  (** [make config] returns the docker CLI arguments and the command
     to execute. *)
  let make {Config.cwd; argv; hostname; user; env; mounts; network; mount_secrets; entrypoint}
        ~config_dir ({docker_cpus; docker_isolation; _} : t) =
    let mounts = mounts |> List.concat_map (fun mount ->
      [ "--mount"; Config.Mount.(Printf.sprintf "type=volume,src=%s,dst=%s%s"
          mount.src mount.dst (if mount.readonly then ",readonly" else "")) ]) in
    let env = env |> List.concat_map (fun (k, v) -> [ "--env"; Printf.sprintf "%s=%s" k v ]) in
    let network = network |> List.concat_map (fun network -> ["--network"; network]) in
    let user =
      match user with
      | `Unix { Obuilder_spec.uid; gid } when not Sys.win32 -> ["--user"; Printf.sprintf "%d:%d" uid gid]
      | `Windows { name } when Sys.win32 -> ["--user"; name]
      | _ -> assert false
    in
    let (_, mount_secrets) =
      List.fold_left (fun (id, mount_secrets) _ ->
          let host, guest = config_dir / secret_dir id, secrets_guest_root / secret_dir id in
          let argv = "--mount" :: (Printf.sprintf "type=bind,src=%s,dst=%s,readonly" host guest) :: mount_secrets in
          id + 1, argv)
        (0, []) mount_secrets in
    let entrypoint = Option.fold ~none:[] ~some:(fun exe -> ["--entrypoint"; exe]) entrypoint in
    let docker_argv = [
        "--cpus"; string_of_int docker_cpus;
        "--isolation"; (List.assoc docker_isolation isolations);
        "--hostname"; hostname;
        "--workdir"; cwd;
      ] @ user @ env @ mounts @ mount_secrets @ network @ entrypoint in
    docker_argv, argv
end

let secrets_layer mount_secrets base_image container docker_argv =
  (* FIXME: the shell, mkdir mklink/ln should come from a trusted
     volume rather than the container itself. *)
  let link id link =
    let target = secrets_guest_root / secret_dir id / "secret" in
    if Sys.win32 then
      ["mkdir"; Filename.dirname link; "&&";
       "mklink"; link; target]
    else
      ["mkdir"; "-p"; Filename.(dirname link |> quote); "&&";
       "ln"; "-s"; "--"; Filename.quote target; Filename.quote link]
  in
  let (_, argv) =
    List.fold_left (fun (id, argv) {Config.Secret.target; _} ->
        let argv = if argv = [] then link id target else argv @ "&&" :: link id target in
        id + 1, argv)
      (0, []) mount_secrets
  in
  if mount_secrets = [] then
    Lwt_result.ok Lwt.return_unit
  else
    let docker_argv, argv =
      if Sys.win32 then
        docker_argv @ ["--entrypoint"; {|C:\Windows\System32\cmd.exe|}],
        ["/S"; "/C"; String.concat " " argv]
      else
        docker_argv @ ["--entrypoint"; {|/bin/sh|}],
        ["-c"; String.concat " " argv]
    in
    let pp f = Os.pp_cmd f ("docker" :: "run" :: docker_argv @ argv) in
    Lwt_result.bind_lwt
      (Docker.run_result ~pp ~name:container docker_argv base_image argv)
      (fun () ->
        let* () = Docker.commit base_image container base_image in
        Docker.rm [container])

let run ~cancelled ?stdin ~log t config results_dir =
  ignore log; (* FIXME: I get broken pipe errors *)
  Lwt_io.with_temp_dir ~perm:0o700 ~prefix:"obuilder-docker-" @@ fun tmp ->
  let docker_argv, argv = Docker_config.make config ~config_dir:tmp t in
  let* _ = Lwt_list.fold_left_s
    (fun id Config.Secret.{value; _} ->
      Os.ensure_dir (tmp / "secrets");
      Os.ensure_dir (tmp / secret_dir id);
      let+ () = Os.write_file ~path:(tmp / secret_dir id / "secret") value in
      id + 1
    ) 0 config.mount_secrets
  in
  let id = Filename.basename results_dir in
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let proc =
    Lwt_result.bind
      (secrets_layer config.Config.mount_secrets base_image container docker_argv)
      (fun () ->
        let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
        let pp f = Os.pp_cmd f ("docker" :: "run" :: docker_argv @ argv) in
        Docker.run_result ?stdin ~pp ~name:container docker_argv base_image argv)
  in
  Lwt.on_termination cancelled (fun () ->
      let rec aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "docker stop %S" id in
          let* r = Docker.stop ~pp container in
          match r with
          | Ok () -> Lwt.return_unit
          | Error (`Msg m) ->
            (* This might be because it hasn't been created yet, so retry. *)
            Log.warn (fun f -> f "stop failed: %s (will retry in 10s)" m);
            let* () = Lwt_unix.sleep 10.0 in
            aux ()
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  let* r = proc in
  let* () = match r with
    | Ok () -> Lwt.return_unit
    | _ -> Docker.rm [container]
  in
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let clean_docker dir =
  Log.warn (fun f -> f "Removing left-over Docker containers");
  let* containers = Docker.obuilder_containers () in
  let* () = if containers <> [] then Docker.rm containers else Lwt.return_unit in
  Log.warn (fun f -> f "Removing left-over Docker images");
  let* images = Docker.obuilder_images () in
  let* () =  if images <> [] then Docker.rmi images else Lwt.return_unit in
  Sys.readdir dir
  |> Array.to_list
  |> Lwt_list.iter_s (fun item ->
         Log.warn (fun f -> f "Removing left-over docker sandbox data %S" item);
         Os.delete_recursively item
       )

(* Windows ships a bsdtar that doesn't support symlinks (neither when
   creating the tar archive, nor when extracting it). We need a
   working tar for copying files in and out Docker images, so we pull
   Cygwin, install it, and extract tar and its dependencies in a
   Docker volume that is mounted each time we need tar.

   On Linux, we assume a tar is always present in /usr/bin/tar.

   We use `manifest.sh', an implementation of {!Manifest} in Bash, to
   extract the tar manifest from the Docker image. *)
let create_tar_volume isolation =
  Log.info (fun f -> f "Preparing tar volume...");
  let* _ = Docker.volume ["create"] (`Docker_volume (Docker.obuilder_volume ())) in
  let* () = if Sys.win32 then begin
    let cygwin_root = {|C:\cygwin64|} in
    let* () = Lwt_io.(with_temp_dir ~perm:0o700 @@ fun temp_dir ->
      let* () = Lwt_list.iter_p (fun name ->
        with_file ~perm:0o400 ~mode:Output (temp_dir / name) @@ fun ch ->
        fprint ch (Option.get (Static_files.read name))) ["Dockerfile"; "extract.cmd"] in
      Array.iter (fun s -> Log.debug (fun f -> f "%s"s )) (Sys.readdir temp_dir);
      let docker_argv = [
        "--isolation"; List.assoc isolation isolations;
        Printf.sprintf "--build-arg=CYGWIN_ROOT=%s" cygwin_root;
      ] in
      Docker.build docker_argv (`Docker_image (Docker.obuilder_volume ())) temp_dir) in
    let destination = Printf.sprintf {|C:\%s|} (Docker.obuilder_volume ()) in
    let docker_argv = [
      "--isolation"; List.assoc isolation isolations;
      "--mount"; Printf.sprintf "type=volume,src=%s,dst=%s" (Docker.obuilder_volume ()) destination;
      "--env"; Printf.sprintf "CYGWIN_ROOT=%s" cygwin_root;
      "--env"; Printf.sprintf "DESTINATION=%s" destination;
      "--entrypoint"; {|C:\Windows\System32\cmd.exe|};
    ] in
    Docker.run ~rm:true docker_argv (`Docker_image (Docker.obuilder_volume ())) ["/S"; "/C"; {|C:\extract.cmd|}]
  end else Lwt.return_unit in
  let* mount_point = Docker.mount_point (`Docker_volume (Docker.obuilder_volume ())) in
  let name = "manifest.sh" in
  let write_manifest_sh ch = Lwt_io.fprint ch (Option.get (Static_files.read name)) in
  if Sys.win32 then
    Lwt_io.(with_file ~perm:0o500 ~mode:Output (mount_point / name) write_manifest_sh)
  else
    Lwt_io.(with_temp_file ~perm:0o500 @@ fun (temp_name, ch) ->
      let* () = write_manifest_sh ch in
      Os.sudo ["cp"; "--"; temp_name; mount_point / name])

let create ~state_dir (c : config) =
  Log.debug (fun f -> f "Docker sandbox: create %s" state_dir);
  Os.ensure_dir state_dir;
  let* () = clean_docker state_dir in
  let* () = create_tar_volume c.docker_isolation in
  Lwt.return { state_dir; docker_cpus = c.docker_cpus; docker_isolation = c.docker_isolation }

open Cmdliner

let docker_cpus =
  Arg.value @@
  Arg.opt Arg.int 2 @@
  Arg.info
    ~doc:"Number of CPUs to be used by Docker"
    ["docker-cpus"]

let docker_isolation =
  let isolations = List.rev_map (fun (k, v) -> v, k) isolations in
  let doc = Arg.doc_alts_enum isolations |> Printf.sprintf
    "Docker isolation, must be %s. Only `default' is available on \
     Linux, only `process' and `hyperv' are available on Windows" in
  Arg.value @@
  Arg.opt (Arg.enum isolations) (if Sys.win32 then `HyperV else `Default) @@
  Arg.info ~doc
    ["docker-isolation"]

let cmdliner : config Term.t =
  let make docker_cpus docker_isolation =
    { docker_cpus; docker_isolation; }
  in
  Term.(const make $ docker_cpus $ docker_isolation)
