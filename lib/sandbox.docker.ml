open Lwt.Syntax
open Sexplib.Conv

let backend = `Docker

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

module Docker_config = struct
  (** [make config] returns the docker CLI arguments and the command
     to execute. *)
  let make
        ({docker_cpus; docker_isolation; _} : t)
        {Config.cwd; argv; hostname; user; env; mounts; network; mount_secrets; entrypoint} =
    ignore cwd;
    ignore user;
    ignore network;
    ignore mount_secrets;
    let mounts = mounts |> List.concat_map (fun mount ->
      [ "--mount"; Config.Mount.(Printf.sprintf "type=volume,src=%s,dst=%s%s"
          mount.src mount.dst (if mount.readonly then ",readonly" else "")) ]) in
    let env = env |> List.concat_map (fun (k, v) -> [ "--env"; Printf.sprintf "%s=%s" k v ]) in
    let entrypoint = Option.fold ~none:[] ~some:(fun exe -> ["--entrypoint"; exe]) entrypoint in
    let docker_argv = [
        "--cpus"; string_of_int docker_cpus;
        "--isolation"; (List.assoc docker_isolation isolations);
        "--hostname"; hostname;
      ] @ env @ mounts @ entrypoint in
    docker_argv, argv
end

let run ~cancelled ?stdin ~log t config results_dir =
  (* I get broken pipes and clear_nonblock errors *)
  ignore stdin;
  ignore log;
  let id = Filename.basename results_dir in
  let docker_argv, argv = Docker_config.make t config in
  let pp f = Os.pp_cmd f ("docker" :: "run" :: docker_argv @ argv) in
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let proc = Docker.run_result ~pp ~name:container docker_argv base_image argv in
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

let create ~state_dir (c : config) =
  ignore c;
  Log.debug (fun f -> f "Docker sandbox: create %s" state_dir);
  Os.ensure_dir state_dir;
  let* () = clean_docker state_dir in
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
