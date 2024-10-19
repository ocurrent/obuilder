open Lwt.Syntax
let ( >>!= ) = Lwt_result.bind
open Sexplib.Conv

let ( / ) = Filename.concat
let ( // ) dirname filename =
  if Sys.win32 then
    let l = String.length dirname in
    if l = 0 || dirname.[l-1] = '/'
    then dirname ^ filename
    else dirname ^ "/" ^ filename
  else Filename.concat dirname filename

let strf = Printf.sprintf

type isolation = [ `HyperV | `Process | `Default ] [@@deriving sexp]
let isolations : (isolation * string) list = [(`HyperV, "hyperv"); (`Process, "process"); (`Default, "default")]

type t = {
  docker_cpus : float;
  docker_isolation : isolation;
  docker_memory : string option;
  docker_network : string;   (* Default network, overridden by network stanza *)
}

type config = {
  cpus : float;
  isolation : isolation;
  memory : string option;
  network : string;
} [@@deriving sexp]

let secrets_guest_root = if Sys.win32 then {|C:\ProgramData\obuilder\|} else "/run/secrets/obuilder"
let secret_dir id = "secrets" / string_of_int id

module Docker_config = struct
  let make {Config.cwd; argv; hostname; user; env; mounts; network; mount_secrets; entrypoint}
      ?(config_dir="")
      ({docker_cpus; docker_isolation; docker_memory; _} : t) =
    assert (entrypoint <> None);
    let mounts = List.concat_map Docker.mount_args mounts in
    let env = env |> List.concat_map (fun (k, v) -> [ "--env"; strf "%s=%s" k v ]) in
    let network = network |> List.concat_map (fun network -> ["--network"; network]) in
    let user =
      match user with
      | `Unix { Obuilder_spec.uid; gid } when not Sys.win32 -> ["--user"; strf "%d:%d" uid gid]
      | `Windows { name } when Sys.win32 -> ["--user"; name]
      | _ -> assert false
    in
    let mount_secrets =
      let id = ref (-1) in
      List.concat_map (fun _ ->
          incr id;
          Config.Mount.{ty = `Bind; src = config_dir / secret_dir !id; dst = secrets_guest_root / secret_dir !id; readonly = true }
          |> Docker.mount_args) mount_secrets in
    let memory = Option.fold ~none:[] ~some:(fun m -> ["--memory"; m]) docker_memory in
    let docker_argv = [
      "--cpus"; strf "%f" docker_cpus;
      "--isolation"; (List.assoc docker_isolation isolations);
      "--hostname"; hostname;
      "--workdir"; cwd;
      "--entrypoint"; Option.get entrypoint;
    ] @ memory @ user @ env @ mounts @ mount_secrets @ network in
    docker_argv, argv
end

let secrets_layer ~log mount_secrets base_image container docker_argv =
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

    Lwt_result.bind_lwt
      (Docker.Cmd_log.run_result ~log ~name:container docker_argv base_image argv)
      (fun () ->
         let* () = Docker.Cmd_log.commit ~log base_image container base_image in
         Docker.Cmd_log.rm ~log [container])

let teardown ~log ~commit id =
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let target_image = Docker.docker_image id in
  let* () =
    if commit then Docker.Cmd_log.commit ~log base_image container target_image
    else Lwt.return_unit
  in
  Docker.Cmd_log.rm ~log [container]

let run ~cancelled ?stdin ~log t config (id:S.id) =
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
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let proc =
    Lwt_result.bind
      (secrets_layer ~log config.Config.mount_secrets base_image container docker_argv)
      (fun () ->
         let* r = Docker.Cmd.exists container in
         let* () =
           if Result.is_ok r then begin
             let `Docker_container name = container in
             Log.warn (fun f -> f "Removing left over container %s." name);
             Docker.Cmd.rm [ container ]
           end else
             Lwt.return_unit
         in
         let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
         Docker.Cmd_log.run_result ~log ?stdin ~name:container docker_argv base_image argv)
  in
  Lwt.on_termination cancelled (fun () ->
      let aux () =
        if Lwt.is_sleeping proc then (
          Docker.Cmd_log.rm ~log [container]
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  let* r = proc in
  let+ () = match r with
    | Ok () -> Lwt.return_unit
    | _ -> Docker.Cmd_log.rm ~log [container]
  in
  if Lwt.is_sleeping cancelled then (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Error `Cancelled

let tar_in ~cancelled ?stdin ~log t config result_tmp =
  run ~cancelled ?stdin ~log t config result_tmp

(* Duplicate of Build.hostname. *)
let hostname = "builder"

let manifest_from_build t ~base ~exclude src workdir user =
  let argv =
    (* FIXME: pipe the list of files to manifest.bash *)
    Printf.sprintf "exec %s %S %S %d %s %d %s"
      (Docker.mount_point_inside_unix // Docker.obuilder_libexec () // "manifest.bash")
      workdir
      "/"
      (List.length exclude)
      (String.concat " " (List.map Filename.quote exclude))
      (List.length src)
      (String.concat " " (List.map Filename.quote src))
  in
  let config =
    let entrypoint, argv = Docker.setup_command ~entp:Docker.(bash_entrypoint (obuilder_libexec ())) ~cmd:[argv] in
    Config.v
      ~cwd:workdir
      ~argv
      ~hostname
      ~user
      ~env:["PATH", if Sys.win32 then Docker.mount_point_inside_unix // Docker.obuilder_libexec () else "/bin:/usr/bin"]
      ~mount_secrets:[]
      ~mounts:[Docker.obuilder_libexec_volume ()]
      ~network:[]
      ~entrypoint
      ()
  in
  let docker_args, args = Docker_config.make config t in
  Docker.Cmd.run_pread_result ~rm:true docker_args (Docker.docker_image base) args >>!= fun manifests ->
  match Parsexp.Many.parse_string manifests with
  | Ok ts -> List.rev_map Manifest.t_of_sexp ts |> Lwt_result.return
  | Error e -> Lwt_result.fail (`Msg (Parsexp.Parse_error.message e))

let manifest_files_from op fd =
  let copy_root manifest =
    let list = Manifest.to_from_files ~null:true manifest in
    Os.write_all_string fd list 0 (String.length list)
  in
  match op with
  | `Copy_items (src_manifest, _) -> Lwt_list.iter_s copy_root src_manifest
  | `Copy_item (src_manifest, _) -> copy_root src_manifest

let tarball_from_build t ~log ~files_from ~tar workdir user id =
  let entrypoint =
    if Sys.win32 then Docker.mount_point_inside_native / Docker.obuilder_libexec () / "tar.exe"
    else "tar"
  in
  let argv =
    [ "-cf-"; "--format=gnu";
      "--directory"; workdir;
      (* Beware, the order is meaningful: --files-from should come last. *)
      "--verbatim-files-from"; "--null"; "--absolute-names"; "--files-from=-" ]
  in
  let config =
    Config.v
      ~cwd:workdir
      ~argv
      ~hostname
      ~user
      ~env:[]
      ~mount_secrets:[]
      ~mounts:[Docker.obuilder_libexec_volume ()]
      ~network:[]
      ~entrypoint
      ()
  in
  let docker_args, args = Docker_config.make config t in
  (* FIXME: on Windows, the Docker container producing the tar archive never
     stops for an unkwnown reason. However, if in the transform step ocaml-tar
     reads the end-of-tar magic sequence, then we can close the output pipe of
     the Docker process and ignore the error. *)
  let is_success = if Sys.win32 then Some (function 0 | 1 -> true | _ -> false) else None in
  Docker.Cmd_log.run' ~log ~stdin:(`FD_move_safely files_from) ~stdout:(`FD_move_safely tar)
    ~rm:true ?is_success docker_args (Docker.docker_image id) args

let transform op ~user ~from_tar ~to_untar =
  match op with
  | `Copy_items (src_manifest, dst_dir) ->
    Tar_transfer.transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar
  | `Copy_item (src_manifest, dst) ->
    Tar_transfer.transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar

let untar t ~cancelled ~stdin ~log ?dst_dir id =
  let entrypoint, argv =
    if Sys.win32 && dst_dir <> None then
      "powershell",           (* PowerShell 6 *)
      ["-Command";
       (* Extracting the tarball changes the permissions of the destination
          directory, making it un-writable by ContainerAdministrator, even if
          the permissions should be set correctly in the tar header. Backup
          and restore these permissions. *)
       Printf.sprintf {|$path = "%s"; if (Test-Path -Path $path -PathType Container) { $acl = Get-Acl -Path $path }; & %s/tar.exe -xpf - --verbose; if ($acl -ne $null) { Set-Acl -Path $path $acl }|}
         (Option.get dst_dir) (Docker.mount_point_inside_native // Docker.obuilder_libexec ()) ]
    else begin
      assert (dst_dir = None);
      "tar", ["-xpf"; "-"; "--verbose"]
    end in
  let config = Config.v
      ~cwd:(if Sys.unix then "/" else "C:/")
      ~argv
      ~hostname
      ~user:Obuilder_spec.root
      ~env:[]
      ~mount_secrets:[]
      ~mounts:(if Sys.win32 then [Docker.obuilder_libexec_volume ()] else [])
      ~network:[]
      ~entrypoint
      ()
  in
  Lwt_result.bind_lwt
    (run ~cancelled ~stdin ~log t config id)
    (fun () -> teardown ~log ~commit:true id)

let copy_from_context t ~cancelled ~log op ~user ~src_dir ?dst_dir id =
  (* If the sending thread finishes (or fails), close the writing end
     of the pipe immediately so that the untar process finishes too. *)
  Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
  let proc = untar t ~cancelled ~stdin:from_us ~log ?dst_dir id in
  let send =
    Lwt.finalize
      (fun () ->
         match op with
         | `Copy_items (src_manifest, dst_dir) ->
           Tar_transfer.send_files ~src_dir ~src_manifest ~dst_dir ~to_untar ~user
         | `Copy_item (src_manifest, dst) ->
           Tar_transfer.send_file ~src_dir ~src_manifest ~dst ~to_untar ~user
      )
      (fun () -> Lwt_unix.close to_untar) in
  let* result = proc in
  let+ () = send in
  result

let copy_from_build t ~cancelled ~log op ~user ~workdir ?dst_dir ~from_id id =
  (* If a sending thread finishes (or fails), close the writing end of
     the pipes immediately so that the receiving processes may finish
     too. *)
  Lwt_switch.with_switch @@ fun switch ->
  let kill () = Lwt_switch.turn_off switch in
  let kill_exn exn = let+ () = kill () in raise exn in
  let tarball ~tar () =
    Os.with_pipe_to_child @@ fun ~r:files_from ~w:files_from_out ->
    let proc = tarball_from_build ~log t ~files_from ~tar workdir user from_id in
    let f () = Os.ensure_closed_lwt files_from_out in
    let send = Lwt.try_bind (fun () ->
        let* () = manifest_files_from op files_from_out in
        f ())
        f kill_exn in
    let* () = Lwt_switch.add_hook_or_exec (Some switch) f in
    let* result = proc in
    let+ () = send in
    result
  in
  let transform ~to_untar () =
    Os.with_pipe_from_child @@ fun ~r:from_tar ~w:tar ->
    let f () = Os.ensure_closed_lwt from_tar in
    let proc =
      let* () = transform op ~user ~from_tar ~to_untar in
      f ()
    in
    let send = Lwt.try_bind (tarball ~tar) f kill_exn in
    let* () = Lwt_switch.add_hook_or_exec (Some switch) f in
    let* result = proc in
    let+ () = send in
    result
  in
  Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
  let proc = untar t ~cancelled ~stdin:from_us ~log ?dst_dir id in
  let f () = Os.ensure_closed_lwt to_untar in
  let send = Lwt.try_bind (transform ~to_untar) f kill_exn in
  let* () = Lwt_switch.add_hook_or_exec (Some switch) f in
  let* result = proc in
  let+ () = send in
  result

(* The container must be based on the same version as the host. *)
let servercore =
  let img = ref None in
  fun () ->
  match !img with
  | None ->
    let keyname = {|HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion|} in
    let valuename = "CurrentBuild" in
    let* value = Os.pread ["reg"; "query"; keyname; "/v"; valuename] in
    let line = String.(value |> trim |> split_on_char '\n') |> Fun.flip List.nth 1 in
    Scanf.sscanf line " CurrentBuild REG_SZ %i" @@ fun version ->
    let version' = match version with
      (* Prior to Windows 11 and Server 2022, the build number of the Windows
       * must match the build number of container *)
      | 14393 -> "ltsc2016" (* aka 1607 *)
      | 16299 -> "1709"
      | 17134 -> "1803"
      | 17763 -> "ltsc2019" (* aka 1809 *)
      | 18362 -> "1903"
      | 18363 -> "1909"
      | 19041 -> "2004"
      | 19042 -> "20H2"
      | _ -> "ltsc2022"
    in
    let img' = "mcr.microsoft.com/windows/servercore:" ^ version' in
    Log.info (fun f -> f "Windows host is build %i, will use tag %s." version img');
    img := Some (Lwt.return (`Docker_image img'));
    Option.get !img
  | Some img -> img

(* Windows ships a bsdtar that doesn't support symlinks (neither when
   creating the tar archive, nor when extracting it). We need a
   working tar for copying files in and out Docker images, so we pull
   Cygwin, install it, and extract tar and its dependencies in a
   Docker volume that is mounted each time we need tar.

   On Linux, we assume a tar is always present in /usr/bin/tar.

   We use `manifest.bash', an implementation of {!Manifest} in Bash, to
   extract the tar manifest from the Docker image. *)
let create_tar_volume (t:t) =
  Log.info (fun f -> f "Preparing tar volume…");
  let name = Docker.obuilder_libexec () in
  let vol = `Docker_volume name and img = `Docker_image name in
  let* _ = Docker.Cmd.volume (`Create vol) in

  let* (`Docker_image base) = if Sys.win32 then servercore () else Lwt.return (`Docker_image "busybox") in

  let* config =
    if Sys.win32 then
      let destination = Docker.(mount_point_inside_native // obuilder_libexec ()) in
      let dockerfile =
        "# escape=`\n" ^ (strf "FROM %s\n" base) ^ {|
       ENV CYGWIN="winsymlinks:native"
       ADD [ "https://www.cygwin.com/setup-x86_64.exe", "C:\\cygwin-setup-x86_64.exe" ]
       RUN mkdir C:\cygwin64\lib\cygsympathy && mkdir C:\cygwin64\etc\postinstall
       ADD [ "https://raw.githubusercontent.com/metastack/cygsympathy/master/cygsympathy.cmd", "C:\\cygwin64\\lib\\cygsympathy\\" ]
       ADD [ "https://raw.githubusercontent.com/metastack/cygsympathy/master/cygsympathy.sh", "C:\\cygwin64\\lib\\cygsympathy\\cygsympathy" ]
       RUN mklink C:\cygwin64\etc\postinstall\zp_zcygsympathy.sh C:\cygwin64\lib\cygsympathy\cygsympathy
       RUN C:\cygwin-setup-x86_64.exe --quiet-mode --no-shortcuts --no-startmenu `
         --no-desktop --only-site --local-package-dir %TEMP% --root C:\cygwin64 `
         --site http://mirrors.kernel.org/sourceware/cygwin/ `
         --packages tar
       COPY [ "extract.cmd", "C:/extract.cmd" ]
       COPY [ "manifest.bash", "C:/manifest.bash" ]
      |} in

      let+ () = Lwt_io.with_temp_dir ~perm:0o700 @@ fun temp_dir ->
        let write_file dst ?(perm=0o400) contents =
          Lwt_io.(with_file ~perm ~mode:Output (temp_dir / dst)) @@ fun ch ->
          Lwt_io.fprint ch contents in
        let* () = write_file "Dockerfile" dockerfile in
        let* () = write_file "extract.cmd" ~perm:0o500 (Option.get (Static_files.read "extract.cmd")) in
        let* () = write_file "manifest.bash" ~perm:0o500 (Option.get (Static_files.read "manifest.bash")) in
        let docker_argv = [
          "--isolation"; List.assoc t.docker_isolation isolations;
          "--network"; t.docker_network;
        ] in
        Docker.Cmd.build docker_argv img temp_dir
      in

      let entrypoint, argv = {|C:\Windows\System32\cmd.exe|}, ["/S"; "/C"; {|C:\extract.cmd|}] in
      Config.v ~cwd:{|C:/|} ~argv ~hostname:""
        ~user:Obuilder_spec.((root_windows :> user))
        ~env:["DESTINATION", destination]
        ~mount_secrets:[]
        ~mounts:[Docker.obuilder_libexec_volume ~readonly:false ()]
        ~network:[]
        ~entrypoint
        ()

    else
      let destination = Docker.(mount_point_inside_native / obuilder_libexec ()) in
      let dockerfile = strf "FROM %s\n" base ^ strf {|COPY [ "manifest.bash", "%s/manifest.bash" ]|} destination in
      let+ () = Lwt_io.with_temp_dir ~perm:0o700 @@ fun temp_dir ->
        let write_file dst ?(perm=0o400) contents =
          Lwt_io.(with_file ~perm ~mode:Output (temp_dir / dst)) @@ fun ch ->
          Lwt_io.fprint ch contents in
        let* () = write_file "Dockerfile" dockerfile in
        let* () = write_file "manifest.bash" ~perm:0o500 (Option.get (Static_files.read "manifest.bash")) in
        let docker_argv = [
            "--isolation"; List.assoc t.docker_isolation isolations;
            "--network"; t.docker_network;
        ] in
        Docker.Cmd.build docker_argv img temp_dir
      in

      let entrypoint, argv = "/bin/sh", ["-c"; ":"] in
      Config.v ~cwd:"/" ~argv ~hostname:""
        ~user:Obuilder_spec.((root_unix :> user))
        ~env:["DESTINATION", destination]
        ~mount_secrets:[]
        ~mounts:[Docker.obuilder_libexec_volume ~readonly:false ()]
        ~network:[]
        ~entrypoint
        ()
  in
  let docker_args, args = Docker_config.make config t in
  let* () = Docker.Cmd.run ~rm:true docker_args img args in
  Docker.Cmd.image (`Remove img)

let create (c : config) =
  let t = { docker_cpus = c.cpus; docker_isolation = c.isolation;
            docker_memory = c.memory; docker_network = c.network; } in
  let* volume_exists = Docker.Cmd.exists (`Docker_volume (Docker.obuilder_libexec ())) in
  let+ () = if Result.is_error volume_exists then create_tar_volume t else Lwt.return_unit in
  t

let finished () =
  Lwt.return ()

open Cmdliner

let docs = "DOCKER BACKEND"

let cpus =
  Arg.value @@
  Arg.opt Arg.float 2.0 @@
  Arg.info ~docs
    ~doc:"Number of CPUs to be used by Docker."
    ~docv:"CPUS"
    ["docker-cpus"]

let isolation =
  let isolations = List.rev_map (fun (k, v) -> v, k) isolations in
  let doc = Arg.doc_alts_enum isolations |> strf
              "Docker isolation, must be %s. Only $(b,default) is available on \
               Linux, only $(b,process) and $(b,hyperv) are available on Windows." in
  Arg.value @@
  Arg.opt (Arg.enum isolations) (if Sys.win32 then `HyperV else `Default) @@
  Arg.info ~doc ~docs
    ~docv:"ISOLATION"
    ["docker-isolation"]

let memory =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info ~docs
    ~doc:"The maximum amount of memory the container can use. A positive \
          integer, followed by a suffix of b, k, m, g, to indicate bytes, \
          kilobytes, megabytes, or gigabytes."
    ~docv:"MEMORY"
    ["docker-memory"]

let network =
  Arg.value @@
  Arg.opt Arg.string (if Sys.unix then "host" else "nat") @@
  Arg.info ~docs
    ~doc:"Docker network used for the Docker backend setup."
    ~docv:"NETWORK"
    ["docker-network"]

let cmdliner : config Term.t =
  let make cpus isolation memory network =
    { cpus; isolation; memory; network; }
  in
  Term.(const make $ cpus $ isolation $ memory $ network)
