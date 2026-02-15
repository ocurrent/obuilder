open Sexplib.Conv

include S.Sandbox_default

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
    Ok ()
  else
    let docker_argv, argv =
      if Sys.win32 then
        docker_argv @ ["--entrypoint"; {|C:\Windows\System32\cmd.exe|}],
        ["/S"; "/C"; String.concat " " argv]
      else
        docker_argv @ ["--entrypoint"; {|/bin/sh|}],
        ["-c"; String.concat " " argv]
    in
    match Docker.Cmd_log.run_result ~log ~name:container docker_argv base_image argv with
    | Error _ as e -> e
    | Ok () ->
      Docker.Cmd_log.commit ~log base_image container base_image;
      Docker.Cmd_log.rm ~log [container];
      Ok ()

let teardown ~log ~commit id =
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let target_image = Docker.docker_image id in
  if commit then Docker.Cmd_log.commit ~log base_image container target_image;
  Docker.Cmd_log.rm ~log [container]

let run ~cancelled ?stdin ~log t config (id:S.id) =
  let tmp = Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "obuilder-docker-" "" in
  Fun.protect ~finally:(fun () -> Os.rm ~directory:tmp) @@ fun () ->
  Unix.chmod tmp 0o700;
  let docker_argv, argv = Docker_config.make config ~config_dir:tmp t in
  let _ = List.fold_left
      (fun id Config.Secret.{value; _} ->
         Os.ensure_dir (tmp / "secrets");
         Os.ensure_dir (tmp / secret_dir id);
         Os.write_file ~path:(tmp / secret_dir id / "secret") value;
         id + 1
      ) 0 config.mount_secrets
  in
  let container = Docker.docker_container id in
  let base_image = Docker.docker_image ~tmp:true id in
  let r =
    match secrets_layer ~log config.Config.mount_secrets base_image container docker_argv with
    | Error _ as e -> e
    | Ok () ->
      let r = Docker.Cmd.exists container in
      if Result.is_ok r then begin
        let `Docker_container name = container in
        Log.warn (fun f -> f "Removing left over container %s." name);
        Docker.Cmd.rm [ container ]
      end;
      let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
      Docker.Cmd_log.run_result ~log ?stdin ~name:container docker_argv base_image argv
  in
  (* Check cancellation after process completes *)
  (match r with
   | Ok () -> ()
   | _ -> Docker.Cmd_log.rm ~log [container]);
  if Eio.Promise.is_resolved cancelled then Error `Cancelled
  else (r :> (unit, [`Msg of string | `Cancelled]) result)

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
  match Docker.Cmd.run_pread_result ~rm:true docker_args (Docker.docker_image base) args with
  | Error _ as e -> e
  | Ok manifests ->
    match Parsexp.Many.parse_string manifests with
    | Ok ts -> Ok (List.rev_map Manifest.t_of_sexp ts)
    | Error e -> Error (`Msg (Parsexp.Parse_error.message e))

let manifest_files_from op fd =
  let copy_root manifest =
    let list = Manifest.to_from_files ~null:true manifest in
    Os.write_all_string fd list 0 (String.length list)
  in
  match op with
  | `Copy_items (src_manifest, _) -> List.iter copy_root src_manifest
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
  match run ~cancelled ~stdin ~log t config id with
  | Error _ as e -> e
  | Ok () ->
    teardown ~log ~commit:true id;
    Ok ()

let copy_from_context t ~cancelled ~log op ~user ~src_dir ?dst_dir id =
  (* If the sending thread finishes (or fails), close the writing end
     of the pipe immediately so that the untar process finishes too. *)
  Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
  let proc = untar t ~cancelled ~stdin:from_us ~log ?dst_dir id in
  Fun.protect
    (fun () ->
       match op with
       | `Copy_items (src_manifest, dst_dir) ->
         Tar_transfer.send_files ~src_dir ~src_manifest ~dst_dir ~to_untar ~user
       | `Copy_item (src_manifest, dst) ->
         Tar_transfer.send_file ~src_dir ~src_manifest ~dst ~to_untar ~user
    )
    ~finally:(fun () -> (try Unix.close to_untar with Unix.Unix_error _ -> ()));
  proc

let copy_from_build t ~cancelled ~log op ~user ~workdir ?dst_dir ~from_id id =
  (* If a sending thread finishes (or fails), close the writing end of
     the pipes immediately so that the receiving processes may finish
     too. *)
  Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
  let proc = untar t ~cancelled ~stdin:from_us ~log ?dst_dir id in
  Fun.protect
    (fun () ->
       Os.with_pipe_from_child @@ fun ~r:from_tar ~w:tar ->
       Fun.protect
         (fun () ->
            Os.with_pipe_to_child @@ fun ~r:files_from ~w:files_from_out ->
            Fun.protect
              (fun () ->
                 manifest_files_from op files_from_out;
                 (try Unix.close files_from_out with Unix.Unix_error _ -> ()))
              ~finally:(fun () -> (try Unix.close files_from_out with Unix.Unix_error _ -> ()));
            tarball_from_build ~log t ~files_from ~tar workdir user from_id)
         ~finally:(fun () -> (try Unix.close from_tar with Unix.Unix_error _ -> ()));
       transform op ~user ~from_tar ~to_untar)
    ~finally:(fun () -> (try Unix.close to_untar with Unix.Unix_error _ -> ()));
  proc

(* The container must be based on the same version as the host. *)
let servercore =
  let img = ref None in
  fun () ->
  match !img with
  | None ->
    let keyname = {|HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion|} in
    let valuename = "CurrentBuild" in
    let value = Os.pread ["reg"; "query"; keyname; "/v"; valuename] in
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
    img := Some (`Docker_image img');
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
  Log.info (fun f -> f "Preparing tar volume...");
  let name = Docker.obuilder_libexec () in
  let vol = `Docker_volume name and img = `Docker_image name in
  let _ = Docker.Cmd.volume (`Create vol) in

  let (`Docker_image base) = if Sys.win32 then servercore () else (`Docker_image "busybox") in

  let config =
    if Sys.win32 then begin
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

      let temp_dir = Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "obuilder-tar-" "" in
      Fun.protect ~finally:(fun () -> Os.rm ~directory:temp_dir) (fun () ->
        Unix.chmod temp_dir 0o700;
        let write_file dst ?(perm=0o400) contents =
          let path = temp_dir / dst in
          let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC] perm in
          Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
            Os.write_all_string fd contents 0 (String.length contents))
        in
        write_file "Dockerfile" dockerfile;
        write_file "extract.cmd" ~perm:0o500 (Option.get (Static_files.read "extract.cmd"));
        write_file "manifest.bash" ~perm:0o500 (Option.get (Static_files.read "manifest.bash"));
        let docker_argv = [
          "--isolation"; List.assoc t.docker_isolation isolations;
          "--network"; t.docker_network;
        ] in
        Docker.Cmd.build docker_argv img temp_dir);

      let entrypoint, argv = {|C:\Windows\System32\cmd.exe|}, ["/S"; "/C"; {|C:\extract.cmd|}] in
      Config.v ~cwd:{|C:/|} ~argv ~hostname:""
        ~user:Obuilder_spec.((root_windows :> user))
        ~env:["DESTINATION", destination]
        ~mount_secrets:[]
        ~mounts:[Docker.obuilder_libexec_volume ~readonly:false ()]
        ~network:[]
        ~entrypoint
        ()

    end else begin
      let destination = Docker.(mount_point_inside_native / obuilder_libexec ()) in
      let dockerfile = strf "FROM %s\n" base ^ strf {|COPY [ "manifest.bash", "%s/manifest.bash" ]|} destination in

      let temp_dir = Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "obuilder-tar-" "" in
      Fun.protect ~finally:(fun () -> Os.rm ~directory:temp_dir) (fun () ->
        Unix.chmod temp_dir 0o700;
        let write_file dst ?(perm=0o400) contents =
          let path = temp_dir / dst in
          let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC] perm in
          Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
            Os.write_all_string fd contents 0 (String.length contents))
        in
        write_file "Dockerfile" dockerfile;
        write_file "manifest.bash" ~perm:0o500 (Option.get (Static_files.read "manifest.bash"));
        let docker_argv = [
            "--isolation"; List.assoc t.docker_isolation isolations;
            "--network"; t.docker_network;
        ] in
        Docker.Cmd.build docker_argv img temp_dir);

      let entrypoint, argv = "/bin/sh", ["-c"; ":"] in
      Config.v ~cwd:"/" ~argv ~hostname:""
        ~user:Obuilder_spec.((root_unix :> user))
        ~env:["DESTINATION", destination]
        ~mount_secrets:[]
        ~mounts:[Docker.obuilder_libexec_volume ~readonly:false ()]
        ~network:[]
        ~entrypoint
        ()
    end
  in
  let docker_args, args = Docker_config.make config t in
  Docker.Cmd.run ~rm:true docker_args img args;
  Docker.Cmd.image (`Remove img)

let create (c : config) =
  let t = { docker_cpus = c.cpus; docker_isolation = c.isolation;
            docker_memory = c.memory; docker_network = c.network; } in
  let volume_exists = Docker.Cmd.exists (`Docker_volume (Docker.obuilder_libexec ())) in
  if Result.is_error volume_exists then create_tar_volume t;
  t

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
