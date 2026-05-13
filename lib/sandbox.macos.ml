open Lwt.Infix
open Cmdliner

include S.Sandbox_default

type t = {
  uid: int;
  gid: int;
  (* mount point where Homebrew is installed. Either /opt/homebrew or /usr/local depending upon architecture *)
  brew_path : string;
  lock : Lwt_mutex.t;
}

open Sexplib.Conv

type config = {
  uid: int;
  brew_path : string;
}[@@deriving sexp]

let run_as ~env ~user ~cmd =
  let command =
    let env = String.concat " " (List.map (fun (k, v) -> Filename.quote (k^"="^v)) env) in
    "sudo" :: "su" :: "-l" :: user :: "-c" :: "--"
    :: Printf.sprintf {|source ~/.obuilder_profile.sh && env %s "$0" "$@"|} env
    :: cmd
  in
  Log.debug (fun f -> f "Running: %s" (String.concat " " command));
  command

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

let user_name ~prefix ~uid =
  Fmt.str "%s%i" prefix uid

let zfs_volume_from path =
  String.split_on_char '/' path
  |> List.filter (fun x -> String.length x > 0)
  |> List.tl
  |> String.concat "/"

let secrets_env_var = "OBUILDER_SECRETS_DIR"

(* Stage secrets in a private /tmp directory, one file per secret named
   by the basename of [secret.target]. The build locates them via the
   [OBUILDER_SECRETS_DIR] environment variable (e.g.
   "$OBUILDER_SECRETS_DIR/github_token"). Returns the staging directory
   (or [None] if there were no secrets) and the list of file paths
   created so that [cleanup_secrets] can undo everything. *)
let setup_secrets ~uid ~gid mount_secrets =
  match mount_secrets with
  | [] -> Lwt.return (None, [])
  | _ ->
    let tmp = Filename.temp_dir ~temp_dir:"/tmp" "obuilder-macos-secrets-" "" in
    Unix.chmod tmp 0o755;
    let owner = Printf.sprintf "%d:%d" uid gid in
    let names = List.map (fun { Config.Secret.target; _ } -> Filename.basename target) mount_secrets in
    let unique = List.sort_uniq String.compare names in
    if List.length unique <> List.length names then
      Fmt.failwith "Two secrets resolve to the same filename in %s; \
                    give each secret a distinct target basename." secrets_env_var;
    Lwt_list.fold_left_s
      (fun paths { Config.Secret.value; target } ->
         let src = Filename.concat tmp (Filename.basename target) in
         Os.write_file ~path:src value >>= fun () ->
         Os.sudo [ "chown"; owner; src ] >>= fun () ->
         Os.sudo [ "chmod"; "0400"; src ] >>= fun () ->
         Lwt.return (src :: paths))
      [] mount_secrets
    >>= fun paths ->
    Lwt.return (Some tmp, List.rev paths)

let cleanup_secrets ~tmp ~paths =
  match tmp with
  | None -> Lwt.return_unit
  | Some tmp ->
    Lwt_list.iter_s (fun src -> Os.sudo [ "rm"; "-f"; src ]) paths
    >>= fun () ->
    Os.sudo [ "rmdir"; tmp ]

let run ~cancelled ?stdin:stdin ~log (t : t) config result_tmp =
  Lwt_mutex.with_lock t.lock (fun () ->
  Log.info (fun f -> f "result_tmp = %s" result_tmp);
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let user = user_name ~prefix:"mac" ~uid:t.uid in
  let zfs_volume = zfs_volume_from result_tmp in
  let home_dir = Filename.concat "/Users/" user in
  let zfs_home_dir = Filename.concat zfs_volume "home" in
  let zfs_brew = Filename.concat zfs_volume "brew" in
  Os.sudo [ "zfs"; "set"; "mountpoint=" ^ home_dir; zfs_home_dir ] >>= fun () ->
  Os.sudo [ "zfs"; "set"; "mountpoint=" ^ t.brew_path; zfs_brew ] >>= fun () ->
  Lwt_list.iter_s (fun { Config.Mount.src; dst; readonly; _ } ->
    Log.info (fun f -> f "src = %s, dst = %s, type %s" src dst (if readonly then "ro" else "rw") );
    if Sys.file_exists dst then
      Os.sudo [ "zfs"; "set"; "mountpoint=" ^ dst; zfs_volume_from src ]
    else Lwt.return_unit) config.Config.mounts >>= fun () ->
  let uid = string_of_int t.uid in
  let gid = string_of_int t.gid in
  Macos.create_new_user ~username:user ~home_dir ~uid ~gid >>= fun _ ->
  setup_secrets ~uid:t.uid ~gid:t.gid config.Config.mount_secrets >>= fun (secrets_tmp, secrets_paths) ->
  let osenv = config.Config.env in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc_id = ref None in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f ("", config.Config.argv) in
    Os.pread @@ Macos.get_tmpdir ~user >>= fun tmpdir ->
    let tmpdir = List.hd (String.split_on_char '\n' tmpdir) in
    let env =
      let base = ("TMPDIR", tmpdir) :: osenv in
      match secrets_tmp with
      | None -> base
      | Some d -> (secrets_env_var, d) :: base
    in
    let cmd = run_as ~env ~user ~cmd:config.Config.argv in
    Os.ensure_dir config.Config.cwd;
    let pid, proc = Os.open_process ?stdin ~stdout ~stderr ~pp ~cwd:config.Config.cwd cmd in
    proc_id := Some pid;
    Os.process_result ~pp proc >>= fun r ->
    Lwt.return r
  in
  Lwt.on_termination cancelled (fun () ->
    let aux () =
      if Lwt.is_sleeping proc then
        match !proc_id with
          | Some _ -> Macos.kill_users_processes ~uid:t.uid
          | None -> Log.warn (fun f -> f "Failed to find pid…"); Lwt.return ()
      else Lwt.return_unit (* Process has already finished *)
    in
    Lwt.async aux
  );
  proc >>= fun r ->
  copy_log >>= fun () ->
    cleanup_secrets ~tmp:secrets_tmp ~paths:secrets_paths >>= fun () ->
    Lwt_list.iter_s (fun { Config.Mount.src; dst = _; readonly = _; ty = _ } ->
      Os.sudo [ "zfs"; "inherit"; "mountpoint"; zfs_volume_from src ]) config.Config.mounts >>= fun () ->
    Macos.sudo_fallback [ "zfs"; "set"; "mountpoint=none"; zfs_home_dir ] [ "zfs"; "unmount"; "-f"; zfs_home_dir ] ~uid:t.uid >>= fun () ->
    Macos.sudo_fallback [ "zfs"; "set"; "mountpoint=none"; zfs_brew ] [ "zfs"; "unmount"; "-f"; zfs_brew ] ~uid:t.uid >>= fun () ->
    if Lwt.is_sleeping cancelled then
      Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
    else Lwt_result.fail `Cancelled)

let create ~state_dir:_ c =
  Lwt.return {
    uid = c.uid;
    gid = 1000;
    brew_path = c.brew_path;
    lock = Lwt_mutex.create ();
  }

let finished () =
  Os.sudo [ "zfs"; "unmount"; "obuilder/result" ] >>= fun () ->
  Os.sudo [ "zfs"; "mount"; "obuilder/result" ] >>= fun () ->
  Lwt.return ()

let uid =
  Arg.required @@
  Arg.opt Arg.(some int) None @@
  Arg.info
    ~doc:"The uid of the user that will be used as the builder. This should be unique and not in use. \
          You can run `dscl . -list /Users uid` to see all of the currently active users and their uids."
    ~docv:"UID"
    ["uid"]

let brew_path =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Directory where Homebrew is installed. Typically this is either /usr/local or /opt/homebrew."
    ~docv:"BREW_PATH"
    ["brew-path"]

let cmdliner : config Term.t =
  let make uid brew_path =
    { uid; brew_path }
  in
  Term.(const make $ uid $ brew_path)
