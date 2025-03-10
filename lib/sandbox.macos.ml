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
    let env = ("TMPDIR", tmpdir) :: osenv in
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
  let pp s ppf = Fmt.pf ppf "[ zfs ] %s\n" s in
  Os.sudo_result ~pp:(pp "unmount") [ "zfs"; "unmount"; "obuilder/result" ] >>= fun _ ->
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
