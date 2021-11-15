open Lwt.Infix
open Cmdliner

type t = {
  uid: int;
  gid: int;
  (* Where zfs dynamic libraries are -- can't be in /usr/local/lib
     see notes in .mli file under "Various Gotchas"... *)
  fallback_library_path : string;
  (* Scoreboard -- where we keep our symlinks for knowing homedirs for users *)
  scoreboard : string;
}

open Sexplib.Conv

type config = {
  uid: int;
  fallback_library_path : string;
  scoreboard : string;
}[@@deriving sexp]

let run_as ~env ~user ~cmd =
  let command =
    let env = String.concat " " (List.map (fun (k, v) -> Filename.quote (k^"="^v)) env) in
    "sudo" :: "su" :: "-l" :: user :: "-c" :: "--" ::
    Printf.sprintf {|source ~/.obuilder_profile.sh && env %s "$0" "$@"|} env ::
    cmd
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

(* HACK: Unmounting and remounting the FUSE filesystem seems to "fix"
   some weird cachining bug, see https://github.com/patricoferris/obuilder/issues/9 *)
let post_build () =
  let f = ["umount"; "-f"; "/usr/local"] in
  Os.sudo f >>= fun _ -> Lwt.return ()

let rec pre_build (t : t) =
  let f = [ "obuilderfs"; t.scoreboard ; "/usr/local"; "-o"; "allow_other" ] in
  let pp ppf = Fmt.pf ppf "[ macFUSE ] " in
  Os.sudo_result ~pp f >>= function Ok _ -> Lwt.return () | Error (`Msg _) -> (post_build () >>= fun _ -> pre_build t)

let user_name ~prefix ~uid =
  Fmt.str "%s%i" prefix uid

(* A build step in macos:
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log (t : t) config homedir =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let homedir = Filename.concat homedir "rootfs" in
  let user = user_name ~prefix:"mac" ~uid:t.uid in
  let uid = string_of_int t.uid in
  Macos.create_new_user ~username:user ~home:homedir ~uid ~gid:"1000" >>= fun _ ->
  let set_homedir = Macos.change_home_directory_for ~user ~homedir in
  let update_scoreboard = Macos.update_scoreboard ~uid:t.uid ~homedir ~scoreboard:t.scoreboard in
  let osenv = config.Config.env in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc_id = ref None in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.Config.argv in
    Os.sudo_result ~pp set_homedir >>= fun _ ->
    Os.sudo_result ~pp update_scoreboard >>= fun _ ->
    pre_build t >>= fun _ ->
    Os.pread @@ Macos.get_tmpdir ~user >>= fun tmpdir ->
    let tmpdir = List.hd (String.split_on_char '\n' tmpdir) in
    let env = ("TMPDIR", tmpdir) :: osenv in
    let cmd = run_as ~env ~user ~cmd:config.Config.argv in
    Os.ensure_dir config.Config.cwd;
    let pid, proc = Os.open_process ?stdin ~stdout ~stderr ~pp ~cwd:config.Config.cwd cmd in
    proc_id := Some pid;
    Os.process_result ~pp proc >>= fun r ->
    post_build () >>= fun () ->
    Lwt.return r
  in
  Lwt.on_termination cancelled (fun () ->
    let aux () =
      if Lwt.is_sleeping proc then (
        match !proc_id with
          | Some pid -> Macos.kill_all_descendants ~pid
          | None -> Log.warn (fun f -> f "Failed to find pid..."); Lwt.return ()
          )
      else Lwt.return_unit  (* Process has already finished *)
    in
      Lwt.async aux
  );
  proc >>= fun r ->
  copy_log >>= fun () ->
    if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
    else Lwt_result.fail `Cancelled

let create ~state_dir:_ c =
  Lwt.return {
    uid = c.uid;
    gid = 1000;
    fallback_library_path = c.fallback_library_path;
    scoreboard = c.scoreboard;
  }

let uid =
  Arg.required @@
  Arg.opt Arg.(some int) None @@
  Arg.info
    ~doc:"The uid of the user that will be used as the builder. This should be unique and not in use. \
    You can run `dscl . -list /Users uid` to see all of the currently active users and their uids."
    ~docv:"UID"
    ["uid"]

let fallback_library_path =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The fallback path of the dynamic libraries. This is used whenever the FUSE filesystem \
    is in place preventing anything is /usr/local from being accessed."
    ~docv:"FALLBACK"
    ["fallback"]

let scoreboard =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The scoreboard directory which is used by the FUSE filesystem to record \
    the association between user id and home directory."
    ~docv:"SCOREBOARD"
    ["scoreboard"]

let cmdliner : config Term.t =
  let make uid fallback_library_path scoreboard =
    {uid; fallback_library_path; scoreboard}
  in
  Term.(const make $ uid $ fallback_library_path $ scoreboard)
