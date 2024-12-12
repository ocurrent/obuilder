open Lwt.Infix
open Sexplib.Conv

include S.Sandbox_default

let ( / ) = Filename.concat

type t = {
  jail_name_prefix : string;
}

type config = unit [@@deriving sexp]

(* Find out the user name to use within the jail, by parsing the
   /etc/passwd file within the jail filesystem. This is roughly
   equivalent to what Unix.getpwuid would do.
   Note that the gid is currently ignored. *)
let jail_username rootdir config =
  match config.Config.user with
  | `Windows w -> w.name
  | `Unix { uid; _ } ->
    let pwdfile = rootdir / "etc" / "passwd" in
    let uidstr = string_of_int uid in
    let rec parse_line ch =
      match In_channel.input_line ch with
      | None -> None
      | Some line ->
        let fields = String.split_on_char ':' line in begin
          match fields with
          | name :: _pass :: uid :: _ ->
            if uid = uidstr then Some name else parse_line ch
          | _ -> parse_line ch
      end
    in
    match In_channel.with_open_text pwdfile parse_line with
    | None -> Fmt.failwith "No user found for uid %d" uid
    | Some name -> name

(* Compute the complete set of arguments passed to the jail(8) command:
   jail username, jail path, command to run, etc. *)
let jail_options config rootdir tmp_dir =
  let cache = match List.length config.Config.mounts with
    | 0 -> []
    | _ ->
      let path = tmp_dir / "fstab" in
      let rec print_fstab oc = function
        | [] -> close_out oc
        | { Config.Mount.src; dst; readonly; _ } :: tl ->
          let full = rootdir ^ dst in
          Os.ensure_dir full;
          Printf.fprintf oc "%s %s nullfs %s 0 0\n" src full (if readonly then "ro" else "rw");
          print_fstab oc tl in
      let oc = open_out path in
        print_fstab oc config.Config.mounts;
      [ "mount.fstab=" ^ path ] in
  let username = jail_username rootdir config in
  let commandline =
    let env = List.rev_map (fun (k, v) -> k ^ "='" ^ v ^ "'") config.env in
    let commandline = List.rev (List.rev_map Filename.quote config.argv) in
    let commandline =
      match env with
      | [] -> commandline
      | _ -> "env" :: List.rev_append env commandline
    in
    let commandline =
      String.concat " "
        ([ "cd" ; Filename.quote config.cwd ; "&&" ] @ commandline)
    in
    (* Ask for a login shell in order to properly source opam settings. *)
    [ "command=/usr/bin/su" ; "-l" ; username ; "-c" ; commandline ]
  in
  let path = "path=" ^ rootdir in
  let devfs_setup = "mount.devfs" in
  let options =
    let options = [ path ; devfs_setup ] @ cache in
    match config.network with
    | [ "host" ] ->
      "ip4=inherit" :: "ip6=inherit" :: "host=inherit" :: options
    | _ ->
      "exec.start=/sbin/ifconfig lo0 127.0.0.1/8" :: "vnet" :: options
  in
  List.rev_append options commandline

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

let jail_id = ref 0

let run ~cancelled ?stdin:stdin ~log (t : t) config rootdir =
  Lwt_io.with_temp_dir ~prefix:"obuilder-jail-" @@ fun tmp_dir ->
  let zfs_volume = String.sub rootdir 1 (String.length rootdir - 1) in  (* remove / from front *)
  Os.sudo [ "zfs"; "inherit"; "mountpoint"; zfs_volume ^ "/rootfs" ] >>= fun () ->
  let cwd = rootdir in
  let jail_name = t.jail_name_prefix ^ "_" ^ string_of_int !jail_id in
  incr jail_id;
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let rootdir = rootdir / "rootfs" in
  let workdir = rootdir / config.Config.cwd in
  (* Make sure the work directory exists prior to starting the jail. *)
  begin
    match Os.check_dir workdir with
    | `Present -> Lwt.return_unit
    | `Missing -> Os.sudo [ "mkdir" ; "-p" ; workdir ]
  end >>= fun () ->
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let cmd =
      let options = jail_options config rootdir tmp_dir in
      "jail" :: "-c" :: ("name=" ^ jail_name) :: options
    in
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f ("", cmd) in
    (* This is similar to
       Os.sudo_result ~cwd ?stdin ~stdout ~stderr ~pp cmd
       but also unmounting the in-jail devfs if necessary, see below. *)
    let cmd = if Os.running_as_root then cmd else "sudo" :: "--" :: cmd in
    Logs.info (fun f -> f "Exec %a" Os.pp_cmd ("", cmd));
    !Os.lwt_process_exec ~cwd ?stdin ~stdout ~stderr ~pp
      ("", Array.of_list cmd) >>= function
    | Ok 0 ->
      let fstab = tmp_dir / "fstab" in
      (if Sys.file_exists fstab
      then
        let cmd = [ "sudo" ; "/sbin/umount" ; "-a" ; "-F" ; fstab ] in
        Os.exec ~is_success:(fun _ -> true) cmd
      else Lwt.return_unit) >>= fun () ->
      (* If the command within the jail completes, the jail is automatically
         removed, but without performing any of the stop and release actions,
         thus we can not use "exec.stop" to unmount the in-jail devfs
         filesystem. Do this here, ignoring the exit code of umount(8). *)
      let cmd = [ "sudo" ; "/sbin/umount" ; rootdir / "dev" ] in
      Os.exec ~is_success:(fun _ -> true) cmd >>= fun () ->
      Lwt_result.ok Lwt.return_unit
    | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %d" pp n
    | Error e -> Lwt_result.fail e
  in
  Lwt.on_termination cancelled (fun () ->
    let rec aux () =
      if Lwt.is_sleeping proc then (
         let pp f = Fmt.pf f "jail -r obuilder" in
         Os.sudo_result ~cwd [ "jail" ; "-r" ; jail_name ] ~pp >>= function
         | Ok () -> Lwt.return_unit
         | Error (`Msg _) ->
           Lwt_unix.sleep 10.0 >>= aux
      ) else Lwt.return_unit (* Process has already finished *)
    in
    Lwt.async aux
  );
  proc >>= fun r ->
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then
    Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else
    Lwt_result.fail `Cancelled

let create ~state_dir:_ _c =
  Lwt.return {
    (* Compute a unique (across obuilder instances) name prefix for the jail. *)
    jail_name_prefix = "obuilder_" ^ (Int.to_string (Unix.getpid ()));
  }

open Cmdliner

let cmdliner : config Term.t =
  Term.(const ())
