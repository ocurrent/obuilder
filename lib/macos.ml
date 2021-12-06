(* Extensions to the Os module for macOS *)
open Lwt.Syntax
open Os

let ( / ) = Filename.concat

let user_exists ~user =
  let+ s = pread ["sudo"; "dscl"; "."; "list"; "/Users"] in
  List.exists (Astring.String.equal user) (Astring.String.cuts ~sep:"\n" s)

(* Generates a new MacOS user called `<prefix><uid>' *)
let create_new_user ~username ~home_dir ~uid ~gid =
  let* exists = user_exists ~user:username in
  if exists then Lwt.return_ok ()
  else
    let user = "/Users" / username in
    let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
    let dscl = [ "dscl"; "."; "-create"; user ] in
    sudo_result ~pp:(pp "UniqueID") (dscl @ [ "UniqueID"; uid ]) >>!= fun _ ->
    sudo_result ~pp:(pp "PrimaryGroupID") (dscl @ [ "PrimaryGroupID"; gid ])
    >>!= fun _ ->
    sudo_result ~pp:(pp "UserShell") (dscl @ [ "UserShell"; "/bin/bash" ])
    >>!= fun _ ->
    sudo_result ~pp:(pp "NFSHomeDirectory") (dscl @ [ "NFSHomeDirectory"; home_dir ])

let delete_user ~user =
  let* exists = user_exists ~user in
  match exists with
    | false ->
      Log.info (fun f -> f "Not deleting %s as they don't exist" user);
      Lwt_result.return ()
    | true ->
      let user = "/Users" / user in
      let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
      let delete = ["dscl"; "."; "-delete"; user ] in
        sudo_result ~pp:(pp "Deleting") delete

let descendants ~pid =
  Lwt.catch
    (fun () ->
      let+ s = pread ["sudo"; "pgrep"; "-P"; string_of_int pid ] in
      let pids = Astring.String.cuts ~sep:"\n" s in
      List.filter_map int_of_string_opt pids)
    (* Errors if there are none, probably errors for other reasons too... *)
    (fun _ -> Lwt.return [])

let kill ~pid =
  let pp _ ppf = Fmt.pf ppf "[ KILL ]" in
  let delete = ["kill"; "-9";  string_of_int pid ] in
  let* t = sudo_result ~pp:(pp "KILL") delete in
    match t with
    | Ok () -> Lwt.return ()
    | Error (`Msg m) ->
      Log.warn (fun f -> f "Failed to kill process %i because %s" pid m);
      Lwt.return ()

let kill_all_descendants ~pid =
  let rec kill_all pid : unit Lwt.t =
    let* ds = descendants ~pid in
    let* () = Lwt_list.iter_s kill_all ds in
    kill ~pid
  in
    kill_all pid

let copy_template ~base ~local =
  let pp s ppf = Fmt.pf ppf "[ %s ]" s in
  sudo_result ~pp:(pp "RSYNC") ["rsync"; "-avq"; base ^ "/"; local]

let change_home_directory_for ~user ~home_dir =
  ["dscl"; "."; "-create"; "/Users/" ^ user ; "NFSHomeDirectory"; home_dir ]

(* Used by the FUSE filesystem to indicate where a users home directory should be ...*)
let update_scoreboard ~uid ~scoreboard ~home_dir =
  ["ln"; "-Fhs"; home_dir; scoreboard ^ "/" ^ string_of_int uid]

let remove_link ~uid ~scoreboard =
  [ "rm"; scoreboard ^ "/" ^ string_of_int uid ]

let get_tmpdir ~user =
  ["sudo"; "-u"; user; "-i"; "getconf"; "DARWIN_USER_TEMP_DIR"]
