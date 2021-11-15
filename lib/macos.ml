(* Extensions to the Os module for macOS *)
open Lwt.Infix
open Os

let ( / ) = Filename.concat

let user_exists ~user =
  pread ["sudo"; "dscl"; "."; "list"; "/Users"] >|= fun s ->
  List.exists (Astring.String.equal user) (Astring.String.cuts ~sep:"\n" s)

(* Generates a new MacOS user called `<prefix><uid>' *)
let create_new_user ~username ~home ~uid ~gid =
  user_exists ~user:username >>= begin function
    | true ->  Lwt.return_ok ()
    | false ->
      let user = "/Users" / username in
      let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
      let dscl = ["dscl"; "."; "-create"; user ] in
        sudo_result ~pp:(pp "UniqueID") (dscl @ ["UniqueID"; uid]) >>!= fun _ ->
        sudo_result ~pp:(pp "PrimaryGroupID") (dscl @ ["PrimaryGroupID"; gid]) >>!= fun _ ->
        sudo_result ~pp:(pp "UserShell") (dscl @ ["UserShell"; "/bin/bash"]) >>!= fun _ ->
        sudo_result ~pp:(pp "NFSHomeDirectory") (dscl @ ["NFSHomeDirectory"; home])
  end

let delete_user ~user =
  user_exists ~user >>= begin function
    | false ->
      Log.info (fun f -> f "Not deleting %s as they don't exist" user);
      Lwt_result.return ()
    | true ->
      let user = "/Users" / user in
      let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in
      let delete = ["dscl"; "."; "-delete"; user ] in
        sudo_result ~pp:(pp "Deleting") delete
  end

let descendants ~pid =
  Lwt.catch
    (fun () -> pread ["sudo"; "pgrep"; "-P"; pid ] >|= fun s -> Astring.String.cuts ~sep:"\n" s)
    (* Errors if there are none, probably errors for other reasons too... *)
    (fun _ -> Lwt.return [])

let kill ~pid =
  let pp s ppf = Fmt.pf ppf "[ %s ]" s in
  if String.length pid = 0 then (Log.warn (fun f -> f "Empty PID"); Lwt.return ())
  else begin
    let delete = ["kill"; "-9";  pid ] in
    sudo_result ~pp:(pp "KILL") delete >>= fun t ->
      match t with
      | Ok () -> Lwt.return ()
      | Error (`Msg m) -> (
        Log.warn (fun f -> f "Failed to kill process %s because %s" pid m);
        Lwt.return ()
      )
  end

let kill_all_descendants ~pid =
  let rec kill_all pid : unit Lwt.t =
    descendants ~pid >>= fun ds ->
    Lwt_list.iter_s kill_all ds >>= fun () ->
    kill ~pid
  in
    kill_all pid

let copy_template ~base ~local =
  let pp s ppf = Fmt.pf ppf "[ %s ]" s in
  sudo_result ~pp:(pp "RSYNC") ["rsync"; "-avq"; base ^ "/"; local]

let change_home_directory_for ~user ~homedir =
  ["dscl"; "."; "-create"; "/Users/" ^ user ; "NFSHomeDirectory"; homedir ]

(* Used by the FUSE filesystem to indicate where a users home directory should be ...*)
let update_scoreboard ~uid ~scoreboard ~homedir =
  ["ln"; "-Fhs"; homedir; scoreboard ^ "/" ^ string_of_int uid]

let remove_link ~uid ~scoreboard =
  [ "rm"; scoreboard ^ "/" ^ string_of_int uid ]

let get_tmpdir ~user =
  ["sudo"; "-u"; user; "-i"; "getconf"; "DARWIN_USER_TEMP_DIR"]
