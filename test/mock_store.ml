open Lwt.Infix

module Os = Obuilder.Os

let ( >>!= ) = Lwt_result.bind
let ( / ) = Filename.concat

type t = {
  dir : string;
}

let delay_store = ref Lwt.return_unit

let build t ?base ~id fn =
  base |> Option.iter (fun base -> assert (not (String.contains base '/')));
  let dir = t.dir / id in
  assert (Os.check_dir dir = `Missing);
  let tmp_dir = dir ^ ".part" in
  begin match base with
    | None -> Os.ensure_dir tmp_dir; Lwt.return_unit
    | Some base ->
      Lwt_process.exec ("", [| "cp"; "-r"; t.dir / base; tmp_dir |]) >>= function
      | Unix.WEXITED 0 -> Lwt.return_unit
      | _ -> failwith "cp failed!"
  end >>= fun () ->
  fn tmp_dir >>= fun r ->
  !delay_store >>= fun () ->
  match r with
  | Ok () ->
    Unix.rename tmp_dir dir;
    Lwt_result.return ()
  | Error _ as e ->
    Lwt.return e

let state_dir t = t.dir / "state"

let with_store fn =
  Lwt_io.with_temp_dir ~prefix:"mock-store-" @@ fun dir ->
  let t = { dir } in
  Obuilder.Os.ensure_dir (state_dir t);
  fn t

let add t id fn =
  let dir = t.dir / id in
  match Os.check_dir dir with
  | `Present -> Fmt.failwith "%S is already in the store!" id
  | `Missing ->
    Os.ensure_dir dir;
    fn dir

let path t id = t.dir / id

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None
