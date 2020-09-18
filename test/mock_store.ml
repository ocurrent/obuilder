open Lwt.Infix

module Os = Obuilder.Os

let ( / ) = Filename.concat

type t = {
  dir : string;
}

let build t ?base ~id ~log fn =
  ignore log;
  let dir = t.dir / id in
  match Os.check_dir dir with
  | `Present -> Lwt_result.return ()
  | `Missing ->
    begin match base with
      | None -> Os.ensure_dir dir; Lwt.return_unit
      | Some base ->
        Lwt_process.exec ("", [| "cp"; "-r"; t.dir / base; dir |]) >>= function
        | Unix.WEXITED 0 -> Lwt.return_unit
        | _ -> failwith "cp failed!"
    end >>= fun () ->
    fn dir

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
