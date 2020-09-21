let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

module Make (Raw : S.STORE) = struct
  type t = {
    raw : Raw.t;
    dao : Dao.t;
  }

  let build t ?base ~id ~log fn =
    match Raw.result t.raw id with
    | Some dir ->
      let now = Unix.(gmtime (gettimeofday ())) in
      Dao.set_used t.dao ~id ~now;
      let log_file = dir / "log" in
      if Sys.file_exists log_file then Os.cat_file log_file ~dst:log;
      Lwt_result.return ()
    | None ->
      Raw.build t.raw ?base ~id (fun dir ->
          let log_file = dir / "log" in
          if Sys.file_exists log_file then Unix.unlink log_file;
          Build_log.with_log log_file (fun log -> fn ~log dir)
        )
      >>!= fun () ->
      let now = Unix.(gmtime (gettimeofday () )) in
      Dao.add t.dao ?parent:(base :> string option) ~id ~now;
      Lwt_result.return ()

  let wrap raw =
    let db_dir = Raw.state_dir raw / "db" in
    Os.ensure_dir db_dir;
    let db = Db.of_dir (db_dir / "db.sqlite") in
    let dao = Dao.create db in
    { raw; dao }
end
