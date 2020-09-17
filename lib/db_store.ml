open Lwt.Infix

let ( / ) = Filename.concat

module Make (Raw : S.STORE) = struct
  type t = {
    raw : Raw.t;
    dao : Dao.t;
  }

  type id = Raw.id

  let build t ?base ~id ~log fn =
    let build_needed = ref false in
    Raw.build t.raw ?base ~id ~log (fun dir ->
        build_needed := true;
        fn dir
      )
    >|= function
    | Error _ as e -> e
    | Ok _ as r ->
      let now = Unix.(gmtime (gettimeofday () )) in
      if !build_needed then
        Dao.add t.dao ?parent:(base :> string option) ~id ~now
      else
        Dao.set_used t.dao ~id ~now;
      r

  let wrap raw =
    let db_dir = Raw.state_dir raw / "db" in
    Os.ensure_dir db_dir;
    let db = Db.of_dir (db_dir / "db.sqlite") in
    let dao = Dao.create db in
    { raw; dao }

  let state_dir t = Raw.state_dir t.raw
end
