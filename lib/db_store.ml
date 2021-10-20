open Lwt.Infix

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

module Make (Raw : S.STORE) = struct
  type build = {
    mutable users : int;
    cancelled : unit Lwt.t;
    set_cancelled : unit Lwt.u;         (* Resolve this to cancel (when [users = 0]). *)
    log : Build_log.t Lwt.t;
    result : (([`Loaded | `Saved] * S.id), [`Cancelled | `Msg of string]) Lwt_result.t;
  }

  module Builds = Map.Make(String)

  type t = {
    raw : Raw.t;
    dao : Dao.t;
    (* Invariants for builds in [in_progress]:
       - [result] is still pending and [log] isn't finished.
       - [cancelled] is resolved iff [users = 0]. *)
    mutable in_progress : build Builds.t;
  }

  let finish_log ~set_log log =
    match Lwt.state log with
    | Lwt.Return log ->
      Build_log.finish log
    | Lwt.Fail _ ->
      Lwt.return_unit
    | Lwt.Sleep ->
      Lwt.wakeup_exn set_log (Failure "Build ended without setting a log!");
      Lwt.return_unit

  let dec_ref build =
    build.users <- build.users - 1;
    if Lwt.is_sleeping build.result then (
      Log.info (fun f -> f "User cancelled job (users now = %d)" build.users);
      if build.users = 0 then (
        Lwt.wakeup_later build.set_cancelled ()
      )
    )

  (* Get the result for [id], either by loading it from the disk cache
     or by doing a new build using [fn]. We only run one instance of this
     at a time for a single [id]. *)
  let get_build t ~base ~id ~cancelled ~set_log fn =
    match Raw.result t.raw id with
    | Some dir ->
      let now = Unix.(gmtime (gettimeofday ())) in
      Dao.set_used t.dao ~id ~now;
      let log_file = dir / "log" in
      begin
        if Sys.file_exists log_file then Build_log.of_saved log_file
        else Lwt.return Build_log.empty
      end >>= fun log ->
      Lwt.wakeup set_log log;
      Lwt_result.return (`Loaded, id)
    | None ->
      Raw.build t.raw ?base ~id (fun dir ->
          let log_file = dir / "log" in
          if Sys.file_exists log_file then Unix.unlink log_file;
          Build_log.create log_file >>= fun log ->
          Lwt.wakeup set_log log;
          fn ~cancelled ~log dir
        )
      >>!= fun () ->
      let now = Unix.(gmtime (gettimeofday () )) in
      Dao.add t.dao ?parent:base ~id ~now;
      Lwt_result.return (`Saved, id)

  let log_ty client_log ~id = function
    | `Loaded -> client_log `Note (Fmt.str "---> using %S from cache" id)
    | `Saved -> client_log `Note (Fmt.str "---> saved as %S" id)

  (* Check to see if we're in the process of building [id].
     If so, just tail the log from that.
     If not, use [get_build] to get the build.
     [get_build] should set the log being used as soon as it knows it
     (this can't happen until we've created the temporary directory
     in the underlying store). *)
  let rec build ?switch t ?base ~id ~log:client_log fn =
    match Builds.find_opt id t.in_progress with
    | Some existing when existing.users = 0 ->
      client_log `Note ("Waiting for previous build to finish cancelling");
      assert (Lwt.is_sleeping existing.result);
      existing.result >>= fun _ ->
      build ?switch t ?base ~id ~log:client_log fn
    | Some existing ->
      (* We're already building this, and the build hasn't been cancelled. *)
      existing.users <- existing.users + 1;
      existing.log >>= fun log ->
      Lwt_switch.add_hook_or_exec switch (fun () -> dec_ref existing; Lwt.return_unit) >>= fun () ->
      Build_log.tail ?switch log (client_log `Output) >>!= fun () ->
      existing.result >>!= fun (ty, r) ->
      log_ty client_log ~id ty;
      Lwt_result.return r
    | None ->
      let result, set_result = Lwt.wait () in
      let log, set_log = Lwt.wait () in
      let tail_log = log >>= fun log -> Build_log.tail ?switch log (client_log `Output) in
      let cancelled, set_cancelled = Lwt.wait () in
      let build = { users = 1; cancelled; set_cancelled; log; result } in
      Lwt_switch.add_hook_or_exec switch (fun () -> dec_ref build; Lwt.return_unit) >>= fun () ->
      t.in_progress <- Builds.add id build t.in_progress;
      Lwt.async
        (fun () ->
           Lwt.try_bind
             (fun () -> get_build t ~base ~id ~cancelled ~set_log fn)
             (fun r ->
                t.in_progress <- Builds.remove id t.in_progress;
                Lwt.wakeup_later set_result r;
                finish_log ~set_log log
             )
             (fun ex ->
                Log.info (fun f -> f "Build %S error: %a" id Fmt.exn ex);
                t.in_progress <- Builds.remove id t.in_progress;
                Lwt.wakeup_later_exn set_result ex;
                finish_log ~set_log log
             )
        );
      tail_log >>!= fun () ->
      result >>!= fun (ty, r) ->
      log_ty client_log ~id ty;
      Lwt_result.return r

  let result t id = Raw.result t.raw id
  let cache ~user t = Raw.cache ~user t.raw

  let delete ?(log=ignore) t id =
    let rec aux id =
      match Dao.children t.dao id with
      | Error `No_such_id ->
        log id;
        Log.warn (fun f -> f "ID %S not in database!" id);
        Raw.delete t.raw id     (* Try removing it anyway *)
      | Ok deps ->
        Lwt_list.iter_s aux deps >>= fun () ->
        log id;
        Raw.delete t.raw id >|= fun () ->
        Dao.delete t.dao id
    in
    aux id

  let prune_batch ?(log=ignore) t ~before limit =
    let items = Dao.lru t.dao ~before limit in
    let n = List.length items in
    Log.info (fun f -> f "Pruning %d items (of %d requested)" n limit);
    items |> Lwt_list.iter_s (fun id ->
        log id;
        Raw.delete t.raw id >|= fun () ->
        Dao.delete t.dao id
      )
    >>= fun () ->
    Lwt.return n

  let prune ?log t ~before limit =
    let rec aux acc limit =
      if limit = 0 then Lwt.return acc  (* Pruned everything we wanted to *)
      else (
        prune_batch ?log t ~before limit >>= function
        | 0 -> Lwt.return acc           (* Nothing left to prune *)
        | n -> aux (acc + n) (limit - n)
      )
    in
    aux 0 limit >>= fun n ->
    Raw.complete_deletes t.raw >>= fun () ->
    Lwt.return n

  let wrap raw =
    let db_dir = Raw.state_dir raw / "db" in
    Os.ensure_dir db_dir;
    let db = Db.of_dir (db_dir / "db.sqlite") in
    let dao = Dao.create db in
    { raw; dao; in_progress = Builds.empty }
end
