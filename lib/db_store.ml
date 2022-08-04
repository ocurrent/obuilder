open Eio

let ( / ) = Filename.concat
let ( >>!= ) = Result.bind

module Make (Raw : S.STORE) = struct
  type build = {
    mutable users : int;
    set_cancelled : unit Promise.u;         (* Resolve this to cancel (when [users = 0]). *)
    log : (Build_log.t, exn) result Promise.t;
    result : (([`Loaded | `Saved] * S.id), [`Cancelled | `Msg of string]) result Promise.t;
  }

  module Builds = Map.Make(String)

  type t = {
    raw : Raw.t;
    dir : Eio.Dir.t;
    dao : Dao.t;
    (* Invariants for builds in [in_progress]:
       - [result] is still pending and [log] isn't finished.
       - [set_cancelled] is resolved iff [users = 0]. *)
    mutable in_progress : build Builds.t;
  }

  let finish_log ~set_log log =
    match Promise.peek log with
    | Some (Ok log) ->
      Build_log.finish log
    | Some _ -> ()
    | None ->
      Promise.resolve_error set_log (Failure "Build ended without setting a log!")

  let dec_ref build =
    build.users <- build.users - 1;
    if Promise.is_resolved build.result then (
      Log.info (fun f -> f "User cancelled job (users now = %d)" build.users);
      if build.users = 0 then (
        Promise.resolve build.set_cancelled ()
      )
    )

  (* Get the result for [id], either by loading it from the disk cache
     or by doing a new build using [fn]. We only run one instance of this
     at a time for a single [id]. *)
  let get_build t ~sw ~base ~id ~cancelled ~set_log fn =
    match Raw.result t.raw id with
    | Some dir ->
      let now = Unix.(gmtime (gettimeofday ())) in
      Dao.set_used t.dao ~id ~now;
      let log_file = dir / "log" in
      let log =
        if Sys.file_exists log_file then Build_log.of_saved t.dir log_file
        else Build_log.empty t.dir
      in
      Promise.resolve set_log (Ok log);
      Ok (`Loaded, id)
    | None ->
      match Raw.build t.raw ?base ~id (fun dir ->
          let log_file = dir / "log" in
          if Sys.file_exists log_file then Unix.unlink log_file;
          let log = Build_log.create ~sw ~dir:t.dir log_file in
          Promise.resolve set_log (Ok log);
          fn ~cancelled ~log dir
        )
          with 
      | Error _ as e -> e
      | Ok () ->
      let now = Unix.(gmtime (gettimeofday () )) in
      Dao.add t.dao ?parent:base ~id ~now;
      Ok (`Saved, id)

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
      assert (not (Promise.is_resolved existing.result));
      let _ = Promise.await existing.result in
      build ?switch t ?base ~id ~log:client_log fn
    | Some existing ->
      (* We're already building this, and the build hasn't been cancelled. *)
      existing.users <- existing.users + 1;
      let log = Promise.await_exn existing.log in
      (* Option.iter (fun sw -> Switch.on_release sw (fun () -> dec_ref existing)) switch; *)
      (* Lwt_switch.add_hook_or_exec switch (fun () -> dec_ref existing; Lwt.return_unit) >>= fun () -> *)
      Build_log.tail ?switch log (client_log `Output) >>!= fun () ->
      Promise.await existing.result >>!= fun (ty, r) ->
      log_ty client_log ~id ty;
      Ok r
    | None ->
      let result, set_result = Promise.create () in
      let log, set_log = Promise.create () in
      let tail_log, set_tl = Promise.create () in
      let cancelled, set_cancelled = Promise.create () in
      let build = { users = 1; set_cancelled; log; result } in
      (* Lwt_switch.add_hook_or_exec switch (fun () -> dec_ref build; Lwt.return_unit) >>= fun () -> *)
      t.in_progress <- Builds.add id build t.in_progress;
      Switch.run @@ fun sw ->
      Fiber.both
        (fun () -> 
          match Promise.await log with
          | Ok log ->
             let v = Build_log.tail ?switch log (client_log `Output) in
             Promise.resolve set_tl v
          | _ -> assert false
        )
        (fun () ->
           try
              let r = get_build t ~sw ~base ~id ~cancelled ~set_log fn in
              Fiber.yield ();
              t.in_progress <- Builds.remove id t.in_progress;
              Promise.resolve set_result r;
              finish_log ~set_log log
           with
              ex ->
                Log.info (fun f -> f "Build %S error: %a" id Fmt.exn ex);
                t.in_progress <- Builds.remove id t.in_progress;
                Promise.resolve_error set_result (`Msg (Printexc.to_string ex));
                finish_log ~set_log log
        );
      Promise.await tail_log >>!= fun () ->
      Promise.await result >>!= fun (ty, r) ->
      log_ty client_log ~id ty;
      Ok r

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
        Fiber.iter aux deps;
        log id;
        Raw.delete t.raw id;
        Dao.delete t.dao id
    in
    aux id

  let prune_batch ?(log=ignore) t ~before limit =
    let items = Dao.lru t.dao ~before limit in
    let n = List.length items in
    Log.info (fun f -> f "Pruning %d items (of %d requested)" n limit);
    items |> Fiber.iter (fun id ->
        log id;
        Raw.delete t.raw id;
        Dao.delete t.dao id
      );
    n

  let prune ?log t ~before limit =
    let rec aux acc limit =
      if limit = 0 then acc  (* Pruned everything we wanted to *)
      else (
        prune_batch ?log t ~before limit |> function
        | 0 -> acc           (* Nothing left to prune *)
        | n -> aux (acc + n) (limit - n)
      )
    in
    let n = aux 0 limit in
    Raw.complete_deletes t.raw;
    n

  let wrap dir raw =
    let db_dir = Raw.state_dir raw / "db" in
    Os.ensure_dir db_dir;
    let db = Db.of_dir (db_dir / "db.sqlite") in
    let dao = Dao.create db in
    { raw; dao; dir; in_progress = Builds.empty }
end
