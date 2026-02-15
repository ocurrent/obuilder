let ( / ) = Filename.concat

module Make (Raw : S.STORE) = struct
  type build = {
    mutable users : int;
    set_cancelled : unit Eio.Promise.u;         (* Resolve this to cancel (when [users = 0]). *)
    log : Build_log.t Eio.Promise.t;
    result : (([`Loaded | `Saved] * S.id), [`Cancelled | `Msg of string]) result Eio.Promise.t;
    base : string option;
  }

  module Builds = Map.Make(String)

  type t = {
    raw : Raw.t;
    dao : Dao.t;
    sw : Eio.Switch.t;
    (* Invariants for builds in [in_progress]:
       - [result] is still pending and [log] isn't finished.
       - [set_cancelled] is resolved iff [users = 0]. *)
    mutable in_progress : build Builds.t;
    mutable cache_hit : int;
    mutable cache_miss : int;
  }

  let finish_log ~set_log log =
    match Eio.Promise.peek log with
    | Some log ->
      Build_log.finish log
    | None ->
      (* Build ended without setting a log. Provide an empty log so
         anyone awaiting the log promise can continue. *)
      Eio.Promise.resolve set_log Build_log.empty

  let dec_ref build =
    build.users <- build.users - 1;
    if not (Eio.Promise.is_resolved build.result) then (
      Log.info (fun f -> f "User cancelled job (users now = %d)" build.users);
      if build.users = 0 then (
        Eio.Promise.resolve build.set_cancelled ()
      )
    )

  (* Get the result for [id], either by loading it from the disk cache
     or by doing a new build using [fn]. We only run one instance of this
     at a time for a single [id]. *)
  let get_build t ~base ~id ~cancelled ~set_log fn =
    match Raw.result t.raw id with
    | Some _ ->
      t.cache_hit <- t.cache_hit + 1;
      let now = Unix.(gmtime (gettimeofday ())) in
      Dao.set_used t.dao ~id ~now;
      let log_file = Raw.log_file t.raw id in
      let log =
        if Sys.file_exists log_file then Build_log.of_saved log_file
        else Build_log.empty
      in
      Eio.Promise.resolve set_log log;
      Ok (`Loaded, id)
    | None ->
      t.cache_miss <- t.cache_miss + 1;
      Raw.build t.raw ?base ~id (fun dir ->
          let log_file = Raw.log_file t.raw id in
          if Sys.file_exists log_file then Unix.unlink log_file;
          let log = Build_log.create log_file in
          Eio.Promise.resolve set_log log;
          fn ~cancelled ~log dir
        )
      |> (function
          | Error _ as e -> e
          | Ok () ->
            let now = Unix.(gmtime (gettimeofday () )) in
            Dao.add t.dao ?parent:base ~id ~now;
            Ok (`Saved, id))

  let log_ty client_log ~id = function
    | `Loaded -> client_log `Note (Fmt.str "---> using %S from cache" id)
    | `Saved -> client_log `Note (Fmt.str "---> saved as %S" id)

  (* Check to see if we're in the process of building [id].
     If so, just tail the log from that.
     If not, use [get_build] to get the build.
     [get_build] should set the log being used as soon as it knows it
     (this can't happen until we've created the temporary directory
     in the underlying store). *)
  let rec build ?cancelled t ?base ~id ~log:client_log fn =
    match Builds.find_opt id t.in_progress with
    | Some existing when existing.users = 0 ->
      client_log `Note ("Waiting for previous build to finish cancelling");
      assert (not (Eio.Promise.is_resolved existing.result));
      let _ = Eio.Promise.await existing.result in
      build ?cancelled t ?base ~id ~log:client_log fn
    | Some existing ->
      (* We're already building this, and the build hasn't been cancelled. *)
      existing.users <- existing.users + 1;
      let log = Eio.Promise.await existing.log in
      (* Hook: when cancelled is resolved, dec_ref.
         Also exit when the build result is resolved so we don't block the switch. *)
      (match cancelled with
       | Some p ->
         Eio.Fiber.fork ~sw:t.sw (fun () ->
           Eio.Fiber.first
             (fun () ->
               Eio.Promise.await p;
               dec_ref existing)
             (fun () ->
               ignore (Eio.Promise.await existing.result)))
       | None -> ());
      let await_result () =
        match Eio.Promise.await existing.result with
        | Error _ as e -> e
        | Ok (ty, r) ->
          log_ty client_log ~id ty;
          Ok r
      in
      (match Build_log.tail ?cancelled log (client_log `Output) with
       | Error `Cancelled ->
         (* Build may have completed before we noticed the cancellation.
            If a result is available, prefer it over the cancellation. *)
         (match Eio.Promise.peek existing.result with
          | Some (Ok (ty, r)) ->
            log_ty client_log ~id ty;
            Ok r
          | Some (Error _ as e) -> e
          | None -> Error `Cancelled)
       | Error _ as e -> e
       | Ok () -> await_result ())
    | None ->
      let result, set_result = Eio.Promise.create () in
      let log, set_log = Eio.Promise.create () in
      let cancelled_p, set_cancelled = Eio.Promise.create () in
      let build = { users = 1; set_cancelled; log; result; base } in
      (* Hook: when cancelled is resolved, dec_ref.
         Also exit when the build result is resolved so we don't block the switch. *)
      (match cancelled with
       | Some p ->
         Eio.Fiber.fork ~sw:t.sw (fun () ->
           Eio.Fiber.first
             (fun () ->
               Eio.Promise.await p;
               dec_ref build)
             (fun () ->
               ignore (Eio.Promise.await build.result)))
       | None -> ());
      t.in_progress <- Builds.add id build t.in_progress;
      Eio.Fiber.fork ~sw:t.sw
        (fun () ->
           match get_build t ~base ~id ~cancelled:cancelled_p ~set_log fn with
           | r ->
              t.in_progress <- Builds.remove id t.in_progress;
              Eio.Promise.resolve set_result r;
              finish_log ~set_log log
           | exception ex ->
              Log.info (fun f -> f "Build %S error: %a" id Fmt.exn ex);
              t.in_progress <- Builds.remove id t.in_progress;
              Eio.Promise.resolve set_result (Error (`Msg (Fmt.str "Build error: %a" Fmt.exn ex)));
              finish_log ~set_log log
        );
      (* Tail the log *)
      let log_v = Eio.Promise.await log in
      let await_result () =
        match Eio.Promise.await result with
        | Error _ as e -> e
        | Ok (ty, r) ->
          log_ty client_log ~id ty;
          Ok r
      in
      (match Build_log.tail ?cancelled log_v (client_log `Output) with
       | Error `Cancelled ->
         (* Build may have completed before we noticed the cancellation.
            If a result is available, prefer it over the cancellation. *)
         (match Eio.Promise.peek result with
          | Some (Ok (ty, r)) ->
            log_ty client_log ~id ty;
            Ok r
          | Some (Error _ as e) -> e
          | None -> Error `Cancelled)
       | Error _ as e -> e
       | Ok () -> await_result ())

  let result t id = Raw.result t.raw id
  let count t = Dao.count t.dao
  let df t = Raw.df t.raw
  let root t = Raw.root t.raw
  let cache_stats t = t.cache_hit, t.cache_miss
  let cache ~user t = Raw.cache ~user t.raw

  let delete ?(log=ignore) t id =
    let rec aux id =
      match Dao.children t.dao id with
      | Error `No_such_id ->
        log id;
        Log.warn (fun f -> f "ID %S not in database!" id);
        Raw.delete t.raw id     (* Try removing it anyway *)
      | Ok deps ->
        List.iter aux deps;
        log id;
        Raw.delete t.raw id;
        Dao.delete t.dao id
    in
    aux id

  let prune_lru ?(log=ignore) t ~before limit =
    let items = Dao.lru t.dao ~before limit in
    let items = List.filter (fun id ->
      Builds.filter (fun _ b -> match b.base with
      | Some base -> base = id
      | None -> false) t.in_progress |> Builds.is_empty) items in
    match items with
    | [] -> 0
    | id :: _ ->
      log id;
      Raw.delete t.raw id;
      Dao.delete t.dao id;
      1

  let prune ?log t ~before limit =
    Log.info (fun f -> f "Pruning %d items" limit);
    let rec aux count =
      if count >= limit then count  (* Pruned everything we wanted to *)
      else (
        match prune_lru ?log t ~before limit with
        | 0 -> count           (* Nothing left to prune *)
        | n -> aux (count + n)
      )
    in
    let n = aux 0 in
    Raw.complete_deletes t.raw;
    Log.info (fun f -> f "Pruned %d items" n);
    n

  let wrap ~sw raw =
    let db_dir = Raw.state_dir raw / "db" in
    Os.ensure_dir db_dir;
    let db = Db.of_dir (db_dir / "db.sqlite") in
    let dao = Dao.create db in
    { raw; dao; sw; in_progress = Builds.empty; cache_hit = 0; cache_miss = 0 }

  let unwrap t =
    Dao.close t.dao
end
