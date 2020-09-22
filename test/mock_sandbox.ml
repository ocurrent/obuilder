type t = {
  dir : string;
  expect : (?stdin:Obuilder.Os.unix_fd -> log:Obuilder.Build_log.t -> Obuilder.Config.t -> string -> unit Lwt.t) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    Lwt.try_bind
      (fun () -> fn ?stdin ~log config dir)
      Lwt_result.return
      (function
        | Failure ex -> Lwt_result.fail (`Msg ex)
        | ex -> Lwt_result.fail (`Msg (Printexc.to_string ex))
      )

let create dir = { dir; expect = Queue.create () }
