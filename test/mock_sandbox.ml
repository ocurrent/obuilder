type error = [`Exn of exn]

let pp_error f (`Exn ex) = Fmt.exn f ex

type t = {
  dir : string;
  expect : (?stdin:Obuilder.Os.unix_fd -> Obuilder.Config.t -> string -> unit Lwt.t) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ?stdin t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    Lwt.try_bind
      (fun () -> fn ?stdin config dir)
      Lwt_result.return
      (fun ex -> Lwt_result.fail (`Exn ex))

let create dir = { dir; expect = Queue.create () }
