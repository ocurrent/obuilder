open Lwt.Infix

module Config = struct
  type t = {
    cwd : string;
    argv : string list;
    hostname : string;
    user : Obuilder.Spec.user;
    env : Obuilder.Os.env;
  }

  let v ~cwd ~argv ~hostname ~user ~env =
    { cwd; argv; hostname; user; env }
end

type t = {
  dir : string;
  expect : (?stdin:Obuilder.Os.unix_fd -> Config.t -> string -> unit Lwt.t) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ?stdin t (config:Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    fn ?stdin config dir >|= fun () ->
    Ok ()

let create dir = { dir; expect = Queue.create () }
