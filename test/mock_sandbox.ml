open Sexplib.Conv
open Lwt.Infix
open Obuilder

type t = {
  dir : string;
  expect :
    (cancelled:unit Lwt.t ->
     ?stdin:Obuilder.Os.unix_fd ->
     log:Obuilder.Build_log.t ->
     Obuilder.Config.t ->
     string ->
     (unit, [`Msg of string | `Cancelled]) Lwt_result.t) Queue.t;
}

type config = {
  dir : string;
}[@@deriving sexp]

let pp ppf = 
  let fields = [
    Fmt.field ~label:Fmt.string "dir" (fun (t : t) -> t.dir) Fmt.string;
  ] in 
  let r = Fmt.(braces @@ record fields) in 
  Fmt.(pf ppf "mock = %a" r)

module Saved_context = struct
  type t = {
    env : Config.env;
  } [@@deriving sexp]
end

open Cmdliner
let dir =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Directory"
    ~docv:"DIR"
    ["dir"]

let cmdliner : config Term.t = 
  let make dir = 
    { dir }
  in
  Term.(const make $ dir)

let expect t x = Queue.add x t.expect

let export_env base =
  Os.pread ["docker"; "image"; "inspect";
            "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
            "--"; base] >|= fun env ->
  String.split_on_char '\x00' env
  |> List.filter_map (function
      | "\n" -> None
      | kv ->
        match Astring.String.cut ~sep:"=" kv with
        | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
        | Some _ as pair -> pair
    )

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

let with_container ~log base fn =
  Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = copy_to_log ~src:r ~dst:log in
      Os.pread ~stderr:(`FD_move_safely w) ["docker"; "create"; "--"; base] >>= fun cid ->
      copy >|= fun () ->
      String.trim cid
    ) >>= fun cid ->
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> Os.exec ~stdout:`Dev_null ["docker"; "rm"; "--"; cid])

let from ~log ~from _t =
  let ( / ) = Filename.concat in 
  let base = from in 
  log `Heading (Fmt.strf "(from %a)" Sexplib.Sexp.pp_hum (Atom base));
  (fun ~cancelled:_ ~log tmp ->
      Logs.info (fun f -> f "Base image not present; importing %S...@." base);
      let rootfs = tmp / "rootfs" in
      Os.sudo ["mkdir"; "--mode=755"; "--"; rootfs] >>= fun () ->
      (* Lwt_process.exec ("", [| "docker"; "pull"; "--"; base |]) >>= fun _ -> *)
      with_container ~log base (fun cid ->
          Os.with_pipe_between_children @@ fun ~r ~w ->
          let exporter = Os.exec ~stdout:(`FD_move_safely w) ["docker"; "export"; "--"; cid] in
          let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
          exporter >>= fun () ->
          tar
        ) >>= fun () ->
      export_env base >>= fun env ->
      Os.write_file ~path:(tmp / "env")
        (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
      Lwt_result.return ()
    )


let run ~cancelled ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    Lwt.catch
      (fun () -> fn ~cancelled ?stdin ~log config dir)
      (function
        | Failure ex -> Lwt_result.fail (`Msg ex)
        | ex -> Lwt_result.fail (`Msg (Printexc.to_string ex))
      )

let create ?state_dir:_ conf = Lwt.return { dir = conf.dir; expect = Queue.create () }

let mock_create conf = { dir = conf.dir; expect = Queue.create () }
