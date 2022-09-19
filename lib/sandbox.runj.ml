open Lwt.Infix
open Sexplib.Conv

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.Infix.( >>= )

type t = {
  runj_state_dir : string;
}

type config = unit [@@deriving sexp]

module Json_config = struct
  let mount ?(options=[]) ~ty ~src dst =
    `Assoc [
      "destination", `String dst;
      "type", `String ty;
      "source", `String src;
      "options", `List (List.map (fun x -> `String x) options);
    ]

  let strings xs = `List ( List.map (fun x -> `String x) xs)

  let make {Config.cwd; argv; hostname; user = _; env; mounts = _; network = _; mount_secrets = _} _t ~config_dir:_ ~results_dir : Yojson.Safe.t =
    (* TODO: runj does not support the "user" field yet *)
    (* TODO: FreeBSD does not support mounts of regular files / directories *)
    let argv =
      (* TODO: runj does not support the "cwd" field yet but we can hack around it *)
      ["/bin/sh";"-c";Printf.sprintf "cd %S && %s" cwd (String.concat " " argv)]
    in
    `Assoc [
      "ociVersion", `String "1.0.2-runj-dev";
      "process", `Assoc [
        "terminal", `Bool false;
        "args", strings argv;
        "env", strings (List.map (fun (k, v)  -> Printf.sprintf "%s=%s" k v) env);
      ];
      "root", `Assoc [
        "path", `String (results_dir / "rootfs");
      ];
      "hostname", `String hostname;
      "mounts", `List [
        mount "/dev"
         ~ty:"devfs"
         ~src:"devfs"
         ~options:[
           "ruleset=4"
         ];
      ];
      "freebsd", `Assoc [
        (* TODO: Add support for non-host network using the runj extension: https://github.com/samuelkarp/runj/pull/32 *)
        "network", `Assoc [
          "ipv4", `Assoc [
            "mode", `String "inherit";
          ];
        ];
      ];
    ]
end

let next_id = ref 0

let run ~cancelled ?stdin:stdin ~log t config results_dir =
  Lwt_io.with_temp_dir ~perm:0o700 ~prefix:"obuilder-runj-" @@ fun tmp ->
  let json_config = Json_config.make config ~config_dir:tmp ~results_dir t in
  Os.write_file ~path:(tmp / "config.json") (Yojson.Safe.pretty_to_string json_config ^ "\n") >>= fun () ->
  Os.write_file ~path:(results_dir / "rootfs" / "etc" / "hosts") "127.0.0.1 localhost builder" >>= fun () ->
  Os.write_file ~path:(results_dir / "rootfs" / "etc" / "resolv.conf") "nameserver 8.8.8.8" >>= fun () ->
  let id = string_of_int !next_id in
  incr next_id;
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let copy_log = Build_log.copy ~src:out_r ~dst:log in
  let proc =
    let cmd1 = ["runj"; "create"; "-b"; t.runj_state_dir; id] in
    let cmd2 = ["runj"; "start"; id] in
    let stdout = `FD_move_safely out_w in
    let stderr = stdout in
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.sudo_result ~cwd:tmp ?stdin ~stdout ~stderr ~pp cmd1 >>!= fun () ->
    Os.sudo_result ~cwd:tmp ?stdin ~stdout ~stderr ~pp cmd2
  in
  Lwt.on_termination cancelled (fun () ->
      let rec aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "runj kill %S" id in
          Os.sudo_result ~cwd:tmp ["runj"; "kill"; id; "KILL"] ~pp >>= function
          | Ok () -> Lwt.return_unit
          | Error (`Msg m) ->
            (* This might be because it hasn't been created yet, so retry. *)
            Log.warn (fun f -> f "kill failed: %s (will retry in 10s)" m);
            Lwt_unix.sleep 10.0 >>= aux
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  proc >>= fun r ->
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let clean_runj dir =
  Sys.readdir dir
  |> Array.to_list
  |> Lwt_list.iter_s (fun item ->
      Log.warn (fun f -> f "Removing left-over runj container %S" item);
      Os.sudo ["runj"; "delete"; item]
    )

let create ~state_dir (() : config) =
  Os.ensure_dir state_dir;
  clean_runj state_dir >|= fun () ->
  { runj_state_dir = state_dir }

module Term = Cmdliner.Term

let cmdliner : config Term.t =
  let make = () in
  Term.(const make)
