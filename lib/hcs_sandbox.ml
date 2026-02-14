open Lwt.Infix
open Sexplib.Conv

let finished () = Lwt.return_unit

let ( / ) = Filename.concat

type t = {
  ctr_path : string;
  hcn_namespace_path : string;
}

type config = {
  ctr_path : string;
  hcn_namespace_path : string;
} [@@deriving sexp]

let next_id = ref (int_of_float (Unix.gettimeofday () *. 100.) mod 1_000_000)



module Json_config = struct
  let strings xs = `List (List.map (fun x -> `String x) xs)

  let make {Config.cwd; argv; hostname; user; env; mounts; network; mount_secrets = _; entrypoint = _}
      ~layer_folders ~network_namespace : Yojson.Safe.t =
    let username =
      match user with
      | `Windows { Obuilder_spec.name } -> name
      | `Unix _ -> "ContainerAdministrator"
    in
    let windows_section =
      let base = [
        "layerFolders", `List (List.map (fun p -> `String p) layer_folders);
        "ignoreFlushesDuringBoot", `Bool true;
      ] in
      match network, network_namespace with
      | ["host"], Some ns ->
        base @ [
          "network", `Assoc [
            "allowUnqualifiedDNSQuery", `Bool true;
            "networkNamespace", `String ns;
          ];
        ]
      | _ -> base
    in
    let oci_mounts = List.map (fun { Config.Mount.src; dst; readonly; ty = _ } ->
      `Assoc [
        "destination", `String dst;
        "type", `String "bind";
        "source", `String src;
        "options", `List (
          (if readonly then [`String "ro"] else [`String "rw"]) @
          [`String "rbind"; `String "rprivate"]
        );
      ]
    ) mounts in
    `Assoc ([
      "ociVersion", `String "1.1.0";
      "process", `Assoc [
        "terminal", `Bool false;
        "user", `Assoc [
          "username", `String username;
        ];
        "args", strings argv;
        "env", strings (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env);
        "cwd", `String cwd;
      ];
      "root", `Assoc [
        "path", `String "";
        "readonly", `Bool false;
      ];
      "hostname", `String hostname;
      "windows", `Assoc windows_section;
    ] @
      (if oci_mounts <> [] then ["mounts", `List oci_mounts] else [])
    )
end

let run ~cancelled ?stdin:stdin ~log (t : t) config results_dir =
  let pp f = Os.pp_cmd f ("", config.Config.argv) in
  let container_id = Printf.sprintf "obuilder-run-%d" !next_id in
  incr next_id;
  let layer_folders =
    if Sys.file_exists (Hcs.layerinfo_path results_dir) then
      let li = Hcs.read_layerinfo results_dir in
      li.parent_layer_paths @ [li.source]
    else
      Fmt.failwith "No layerinfo found in %s" results_dir
  in
  (* Create HCN namespace for networking if requested *)
  let use_network = config.Config.network = ["host"] in
  (if use_network && Sys.win32 then begin
    Log.info (fun f -> f "hcs_sandbox: creating HCN namespace for networking");
    (Os.win32_pread [t.hcn_namespace_path; "create"] >>= function
     | Ok output -> Lwt.return output
     | Error (`Msg m) -> Fmt.failwith "Failed to create HCN namespace: %s" m) >>= fun output ->
    let ns = String.trim output in
    Log.info (fun f -> f "hcs_sandbox: created HCN namespace %s" ns);
    Lwt.return (Some ns)
  end else Lwt.return_none) >>= fun network_namespace ->
  Lwt.finalize (fun () ->
  Lwt_io.with_temp_dir ~perm:0o700 ~prefix:"obuilder-hcs-" @@ fun tmp ->
  (* Generate OCI config.json *)
  let json_config = Json_config.make config ~layer_folders ~network_namespace in
  let json_str = Yojson.Safe.pretty_to_string json_config ^ "\n" in
  Log.info (fun f -> f "hcs_sandbox: OCI config.json:\n%s" json_str);
  Os.write_file ~path:(tmp / "config.json") json_str >>= fun () ->
  (* Write secrets *)
  Lwt_list.iteri_s
    (fun id Config.Secret.{value; _} ->
      let secret_dir = tmp / "secrets" / string_of_int id in
      Os.ensure_dir (tmp / "secrets");
      Os.ensure_dir secret_dir;
      Os.write_file ~path:(secret_dir / "secret") value
    ) config.mount_secrets
  >>= fun () ->
  (* Build the ctr run command *)
  let cmd = [t.ctr_path; "run"; "--rm"] @
            (if Option.is_some network_namespace then ["--cni"] else []) @
            ["--config"; tmp / "config.json";
             container_id] in
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = Build_log.copy ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    Os.exec_result ?stdin ~stdout ~stderr ~pp cmd
  in
  Lwt.on_termination cancelled (fun () ->
      let aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "ctr task kill %S" container_id in
          Os.exec_result [t.ctr_path; "task"; "kill"; "-s"; "SIGKILL"; container_id] ~pp >>= fun _ ->
          Lwt.return_unit
        ) else Lwt.return_unit
      in
      Lwt.async aux
    );
  proc >>= fun r ->
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled
  ) (fun () ->
    (* Clean up HCN namespace if we created one *)
    match network_namespace with
    | Some ns ->
      Log.info (fun f -> f "hcs_sandbox: deleting HCN namespace %s" ns);
      Os.win32_pread [t.hcn_namespace_path; "delete"; ns] >>= fun result ->
      (match result with
       | Ok _ -> ()
       | Error (`Msg m) -> Log.warn (fun f -> f "hcs_sandbox: failed to delete HCN namespace %s: %s" ns m));
      Lwt.return_unit
    | None -> Lwt.return_unit
  )

let create ~state_dir:_ (c : config) : t Lwt.t =
  Lwt.return ({ ctr_path = c.ctr_path; hcn_namespace_path = c.hcn_namespace_path } : t)

let shell _ = [{|C:\cygwin64\bin\bash.exe|}; "-lc"]

let tar _ = ["tar"; "-xf"; "-"]

open Cmdliner

let docs = "HCS SANDBOX"

let ctr_path =
  Arg.value @@
  Arg.opt Arg.string "ctr" @@
  Arg.info ~docs
    ~doc:"Path to the ctr (containerd) CLI."
    ~docv:"CTR_PATH"
    ["hcs-ctr-path"]

let hcn_namespace_path =
  Arg.value @@
  Arg.opt Arg.string "hcn-namespace" @@
  Arg.info ~docs
    ~doc:"Path to the hcn-namespace tool for Windows container networking."
    ~docv:"HCN_NAMESPACE_PATH"
    ["hcs-hcn-namespace-path"]

let cmdliner : config Term.t =
  let make ctr_path hcn_namespace_path =
    { ctr_path; hcn_namespace_path }
  in
  Term.(const make $ ctr_path $ hcn_namespace_path)
