open Lwt.Infix
open Sexplib.Conv

let ( / ) = Filename.concat

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

type t = {
  qemu_cpus : int;
  qemu_memory : int;
  qemu_network : string;   (* Default network, overridden by network stanza *)
}

type config = {
  cpus : int;
  memory : int;
  network : string;
} [@@deriving sexp]

let get_free_port () =
  let fd = Unix.socket PF_INET SOCK_STREAM 0 in
  let () = Unix.bind fd (ADDR_INET(Unix.inet_addr_loopback, 0)) in
  let sa = Unix.getsockname fd in
  let () = Unix.close fd in
  match sa with
  | ADDR_INET (_, n) -> string_of_int n
  | ADDR_UNIX _ -> assert false;;

let run ~cancelled ?stdin ~log t config result_tmp =
  let pp f = Os.pp_cmd f ("", config.Config.argv) in

  Os.with_pipe_to_child @@ fun ~r:qemu_r ~w:qemu_w ->
  let qemu_stdin = `FD_move_safely qemu_r in
  let qemu_monitor = Lwt_io.(of_fd ~mode:output) qemu_w in
  let port = get_free_port () in
  let cmd = [ "qemu-system-x86_64";
              "-m"; (string_of_int t.qemu_memory) ^ "G";
              "-smp"; string_of_int t.qemu_cpus;
              "-machine"; "accel=kvm,type=q35";
              "-cpu"; "host";
              "-nic"; "user,hostfwd=tcp::" ^ port ^ "-:22";
              "-display"; "none";
              "-monitor"; "stdio";
              "-drive"; "file=" ^ result_tmp / "rootfs" / "image.qcow2" ^ ",format=qcow2" ] in
  let _, proc = Os.open_process ~stdin:qemu_stdin ~stdout:`Dev_null ~pp cmd in

  let rec loop = function
    | 0 -> Lwt_result.fail (`Msg "No connection")
    | n ->
      Os.exec_result ~pp ["ssh"; "opam@localhost"; "-p"; port; "-o"; "BatchMode=yes"; "-o"; "NoHostAuthenticationForLocalhost=yes"; "exit"] >>= function
      | Ok _ -> Lwt_result.ok (Lwt.return ())
      | _ -> Lwt_unix.sleep 2. >>= fun _ -> loop (n - 1) in
  Lwt_unix.sleep 2. >>= fun _ ->
  loop 30 >>= fun _ ->

  Lwt_list.iter_s (fun { Config.Mount.src; dst; _ } ->
    let folders = Sys.readdir src |> Array.to_list |> List.map (fun f -> src / f) in
    if List.length folders > 0 then
      Os.exec (["scp"; "-P"; port; "-o"; "NoHostAuthenticationForLocalhost=yes"; "-prq"] @ folders @ ["opam@localhost:" ^ dst ])
    else Lwt.return ()) config.Config.mounts >>= fun () ->

  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let env = List.map (fun (k, v) -> k ^ "=" ^ v) config.Config.env |> Array.of_list in
  let sendenv = if Array.length env > 0 then List.map (fun (k, _) -> ["-o"; "SendEnv=" ^ k]) config.Config.env |> List.flatten else [] in
  let cmd = match config.Config.argv with
  | "cmd" :: "/S" :: "/C" :: tl
  | "/usr/bin/env" :: "bash" :: "-c" :: tl -> tl
  | "/bin/sh" :: "-c" :: tl -> tl
  | x -> x in
  let _, proc2 = Os.open_process ~env ?stdin ~stdout ~stderr ~pp (["ssh"; "opam@localhost"; "-p"; port; "-o"; "NoHostAuthenticationForLocalhost=yes"] @ sendenv @ ["cd"; config.Config.cwd; "&&"] @ cmd) in
  Lwt.on_termination cancelled (fun () ->
    let aux () =
      if Lwt.is_sleeping proc then
        Lwt_io.write qemu_monitor "quit\n"
      else Lwt.return_unit (* Process has already finished *)
    in
    Lwt.async aux
  );
  Os.process_result ~pp proc2 >>= fun res ->
  copy_log >>= fun () ->

  Lwt_list.iter_s (fun { Config.Mount.src; dst; _ } ->
    Os.exec ["scp"; "-P"; port; "-o"; "NoHostAuthenticationForLocalhost=yes"; "-prq"; "opam@localhost:" ^ dst ^ "/*"; src ]) config.Config.mounts >>= fun () ->

  Log.info (fun f -> f "Sending QEMU an ACPI shutdown event");
  Lwt_io.write qemu_monitor "system_powerdown\n" >>= fun () ->
  let rec loop = function
  | 0 ->
    Log.warn (fun f -> f "Powering off QEMU");
    Lwt_io.write qemu_monitor "quit\n"
  | n ->
    if Lwt.is_sleeping proc then 
      Lwt_unix.sleep 1. >>= fun () ->
      loop (n - 1)
    else Lwt.return () in
  loop 30 >>= fun _ ->
  
  Os.process_result ~pp proc >>= fun _ ->

  if Lwt.is_sleeping cancelled then Lwt.return (res :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let tar_in ~cancelled ?stdin ~log:_ _ config result_tmp =
  let proc =
    let cmd = ["guestfish";
               "add-drive"; result_tmp / "rootfs" / "image.qcow2"; ":";
               "run"; ":";
               "mount"; "/dev/sda2"; "/"; ":";
               "tar-in"; "-"; config.Config.cwd; ] in
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f ("", config.Config.argv) in
    Os.sudo_result ?stdin ~pp cmd in
  proc >>= fun r ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let create (c : config) =
  let t = { qemu_cpus = c.cpus; qemu_memory = c.memory; qemu_network = c.network } in
  Lwt.return t

let finished () =
  Lwt.return ()

open Cmdliner

let docs = "QEMU BACKEND"

let cpus =
  Arg.value @@
  Arg.opt Arg.int 2 @@
  Arg.info ~docs
    ~doc:"Number of CPUs to be used by each QEMU machine."
    ~docv:"CPUS"
    ["qemu-cpus"]

let memory =
  Arg.value @@
  Arg.opt Arg.int 2 @@
  Arg.info ~docs
    ~doc:"The amount of memory allocated to the VM in gigabytes."
    ~docv:"MEMORY"
    ["qemu-memory"]

let network =
  Arg.value @@
  Arg.opt Arg.string (if Sys.unix then "host" else "nat") @@
  Arg.info ~docs
    ~doc:"Docker network used for the Docker backend setup."
    ~docv:"NETWORK"
    ["qemu-network"]

let cmdliner : config Term.t =
  let make cpus memory network =
    { cpus; memory; network; }
  in
  Term.(const make $ cpus $ memory $ network)
