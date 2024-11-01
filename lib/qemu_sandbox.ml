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

  let extra_mounts = List.map (fun { Config.Mount.src; _ } ->
    ["-drive"; "file=" ^ src / "rootfs" / "image.qcow2" ^ ",format=qcow2"]
  ) config.Config.mounts |> List.flatten in

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
              "-drive"; "file=" ^ result_tmp / "rootfs" / "image.qcow2" ^ ",format=qcow2" ]
              @ extra_mounts in
  let _, proc = Os.open_process ~stdin:qemu_stdin ~stdout:`Dev_null ~pp cmd in

  let ssh = ["ssh"; "opam@localhost"; "-p"; port; "-o"; "NoHostAuthenticationForLocalhost=yes"] in

  let rec loop = function
    | 0 -> Lwt_result.fail (`Msg "No connection")
    | n ->
      Os.exec_result ~pp (ssh @ ["exit"]) >>= function
      | Ok _ -> Lwt_result.ok (Lwt.return ())
      | _ -> Lwt_unix.sleep 1. >>= fun _ -> loop (n - 1) in
  Lwt_unix.sleep 5. >>= fun _ ->
  loop 30 >>= fun _ ->

  Lwt_list.iteri_s (fun i { Config.Mount.dst; _ } ->
    Os.exec (ssh @ ["cmd"; "/c"; "rmdir /s /q '" ^ dst ^ "'"]) >>= fun () ->
    let drive_letter = String.init 1 (fun _ -> Char.chr (Char.code 'd' + i)) in
    Os.exec (ssh @ ["cmd"; "/c"; "mklink /j '" ^ dst ^ "' '" ^ drive_letter ^ ":\\'"])) config.Config.mounts >>= fun () ->

  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let env = List.map (fun (k, v) -> k ^ "=" ^ v) config.Config.env |> Array.of_list in
  let sendenv = if Array.length env > 0 then List.map (fun (k, _) -> ["-o"; "SendEnv=" ^ k]) config.Config.env |> List.flatten else [] in
  let _, proc2 = Os.open_process ~env ?stdin ~stdout ~stderr ~pp (ssh @ sendenv @ ["cd"; config.Config.cwd; "&&"] @ config.Config.argv) in
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

let create (c : config) =
  let t = { qemu_cpus = c.cpus; qemu_memory = c.memory; qemu_network = c.network } in
  Lwt.return t

let finished () =
  Lwt.return ()

let shell = Some []

let tar = Some ["/cygdrive/c/Windows/System32/tar.exe"; "-xf"; "-"; "-C"; "/"]

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
