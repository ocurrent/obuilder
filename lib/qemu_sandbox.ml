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

type guest_os =
  | Linux
  | OpenBSD
  | Windows
[@@deriving sexp]

type guest_arch =
  | Amd64
  | Riscv64
[@@deriving sexp]

type t = {
  qemu_cpus : int;
  qemu_memory : int;
  qemu_guest_os : guest_os;
  qemu_guest_arch : guest_arch;
  qemu_boot_time : int;
}

type config = {
  cpus : int;
  memory : int;
  guest_os : guest_os;
  guest_arch : guest_arch;
  boot_time : int;
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
    ["-drive"; "file=" ^ src / "rootfs" / "image.qcow2" ^ ",if=virtio"]
  ) config.Config.mounts |> List.flatten in

  Os.with_pipe_to_child @@ fun ~r:qemu_r ~w:qemu_w ->
  let qemu_stdin = `FD_move_safely qemu_r in
  let qemu_monitor = Lwt_io.(of_fd ~mode:output) qemu_w in
  let port = get_free_port () in
  let qemu_binary = match t.qemu_guest_arch with
    | Amd64 -> [ "qemu-system-x86_64"; "-machine"; "accel=kvm,type=pc"; "-cpu"; "host"; "-display"; "none";
                 "-device"; "virtio-net,netdev=net0" ]
    | Riscv64 -> [ "qemu-system-riscv64"; "-machine"; "type=virt"; "-nographic";
                   "-bios"; "/usr/lib/riscv64-linux-gnu/opensbi/generic/fw_jump.bin";
                   "-kernel"; "/usr/lib/u-boot/qemu-riscv64_smode/uboot.elf";
                   "-device"; "virtio-net-device,netdev=net0";
                   "-serial"; "none"] in
  let network = match config.network with
    | [ "host" ] -> ""
    | _ -> "restrict=yes," in
  let cmd = qemu_binary @ [
              "-monitor"; "stdio";
              "-m"; (string_of_int t.qemu_memory) ^ "G";
              "-smp"; string_of_int t.qemu_cpus;
              "-netdev"; "user,id=net0," ^ network ^ "hostfwd=tcp::" ^ port ^ "-:22";
              "-drive"; "file=" ^ result_tmp / "rootfs" / "image.qcow2" ^ ",if=virtio" ]
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
  loop t.qemu_boot_time >>= fun _ ->

  Lwt_list.iteri_s (fun i { Config.Mount.dst; _ } ->
    match t.qemu_guest_os with
    | Linux ->
      let dev = Printf.sprintf "/dev/vd%c1" (Char.chr (Char.code 'b' + i)) in
      Os.exec (ssh @ ["sudo"; "mount"; dev; dst])
    | OpenBSD ->
      let dev = Printf.sprintf "/dev/sd%ca" (Char.chr (Char.code '1' + i)) in
      Os.exec (ssh @ ["doas"; "fsck"; "-y"; dev]) >>= fun () ->
      Os.exec (ssh @ ["doas"; "mount"; dev; dst])
    | Windows ->
      Os.exec (ssh @ ["cmd"; "/c"; "rmdir /s /q '" ^ dst ^ "'"]) >>= fun () ->
      let drive_letter = String.init 1 (fun _ -> Char.chr (Char.code 'd' + i)) in
      Os.exec (ssh @ ["cmd"; "/c"; "mklink /j '" ^ dst ^ "' '" ^ drive_letter ^ ":\\'"])
  ) config.Config.mounts >>= fun () ->

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

  (match t.qemu_guest_arch with
    | Amd64 ->
      Log.info (fun f -> f "Sending QEMU an ACPI shutdown event");
      Lwt_io.write qemu_monitor "system_powerdown\n"
    | Riscv64 ->
      (* QEMU RISCV does not support ACPI until >= v9 *)
      Log.info (fun f -> f "Shutting down the VM");
      Os.exec (ssh @ ["sudo"; "poweroff"])) >>= fun () ->
  let rec loop = function
  | 0 ->
    Log.warn (fun f -> f "Powering off QEMU");
    Lwt_io.write qemu_monitor "quit\n"
  | n ->
    if Lwt.is_sleeping proc then 
      Lwt_unix.sleep 1. >>= fun () ->
      loop (n - 1)
    else Lwt.return () in
  loop t.qemu_boot_time >>= fun _ ->
  
  Os.process_result ~pp proc >>= fun _ ->

  if Lwt.is_sleeping cancelled then Lwt.return (res :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let create (c : config) =
  let t = { qemu_cpus = c.cpus; qemu_memory = c.memory; qemu_guest_os = c.guest_os; qemu_guest_arch = c.guest_arch; qemu_boot_time = c.boot_time } in
  Lwt.return t

let finished () =
  Lwt.return ()

let shell _ = Some []

let tar t =
  match t.qemu_guest_os with
  | Linux -> None
  | OpenBSD -> Some ["gtar"; "-xf"; "-"]
  | Windows -> Some ["/cygdrive/c/Windows/System32/tar.exe"; "-xf"; "-"; "-C"; "/"]

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

let guest_os =
  let options =
    [("linux", Linux);
     ("openbsd", OpenBSD);
     ("windows", Windows)] in
  Arg.value @@
  Arg.opt Arg.(enum options) Linux @@
  Arg.info ~docs
    ~doc:(Printf.sprintf "Set OS used by QEMU guest. $(docv) must be %s." (Arg.doc_alts_enum options))
    ~docv:"GUEST_OS"
    ["qemu-guest-os"]

let guest_arch =
  let options =
    [("amd64", Amd64);
     ("riscv64", Riscv64)] in
  Arg.value @@
  Arg.opt Arg.(enum options) Amd64 @@
  Arg.info ~docs
    ~doc:(Printf.sprintf "Set system architecture used by QEMU guest. $(docv) must be %s." (Arg.doc_alts_enum options))
    ~docv:"GUEST_OS"
    ["qemu-guest-arch"]

let boot_time =
  Arg.value @@
  Arg.opt Arg.int 30 @@
  Arg.info ~docs
    ~doc:"The maximum time in seconds to wait for the machine to boot/power off."
    ~docv:"BOOT_TIME"
    ["qemu-boot-time"]

let cmdliner : config Term.t =
  let make cpus memory guest_os guest_arch boot_time =
    { cpus; memory; guest_os; guest_arch; boot_time }
  in
  Term.(const make $ cpus $ memory $ guest_os $ guest_arch $ boot_time)
