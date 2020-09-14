open Lwt.Infix

let ( / ) = Filename.concat

type t = {
  runc_state_dir : string;
}

module Config = struct
  type t = Yojson.Safe.t

  let mount ?(options=[]) ~ty ~src dst =
    `Assoc [
      "destination", `String dst;
      "type", `String ty;
      "source", `String src;
      "options", `List (List.map (fun x -> `String x) options);
    ]

  let strings xs = `List ( List.map (fun x -> `String x) xs)

  let v ~cwd ~argv ~hostname ~user ~env : t =
    let user =
      let { Spec.uid; gid } = user in
      `Assoc [
        "uid", `Int uid;
        "gid", `Int gid;
      ]
    in
    `Assoc [
      "ociVersion", `String "1.0.1-dev";
      "process", `Assoc [
        "terminal", `Bool false;
        "user", user;
        "args", strings argv;
        "env", strings (List.map (fun (k, v)  -> Printf.sprintf "%s=%s" k v) env);
        "cwd", `String cwd;
        "capabilities", `Assoc [
          "bounding", strings [
            "CAP_CHOWN";
            "CAP_DAC_OVERRIDE";
            "CAP_FSETID";
            "CAP_FOWNER";
            "CAP_MKNOD";
            "CAP_NET_RAW";
            "CAP_SETGID";
            "CAP_SETUID";
            "CAP_SETFCAP";
            "CAP_SETPCAP";
            "CAP_NET_BIND_SERVICE";
            "CAP_SYS_CHROOT";
            "CAP_KILL";
            "CAP_AUDIT_WRITE"
          ];
          "effective", strings [
            "CAP_CHOWN";
            "CAP_DAC_OVERRIDE";
            "CAP_FSETID";
            "CAP_FOWNER";
            "CAP_MKNOD";
            "CAP_NET_RAW";
            "CAP_SETGID";
            "CAP_SETUID";
            "CAP_SETFCAP";
            "CAP_SETPCAP";
            "CAP_NET_BIND_SERVICE";
            "CAP_SYS_CHROOT";
            "CAP_KILL";
            "CAP_AUDIT_WRITE"
          ];
          "inheritable", strings [
            "CAP_CHOWN";
            "CAP_DAC_OVERRIDE";
            "CAP_FSETID";
            "CAP_FOWNER";
            "CAP_MKNOD";
            "CAP_NET_RAW";
            "CAP_SETGID";
            "CAP_SETUID";
            "CAP_SETFCAP";
            "CAP_SETPCAP";
            "CAP_NET_BIND_SERVICE";
            "CAP_SYS_CHROOT";
            "CAP_KILL";
            "CAP_AUDIT_WRITE"
          ];
          "permitted", strings [
            "CAP_CHOWN";
            "CAP_DAC_OVERRIDE";
            "CAP_FSETID";
            "CAP_FOWNER";
            "CAP_MKNOD";
            "CAP_NET_RAW";
            "CAP_SETGID";
            "CAP_SETUID";
            "CAP_SETFCAP";
            "CAP_SETPCAP";
            "CAP_NET_BIND_SERVICE";
            "CAP_SYS_CHROOT";
            "CAP_KILL";
            "CAP_AUDIT_WRITE"
          ]
        ];
        "rlimits", `List [
          `Assoc [
            "type", `String "RLIMIT_NOFILE";
            "hard", `Int 1024;
            "soft", `Int 1024
          ];
        ];
        "noNewPrivileges", `Bool false;
      ];
      "root", `Assoc [
        "path", `String "rootfs";
        "readonly", `Bool false;
      ];
      "hostname", `String hostname;
      "mounts", `List [
        mount "/proc"
          ~options:[      (* TODO: copy to others? *)
            "nosuid";
            "noexec";
            "nodev";
          ]
          ~ty:"proc"
          ~src:"proc";
        mount "/dev"
          ~ty:"tmpfs"
          ~src:"tmpfs"
          ~options:[
            "nosuid";
            "strictatime";
            "mode=755";
            "size=65536k";
          ];
        mount "/dev/pts"
          ~ty:"devpts"
          ~src:"devpts"
          ~options:[
            "nosuid";
            "noexec";
            "newinstance";
            "ptmxmode=0666";
            "mode=0620";
          ];
        mount "/dev/shm"
          ~ty:"tmpfs"
          ~src:"shm"
          ~options:[
            "nosuid";
            "noexec";
            "nodev";
            "mode=1777";
            "size=65536k";
          ];
        mount "/dev/mqueue"
          ~ty:"mqueue"
          ~src:"mqueue"
          ~options:[
            "nosuid";
            "noexec";
            "nodev";
          ];
        mount "/sys"
          ~ty:"none"
          ~src:"/sys"
          ~options:[
            "rbind";
            "nosuid";
            "noexec";
            "nodev";
            "ro";
          ];
        mount "/etc/hosts"
          ~ty:"bind"
          ~src:"/var/lib/docker/tal/hosts"  (* XXX *)
          ~options:[
            "rbind";
            "rprivate"
          ];
        mount "/etc/resolv.conf"
          ~ty:"bind"
          ~src:"/etc/resolv.conf"  (* XXX *)
          ~options:[
            "rbind";
            "rprivate"
          ];
      ];
      "linux", `Assoc [
        "uidMappings", `List [
          `Assoc [
            "containerID", `Int 0;
            "hostID", `Int 1000;
            "size", `Int 1
          ];
        ];
        "gidMappings", `List [
          `Assoc [
            "containerID", `Int 0;
            "hostID", `Int 1000;
            "size", `Int 1;
          ];
        ];
        "namespaces", `List [
          `Assoc [
            "type", `String "pid"
          ];
          `Assoc [
            "type", `String "ipc"
          ];
          `Assoc [
            "type", `String "uts"
          ];
          `Assoc [
            "type", `String "mount"
          ];
        ];
        "maskedPaths", strings [
          "/proc/acpi";
          "/proc/asound";
          "/proc/kcore";
          "/proc/keys";
          "/proc/latency_stats";
          "/proc/timer_list";
          "/proc/timer_stats";
          "/proc/sched_debug";
          "/sys/firmware";
          "/proc/scsi"
        ];
        "readonlyPaths", strings [
          "/proc/bus";
          "/proc/fs";
          "/proc/irq";
          "/proc/sys";
          "/proc/sysrq-trigger"
        ]
      ];
    ]
end  

let next_id = ref 0

let write_config config dir =
  Lwt_io.(with_file ~mode:output) (dir / "config.json") @@ fun ch ->
  Lwt_io.write ch (Yojson.Safe.pretty_to_string config ^ "\n")

let run ?stdin:stdin t config result_tmp =
  write_config config result_tmp >>= fun () ->
  let id = string_of_int !next_id in
  incr next_id;
  let log_file = result_tmp / "log" in
  Os.with_open_out log_file (fun log ->
      Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
      let cmd = ["sudo"; "runc"; "--root"; t.runc_state_dir; "run"; id] in
      let stdout = `FD_copy out_w.raw in
      let stderr = stdout in
      let copy_log = Os.tee ~src:out_r ~dst:log in
      let proc =
        let stdin = Option.map (fun x -> `FD_copy x.Os.raw) stdin in
        Os.exec ~cwd:result_tmp ?stdin ~stdout ~stderr cmd in
      Os.close out_w;
      Option.iter Os.close stdin;
      proc >>= fun () ->
      copy_log
    )
  >|= fun () ->
  Ok ()

let create ~runc_state_dir =
  Os.ensure_dir runc_state_dir;
  { runc_state_dir }
