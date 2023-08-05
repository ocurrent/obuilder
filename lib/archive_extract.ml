open Lwt.Infix

let ( / ) = Filename.concat

let fetch ~log:_ ~rootfs base =
  let zfs_volume = String.sub rootfs 1 (String.length rootfs - 1) in  (* remove / from front *)
  Os.sudo [ "zfs"; "clone"; "obuilder" / "base-image" / base / "rootfs" ^ "@snap"; zfs_volume ] >>= fun () ->
  Lwt.return []
