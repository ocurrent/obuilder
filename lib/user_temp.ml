open Lwt.Infix

let ( / ) = Filename.concat

let fetch ~log:_ ~rootfs base =
  let zfs_volume = String.sub rootfs 9 (String.length rootfs - 16) in  (* remove /Volume/ from front and /rootfs from the end *)
  Os.sudo [ "zfs"; "clone"; "-o"; "mountpoint=none"; "obuilder" / "base-image" / base / "home@snap"; zfs_volume / "home" ] >>= fun () ->
  Os.sudo [ "zfs"; "clone"; "-o"; "mountpoint=none"; "obuilder" / "base-image" / base / "brew@snap"; zfs_volume / "brew" ] >>= fun () ->
  Lwt.return []
