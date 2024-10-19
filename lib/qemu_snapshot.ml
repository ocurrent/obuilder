open Lwt.Infix

let ( / ) = Filename.concat

let fetch ~log:_ ~root ~rootfs base =
    let base_image = match base with
    | "busybox" -> root / "base-image" / "ubuntu-noble-x86_64-ocaml-4.14.img"
    | x -> root / "base-image" / (x ^ ".img") in
    Os.sudo [ "qemu-img"; "create"; "-f"; "qcow2"; "-b"; base_image; "-F"; "qcow2"; rootfs / "image.qcow2" ] >>= fun () ->
  Lwt.return []


