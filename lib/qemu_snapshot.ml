open Lwt.Infix

let ( / ) = Filename.concat

let fetch ~log:_ ~root ~rootfs base =
  Os.sudo [ "qemu-img"; "create";
            "-f"; "qcow2"; "-b"; root / "base-image" / (base ^ ".qcow2");
            "-F"; "qcow2"; rootfs / "image.qcow2" ] >>= fun () ->
  Lwt.return []

