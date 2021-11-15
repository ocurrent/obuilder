open Lwt.Infix

let fetch ~log:_ ~rootfs base =
  let base = Filename.concat "/Users" base in
  Macos.copy_template ~base ~local:rootfs >>= fun _ ->
  Os.sudo [ "chown"; "-R"; ":1000"; rootfs ] >>= fun () ->
  Os.sudo [ "chmod"; "-R"; "g+w"; rootfs ] >>= fun () ->
  Lwt.return []
