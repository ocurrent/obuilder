open Lwt.Infix

let ( / ) = Filename.concat

(* On FreeBSD the input is
    rootfs = "/obuilder/result/522fb2a0e81ba278bc1ae7314bd754201505e6493f4f2f40a166c416624a4005/rootfs"
    with base = "busybox", or base = "freebsd-ocaml-4.14" -> just clone rootfs
   
   On macOS the input is
    rootfs = "/Volumes/obuilder/result/522fb2a0e81ba278bc1ae7314bd754201505e6493f4f2f40a166c416624a4005/rootfs"
    with base = "busybox", or base = "macos-homebrew-ocaml-4.14" -> clone home and brew subvolumes *)

let fetch ~log:_ ~root:_ ~rootfs base =
  let path =
    let remove_on_match s lst = if List.hd lst = s then List.tl lst else lst in
    String.split_on_char '/' rootfs
    |> List.filter (fun x -> String.length x > 0)
    |> remove_on_match "Volumes" |> List.rev
    |> remove_on_match "rootfs" |> List.rev in
  let zfs_rootfs = String.concat "/" path in
  let base_image = (List.hd path) / "base-image" / base in
  Lwt_process.pread ("", [| "zfs"; "list"; "-H"; "-r"; "-o"; "name"; base_image |]) >>= fun output ->
    let len = String.length base_image in
    String.split_on_char '\n' output |> List.map (fun s -> (s, String.length s)) |>
    List.filter (fun (_, l) -> l > len) |> List.map (fun (s, l) -> String.sub s (len + 1) (l - len - 1)) |>
    Lwt_list.iter_s (fun subvolume ->
      Os.sudo ["zfs"; "clone"; base_image / subvolume ^ "@snap"; zfs_rootfs / subvolume ]) >>= fun () ->
  Lwt.return []

