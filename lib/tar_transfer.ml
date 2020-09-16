open Lwt.Infix

let ( / ) = Filename.concat

module Tar_lwt_unix = struct
  include Tar_lwt_unix

  (* Copied from tar_lwt_unix.ml (ISC license). Not sure why this isn't exposed.

     ## ISC License

     Copyright (c) 2012-2018 The ocaml-tar contributors

     Permission to use, copy, modify, and/or distribute this software for any
     purpose with or without fee is hereby granted, provided that the above
     copyright notice and this permission notice appear in all copies.

     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
     WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
     ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
     WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
     OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *)

  module Writer = struct
    type out_channel = Lwt_unix.file_descr
    type 'a t = 'a Lwt.t
    let really_write fd = Lwt_cstruct.(complete (write fd))
  end

  module HW = Tar.HeaderWriter(Lwt)(Writer)

  let write_block (header: Tar.Header.t) (body: Lwt_unix.file_descr -> unit Lwt.t) (fd : Lwt_unix.file_descr) =
    HW.write header fd
    >>= fun () ->
    body fd >>= fun () ->
    Writer.really_write fd (Tar.Header.zero_padding header)

  let write_end (fd: Lwt_unix.file_descr) =
    Writer.really_write fd Tar.Header.zero_block >>= fun () ->
    Writer.really_write fd Tar.Header.zero_block
end

let copy_to ~dst src =
  let len = 4096 in
  let buf = Bytes.create len in
  let rec aux () =
    Lwt_io.read_into src buf 0 len >>= function
    | 0 -> Lwt.return_unit
    | n -> Os.write_all dst buf 0 n >>= aux
  in
  aux ()

let copy_file ~src ~dst ~to_untar =
  Lwt_unix.LargeFile.lstat src >>= fun stat ->
  let hdr = Tar.Header.make
      ~file_mode:(if stat.Lwt_unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644)
      ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
      dst stat.Lwt_unix.LargeFile.st_size
  in
  Tar_lwt_unix.write_block hdr (fun ofd ->
      Lwt_io.(with_file ~mode:input) src (copy_to ~dst:ofd)
    ) to_untar

let copy_symlink ~src ~target ~dst ~to_untar =
  Lwt_unix.LargeFile.lstat src >>= fun stat ->
  let hdr = Tar.Header.make
      ~file_mode:0o777
      ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
      ~link_indicator:Tar.Header.Link.Symbolic
      ~link_name:target
      dst 0L
  in
  Tar_lwt_unix.write_block hdr (fun _ -> Lwt.return_unit) to_untar

let rec copy_dir ~src_dir ~src ~dst ~(items:(Manifest.t list)) ~to_untar =
  Fmt.pr "Copy dir %S -> %S@." src dst;
  Lwt_unix.LargeFile.lstat (src_dir / src) >>= fun stat ->
  begin 
    let hdr = Tar.Header.make
        ~file_mode:0o755
        ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
        (dst ^ "/") 0L
    in
    Tar_lwt_unix.write_block hdr (fun _ -> Lwt.return_unit) to_untar
  end >>= fun () ->
  items |> Lwt_list.iter_s (function
      | `File (src, _) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_file ~src ~dst ~to_untar
      | `Symlink (src, target) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_symlink ~src ~target ~dst ~to_untar
      | `Dir (src, items) ->
        let dst = dst / Filename.basename src in
        copy_dir ~src_dir ~src ~dst ~items ~to_untar
    )

let send_files ~src_dir ~src_manifest ~dst ~to_untar =
  src_manifest |> Lwt_list.iter_s (function
      | `File (path, _) ->
        let src = src_dir / path in
        let dst = dst / (Filename.basename path) in       (* maybe don't copy docker's bad design here? *)
        copy_file ~src ~dst ~to_untar
      | `Symlink (src, target) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_symlink ~src ~target ~dst ~to_untar
      | `Dir (src, items) ->
        let dst =
          if dst.[String.length dst - 1] = '/' then dst / Filename.basename src
          else dst
        in
        copy_dir ~src_dir ~src ~dst ~items ~to_untar
    )
  >>= fun () ->
  Tar_lwt_unix.write_end to_untar
