open Lwt.Infix
open Eio

let level = Tar.Header.GNU

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

  let write_block ?level (header: Tar.Header.t) (body: Lwt_unix.file_descr -> unit Lwt.t) (fd : Lwt_unix.file_descr) =
    HW.write ?level header fd
    >>= fun () ->
    body fd >>= fun () ->
    Writer.really_write fd (Tar.Header.zero_padding header)

  let write_end (fd: Lwt_unix.file_descr) =
    Writer.really_write fd Tar.Header.zero_block >>= fun () ->
    Writer.really_write fd Tar.Header.zero_block
end

let copy_to ~dst src =
  let len = 4096 in
  let buf = Cstruct.create len in
  let rec aux () =
    match Flow.single_read src (Cstruct.sub buf 0 len) with
    | 0 -> Lwt.return_unit
    | n ->
      (* TODO: Remove once we port Tar to Eio *)
      Eio.Switch.run @@ fun sw ->
      let sock = Eio_unix.import_socket_stream ~sw ~close_unix:false dst in 
      Os.write_all sock buf 0 n |> aux
  in
  aux ()

let copy_file ~src ~dst ~to_untar ~user =
  let stat = Eio.File.stat src in
  let hdr = Tar.Header.make
      ~file_mode:(if stat.File.Stat.perm land 0o111 <> 0 then 0o755 else 0o644)
      ~mod_time:(Int64.of_float stat.File.Stat.mtime)
      ~user_id:user.Obuilder_spec.uid
      ~group_id:user.Obuilder_spec.gid
      dst (Optint.Int63.to_int64 stat.File.Stat.size)
  in
  Lwt_eio.Promise.await_lwt @@
  Tar_lwt_unix.write_block ~level hdr (fun ofd ->
      let dst = Lwt_unix.unix_file_descr ofd in 
      (copy_to ~dst src)
    ) to_untar

let copy_symlink ~src ~target ~dst ~to_untar ~user =
  let stat = Eio.File.stat src in
  let hdr = Tar.Header.make
      ~file_mode:0o777
      ~mod_time:(Int64.of_float stat.File.Stat.mtime)
      ~link_indicator:Tar.Header.Link.Symbolic
      ~link_name:target
      ~user_id:user.Obuilder_spec.uid
      ~group_id:user.Obuilder_spec.gid
      dst 0L
  in
  Lwt_eio.Promise.await_lwt @@
  Tar_lwt_unix.write_block ~level hdr (fun _ -> Lwt.return_unit) to_untar

let rec copy_dir ~src_dir ~src ~dst ~(items:(Manifest.t list)) ~to_untar ~user =
  Log.debug(fun f -> f "Copy dir %S -> %S@." src dst);
  Path.(with_open_in (src_dir / src)) @@ fun src ->
  let stat = File.stat src in
  begin
    let hdr = Tar.Header.make
        ~file_mode:0o755
        ~mod_time:(Int64.of_float stat.File.Stat.mtime)
        ~user_id:user.Obuilder_spec.uid
        ~group_id:user.Obuilder_spec.gid
        (dst ^ "/") 0L
    in
    Lwt_eio.Promise.await_lwt @@
    Tar_lwt_unix.write_block ~level hdr (fun _ -> Lwt.return_unit) to_untar
  end;
  send_dir ~src_dir ~dst ~to_untar ~user items

and send_dir ~src_dir ~dst ~to_untar ~user items =
  items |> List.iter (function
      | `File (src, _) ->
        Path.(with_open_in (src_dir / src)) @@ fun src_in ->
        let dst = Filename.(concat dst (basename src)) in
        copy_file ~src:src_in ~dst ~to_untar ~user
      | `Symlink (src, target) ->
        Path.(with_open_in (src_dir / src)) @@ fun src_in ->
        let dst = Filename.(concat dst (Filename.basename src)) in
        copy_symlink ~src:src_in ~target ~dst ~to_untar ~user
      | `Dir (src, items) ->
        let dst = Filename.(concat dst (Filename.basename src)) in
        copy_dir ~src_dir ~src ~dst ~items ~to_untar ~user
    )

let remove_leading_slashes = Astring.String.drop ~sat:((=) '/')

let send_files ~src_dir ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  send_dir ~src_dir ~dst ~to_untar ~user src_manifest;
  Lwt_eio.Promise.await_lwt @@ Tar_lwt_unix.write_end to_untar

let send_file ~src_dir ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  begin
    match src_manifest with
    | `File (path, _) ->
      Path.(with_open_in (src_dir / path)) @@ fun src ->
      copy_file ~src ~dst ~to_untar ~user
    | `Symlink (src, target) ->
      Path.(with_open_in (src_dir / src)) @@ fun src ->
      copy_symlink ~src ~target ~dst ~to_untar ~user
    | `Dir (src, items) ->
      copy_dir ~src_dir ~src ~dst ~items ~to_untar ~user
  end;
  Lwt_eio.Promise.await_lwt @@
  Tar_lwt_unix.write_end to_untar
