let ( / ) = Filename.concat

let level = Tar.Header.GNU

(* Helper to unwrap tar write operation results *)
let unwrap_write_result = function
  | Ok () -> Lwt.return_unit
  | Error (`Msg m) -> failwith m
  | Error (`Unix (e, fn, arg)) ->
    Fmt.failwith "%s(%s): %s" fn arg (Unix.error_message e)

let rec lwt_write_all fd buf ofs len =
  if len = 0 then Lwt.return_unit
  else
    Lwt.bind (Lwt_unix.write fd buf ofs len) (fun n ->
      lwt_write_all fd buf (ofs + n) (len - n))

let lwt_write_all_string fd s ofs len =
  lwt_write_all fd (Bytes.unsafe_of_string s) ofs len

let get_ids = function
  | `Unix user -> Some user.Obuilder_spec.uid, Some user.gid, None, None
  | `Windows user when user.Obuilder_spec.name = "ContainerAdministrator" ->
    (* https://cygwin.com/cygwin-ug-net/ntsec.html#ntsec-mapping *)
    let x = 93 and rid = 1 in
    Some (0x1000 * x + rid), Some (0x1000 * x + rid), Some user.name, Some user.name
  | `Windows _ -> None, None, None, None

let copy_file_lwt ~src ~dst ~to_untar ~user =
  Lwt.bind (Lwt_unix.LargeFile.lstat src) (fun stat ->
    let user_id, group_id, uname, gname = get_ids user in
    let hdr = Tar.Header.make
        ~file_mode:(if stat.Lwt_unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644)
        ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
        ?user_id ?group_id ?uname ?gname
        dst stat.Lwt_unix.LargeFile.st_size
    in
    Lwt.bind (Tar_lwt_unix.append_file ~level ~header:hdr src to_untar) unwrap_write_result)

let copy_symlink_lwt ~src ~target ~dst ~to_untar ~user =
  Lwt.bind (Lwt_unix.LargeFile.lstat src) (fun stat ->
    let user_id, group_id, uname, gname = get_ids user in
    let hdr = Tar.Header.make
        ~file_mode:0o777
        ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
        ~link_indicator:Tar.Header.Link.Symbolic
        ~link_name:target
        ?user_id ?group_id ?uname ?gname
        dst 0L
    in
    Lwt.bind (Tar_lwt_unix.write_header ~level hdr to_untar) unwrap_write_result)

let rec copy_dir_lwt ~src_dir ~src ~dst ~(items:(Manifest.t list)) ~to_untar ~user =
  Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
  Lwt.bind (Lwt_unix.LargeFile.lstat (src_dir / src)) (fun stat ->
    let user_id, group_id, uname, gname = get_ids user in
    let hdr = Tar.Header.make
        ~file_mode:0o755
        ~mod_time:(Int64.of_float stat.Lwt_unix.LargeFile.st_mtime)
        ?user_id ?group_id ?uname ?gname
        (dst ^ "/") 0L
    in
    Lwt.bind (Tar_lwt_unix.write_header ~level hdr to_untar) (fun r ->
      Lwt.bind (unwrap_write_result r) (fun () ->
        send_dir_lwt ~src_dir ~dst ~to_untar ~user items)))

and send_dir_lwt ~src_dir ~dst ~to_untar ~user items =
  Lwt_list.iter_s (function
      | `File (src, _) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_file_lwt ~src ~dst ~to_untar ~user
      | `Symlink (src, target) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_symlink_lwt ~src ~target ~dst ~to_untar ~user
      | `Dir (src, items) ->
        let dst = dst / Filename.basename src in
        copy_dir_lwt ~src_dir ~src ~dst ~items ~to_untar ~user
    ) items

let remove_leading_slashes = Astring.String.drop ~sat:((=) '/')

let write_end_lwt to_untar =
  Lwt.bind (Tar_lwt_unix.write_end to_untar) (function
    | Ok () -> Lwt.return_unit
    | Error (`Msg m) -> failwith m)

let send_files_lwt ~src_dir ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  Lwt.bind (send_dir_lwt ~src_dir ~dst ~to_untar ~user src_manifest) (fun () ->
    write_end_lwt to_untar)

let send_file_lwt ~src_dir ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  Lwt.bind begin
    match src_manifest with
    | `File (path, _) ->
      let src = src_dir / path in
      copy_file_lwt ~src ~dst ~to_untar ~user
    | `Symlink (src, target) ->
      let src = src_dir / src in
      copy_symlink_lwt ~src ~target ~dst ~to_untar ~user
    | `Dir (src, items) ->
      copy_dir_lwt ~src_dir ~src ~dst ~items ~to_untar ~user
  end (fun () ->
    write_end_lwt to_untar)

(* Public direct-style wrappers *)

let send_files ~src_dir ~src_manifest ~dst_dir ~user ~to_untar =
  let lwt_fd = Lwt_unix.of_unix_file_descr ~blocking:true to_untar in
  Lwt_eio.run_lwt (fun () ->
    send_files_lwt ~src_dir ~src_manifest ~dst_dir ~user ~to_untar:lwt_fd)

let send_file ~src_dir ~src_manifest ~dst ~user ~to_untar =
  let lwt_fd = Lwt_unix.of_unix_file_descr ~blocking:true to_untar in
  Lwt_eio.run_lwt (fun () ->
    send_file_lwt ~src_dir ~src_manifest ~dst ~user ~to_untar:lwt_fd)

let transform ~user fname hdr =
  (* Make a copy to erase unneeded data from the tar headers. *)
  let hdr' = Tar.Header.(make ~file_mode:hdr.file_mode ~mod_time:hdr.mod_time hdr.file_name hdr.file_size) in
  let hdr' = match user with
    | `Unix user ->
      { hdr' with Tar.Header.user_id = user.Obuilder_spec.uid; group_id = user.gid; }
    | `Windows user when user.Obuilder_spec.name = "ContainerAdministrator" ->
      (* https://cygwin.com/cygwin-ug-net/ntsec.html#ntsec-mapping *)
      let id = let x = 93 and rid = 1 in 0x1000 * x + rid in
      { hdr' with user_id = id; group_id = id; uname = user.name; gname = user.name; }
    | `Windows _ -> hdr'
  in
  match hdr.Tar.Header.link_indicator with
  | Normal ->
    { hdr' with
      file_mode = if hdr.file_mode land 0o111 <> 0 then 0o755 else 0o644;
      file_name = fname hdr.file_name; }
  | Symbolic ->
    { hdr' with
      file_mode = 0o777;
      file_name = fname hdr.file_name;
      link_indicator = hdr.link_indicator;
      link_name = hdr.link_name; }
  | Directory ->
    { hdr' with
      file_mode = 0o755;
      file_name = fname hdr.file_name ^ "/"; }
  | _ -> Fmt.invalid_arg "Unsupported file type"

let rec map_transform ~dst transformations = function
  | `File (src, _) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst
  | `Symlink (src, _) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst
  | `Dir (src, items) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst;
    Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
    List.iter (map_transform ~dst transformations) items

(* Transform a tar archive: read from source, transform headers, write to dest.
   Uses Tar.fold on the source fd with Tar.High to lift writes to the dest fd. *)
let rec transform_archive_lwt ~transform_hdr ~from_tar ~to_untar =
  let open Tar.Syntax in
  let f ?global:_ hdr () =
    let file_size = Int64.to_int hdr.Tar.Header.file_size in
    let hdr' = transform_hdr hdr in
    if file_size > 0 then begin
      (* Read the file content from source (fold handles padding) *)
      let* data = Tar.really_read file_size in
      (* Write transformed entry to dest *)
      Tar_lwt_unix.value (
        Lwt.bind (Tar_lwt_unix.write_header ~level hdr' to_untar) (function
          | Error (`Msg m) -> Lwt.return_error (`Msg m)
          | Error (`Unix (e, fn, arg)) ->
            Lwt.return_error (`Msg (Fmt.str "%s(%s): %s" fn arg (Unix.error_message e)))
          | Ok () ->
            Lwt.bind (lwt_write_all_string to_untar data 0 file_size) (fun () ->
              let padding = Tar.Header.zero_padding hdr' in
              let plen = String.length padding in
              if plen > 0 then
                Lwt.bind (lwt_write_all_string to_untar padding 0 plen) (fun () ->
                  Lwt.return_ok ())
              else
                Lwt.return_ok ())))
    end else begin
      (* Directory, symlink, or empty file: just write header *)
      Tar_lwt_unix.value (
        Lwt.bind (Tar_lwt_unix.write_header ~level hdr' to_untar) (function
          | Error (`Msg m) -> Lwt.return_error (`Msg m)
          | Error (`Unix (e, fn, arg)) ->
            Lwt.return_error (`Msg (Fmt.str "%s(%s): %s" fn arg (Unix.error_message e)))
          | Ok () -> Lwt.return_ok ()))
    end
  in
  Lwt.bind (Tar_lwt_unix.run (Tar.fold f ()) from_tar) (function
    | Error err ->
      Fmt.failwith "Tar transform error: %a" Tar_lwt_unix.pp_decode_error err
    | Ok () -> write_end_lwt to_untar)

and transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  let transformations = Hashtbl.create ~random:true 64 in
  List.iter (map_transform ~dst transformations) src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found -> Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  let lwt_from = Lwt_unix.of_unix_file_descr ~blocking:true from_tar in
  let lwt_to = Lwt_unix.of_unix_file_descr ~blocking:true to_untar in
  Lwt_eio.run_lwt (fun () ->
    transform_archive_lwt ~transform_hdr:(transform ~user fname) ~from_tar:lwt_from ~to_untar:lwt_to)

let transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  let transformations = Hashtbl.create ~random:true 1 in
  let map_transform = function
    | `File (src, _) -> Hashtbl.add transformations src dst
    | `Symlink (src, _) -> Hashtbl.add transformations src dst
    | `Dir (src, items) ->
      Hashtbl.add transformations src dst;
      Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
      List.iter (map_transform ~dst transformations) items
  in
  map_transform src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found -> Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  let lwt_from = Lwt_unix.of_unix_file_descr ~blocking:true from_tar in
  let lwt_to = Lwt_unix.of_unix_file_descr ~blocking:true to_untar in
  Lwt_eio.run_lwt (fun () ->
    transform_archive_lwt ~transform_hdr:(fun hdr ->
        let hdr' = transform ~user fname hdr in
        Log.debug (fun f -> f "Copying %s -> %s" hdr.Tar.Header.file_name hdr'.Tar.Header.file_name);
        hdr')
      ~from_tar:lwt_from ~to_untar:lwt_to)
