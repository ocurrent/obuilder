val send_files :
  src_dir:string ->
  src_manifest:Manifest.t list ->
  dst_dir:string ->
  user:Obuilder_spec.user ->
  to_untar:Lwt_unix.file_descr ->
  unit Lwt.t
(** [send_files ~src_dir ~src_manifest ~dst_dir ~user ~to_untar] writes a tar-format stream
    to [to_untar] containing all the files listed in [src_manifest], which are
    loaded from [src_dir]. The file names in the stream are prefixed with [dst_dir].
    All files are listed as being owned by [user]. *)

val send_file :
  src_dir:string ->
  src_manifest:Manifest.t ->
  dst:string ->
  user:Obuilder_spec.user ->
  to_untar:Lwt_unix.file_descr ->
  unit Lwt.t
(** [send_files ~src_dir ~src_manifest ~dst ~user ~to_untar] writes a tar-format stream
    to [to_untar] containing the item [src_manifest], which is loaded from
    [src_dir]. The item will be copied as [dst].
    All files are listed as being owned by [user]. *)

val transform_files :
  from_tar:Lwt_unix.file_descr ->
  src_manifest:Manifest.t list ->
  dst_dir:string ->
  user:Obuilder_spec.user ->
  to_untar:Lwt_unix.file_descr ->
  unit Lwt.t
(** [transform_files ~src_dir ~from_tar ~src_manifest ~dst_dir ~user ~to_untar]
    prefixes the files names of all the files found in [from_tar], a tar archive
    streamed in input, with [dst_dir], and writes the resulting tar-format
    stream to [to_untar]. All files are listed as being owned by [user]. *)

val transform_file :
  from_tar:Lwt_unix.file_descr ->
  src_manifest:Manifest.t ->
  dst:string ->
  user:Obuilder_spec.user ->
  to_untar:Lwt_unix.file_descr ->
  unit Lwt.t
(** [transform_files ~src_dir ~from_tar ~src_manifest ~dst ~user ~to_untar]
    renames the _unique_ file found in [from_tar], a tar archive streamed in
    input, to [dst], and writes the resulting tar-format stream to
    [to_untar]. All files are listed as being owned by [user]. *)
