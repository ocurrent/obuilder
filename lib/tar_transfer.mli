val send_files :
  src_dir:string ->
  src_manifest:Manifest.t list ->        (* Better name? *)
  dst:string ->
  user:Obuilder_spec.user ->
  to_untar:Lwt_unix.file_descr ->
  unit Lwt.t
(** [send_files ~src_dir ~src_manifest ~dst ~user ~to_untar] writes a tar-format stream
    to [to_untar] containing all the files listed in [src_manifest], which are
    loaded from [src_dir]. The file names in the stream are prefixed with [dst].
    All files are listed as being owned by [user]. *)
