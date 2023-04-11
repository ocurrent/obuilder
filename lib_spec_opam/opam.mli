open Obuilder_spec

(** opam_init [?opamrc] [opam_version] [distro] *)
val opam_init : ?opamrc:string -> Opam_version.t -> Distro.t -> op list

(** The name of the OPAM download cache *)
val download_cache : string

(** caches [?extra_caches] [distro] *)
val caches : ?opam_download_cache:string -> Distro.t -> Cache.t list

(** set_personality [arch] *)
val set_personality : Ocaml_version.arch -> op list
