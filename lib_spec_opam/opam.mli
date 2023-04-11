open Obuilder_spec

(** opam_init [?opamrc] [opam_version] [distro] *)
val opam_init : ?opamrc:string -> Opam_version.t -> Distro.t -> op list

(** caches [?extra_caches] [distro] *)
val caches : ?extra_caches:(string * string) list -> Distro.t -> Cache.t list

(** set_personality [arch] *)
val set_personality : Ocaml_version.arch -> op list
