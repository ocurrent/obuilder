(** Variants of builds to perform.

    A build variant covers the OCaml version, opam version, hardware [arch] and
    operating system distribution. *)

type t [@@deriving eq, ord, yojson]

val v :
  arch:Ocaml_version.arch ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  (t, [> `Msg of string ]) result

val arch : t -> Ocaml_version.arch
val distro_str : t -> string
val distro : t -> Distro.t
val ocaml_version : t -> Ocaml_version.t
val opam_version : t -> Opam_version.t
val id : t -> string

val docker_tag : t -> string
(** Print [t] as a docker tag. *)

val pp : t Fmt.t
val to_string : t -> string
val of_string : ?default:Opam_version.t -> string -> t
val os : t -> Distro.os_family

val macos_distributions : string list
