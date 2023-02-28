(** Opam versions supported. *)

type t = [ `V2_0 | `V2_1 | `Dev ] [@@deriving ord, yojson, eq]

val pp : Distro.os_family -> t Fmt.t
val to_string : Distro.os_family -> t -> string
val to_string_with_patch : Distro.os_family -> t -> string
val of_string : string -> (t, [ `Msg of string ]) result
