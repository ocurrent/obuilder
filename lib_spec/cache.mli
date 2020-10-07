type t = {
  id : string;
  target : string;
  buildkit_options : (string * string) list;        (* Only used when converting to Docker BuildKit format. *)
} [@@deriving sexp]

val v : ?buildkit_options:(string * string) list -> target:string -> string -> t
(** [v ~target id] mounts cache [id] at [target]. *)
