type t = [
  | `File of (string * Sha256.t)
  | `Symlink of (string * string)
  | `Dir of (string * t list)
] [@@deriving sexp_of]

val generate : exclude:string list -> src_dir:Eio.Fs.dir Eio.Path.t -> string -> (t, [> `Msg of string]) result
(** [generate ~exclude ~src_dir src] returns a manifest of the subtree at [src_dir/src].
    Note that [src_dir] is a native platform path, but [src] is always Unix-style.
    Files with basenames in [exclude] are ignored.
    Returns an error if [src] is not under [src_dir] or does not exist. *)
