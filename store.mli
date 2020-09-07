type t

module Tree : sig
  type t [@@deriving show]

  val hash : t -> string
end

val create : string -> t
(** [create path] is a new store in btrfs directory [path]. *)

val build : t -> ?base:Tree.t -> hash:string -> (string -> unit Lwt.t) -> Tree.t Lwt.t
(** [build t ~base ~hash fn] returns the path of the build with hash [hash].
    If it doesn't exist yet in the store, it runs [fn tmpdir] to create
    it first, where [tmpdir] is initially a clone of [base]. On success,
    [tmpdir] is saved under [hash]. *)

val path : t -> Tree.t -> string
