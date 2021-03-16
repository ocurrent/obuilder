type copy = {
  from : [`Context | `Build of string];
  src : string list;
  dst : string;
  exclude : string list;
} [@@deriving sexp]

type user = {
  uid : int;
  gid : int
} [@@deriving sexp]

type run = {
  cache : Cache.t list;
  network : string list;
  tmpfs : string list;
  shell : string;
} [@@deriving sexp]

type op = [
  | `Comment of string
  | `Workdir of string
  | `Shell of string list
  | `Run of run
  | `Copy of copy
  | `User of user
  | `Env of (string * string)
] [@@deriving sexp]

type t = private {
  child_builds : (string * t) list;
  from : string;
  ops : op list;
} [@@deriving sexp]

val stage : ?child_builds:(string * t) list -> from:string -> op list -> t

val comment : ('a, unit, string, op) format4 -> 'a
val workdir : string -> op
val shell : string list -> op
val run : ?cache:Cache.t list -> ?network:string list -> ?tmpfs:string list -> ('a, unit, string, op) format4 -> 'a
val copy : ?from:[`Context | `Build of string] -> ?exclude:string list -> string list -> dst:string -> op
val env : string -> string -> op
val user : uid:int -> gid:int -> op

val root : user

val pp : t Fmt.t
(** [pp f s] is similar to [Sexplib.Sexp.pp_hum f (sexp_of_t s)], but
    attempts to improve the layout slightly by putting each operation on its
    own line. *)

val pp_op : op Fmt.t
(** [pp_op] formats [op] as an S-expression. *)
