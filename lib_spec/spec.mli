type copy = {
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

type stage = {
  from : string;
  ops : op list;
} [@@deriving sexp]

val stage : from:string -> op list -> stage

val comment : ('a, unit, string, op) format4 -> 'a
val workdir : string -> op
val shell : string list -> op
val run : ?cache:Cache.t list -> ?network:string list -> ('a, unit, string, op) format4 -> 'a
val copy : ?exclude:string list -> string list -> dst:string -> op
val env : string -> string -> op
val user : uid:int -> gid:int -> op

val root : user
