type copy = { src : string list; dst : string } [@@deriving show { with_path = false }]

type user = { uid : int; gid : int } [@@deriving show { with_path = false }]

type op = [
  | `Comment of string
  | `Workdir of string
  | `Run of string
  | `Copy of copy
  | `User of user
  | `Env of (string * string)
] [@@deriving show]

type stage = {
  from : string;
  ops : op list;
}

let comment c = `Comment c
let workdir x = `Workdir x
let run x = `Run x
let copy src dst = `Copy { src; dst }
let env k v = `Env (k, v)
let user ~uid ~gid = `User { uid; gid }

let root = { uid = 0; gid = 0 }
