open Sexplib.Std

type copy = { src : string list; dst : string }
[@@deriving show { with_path = false }, of_sexp]

let copy_of_sexp =
  let open Sexplib.Sexp in function
    | List (List (Atom "src" :: src) :: rest) ->
      copy_of_sexp (List (List [Atom "src"; List src] :: rest))
    | x -> Fmt.failwith "Invalid copy spec: %a" Sexplib.Sexp.pp_hum x

let sexp_of_copy { src; dst } =
  let open Sexplib.Sexp in
  let src = List.map (fun x -> Atom x) src in
  List [List (Atom "src" :: src);
        List [Atom "dst"; Atom dst]]

type user = { uid : int; gid : int }
[@@deriving show { with_path = false }, sexp]

type run = { shell : string }
[@@deriving show { with_path = false }, sexp]

type op = [
  | `Comment of string
  | `Workdir of string
  | `Shell of string list [@sexp.list]
  | `Run of run
  | `Copy of copy
  | `User of user
  | `Env of (string * string)
] [@@deriving show, sexp]

(* For some ops, we remove the extra () in the sexp string format,
   formatting them as if they were in-line records. e.g.
   (copy ((src ...) (dst ...))) becomes (copy (src ...) (dst ...)). *)
let inline = function
  | "run" | "copy" | "user" | "env" -> true
  | _ -> false

let sexp_of_op x : Sexplib.Sexp.t =
  match sexp_of_op x with
  | List (Atom name :: args) ->
    let name = String.lowercase_ascii name in
    let args =
      if inline name then
        match args with
        | [List args] -> args
        | _ -> failwith "Inline op must be a record!"
      else args
    in
    Sexplib.Sexp.List (Sexplib.Sexp.Atom name :: args)
  | x -> Fmt.failwith "Invalid op: %a" Sexplib.Sexp.pp_hum x

let op_of_sexp x =
  let open Sexplib.Sexp in
  (* Fmt.pr "sexp_of_op: %a@." Sexplib.Sexp.pp_hum x; *)
  match x with
  | List (Atom name :: args) ->
    let args = if inline name then [List args] else args in
    let name = String.capitalize_ascii name in
    op_of_sexp (List (Atom name :: args))
  | x -> Fmt.failwith "Invalid op: %a" Sexplib.Sexp.pp_hum x

type stage = {
  from : string;
  ops : op list;
} [@@deriving show]

let sexp_of_stage { from; ops } =
  let open Sexplib.Sexp in
  List (List [ Atom "from"; Atom from ] :: List.map sexp_of_op ops)

let stage_of_sexp = function
  | Sexplib.Sexp.List (List [ Atom "from"; Atom from ] :: ops) ->
    { from; ops = List.map op_of_sexp ops }
  | x -> Fmt.failwith "Invalid stage: %a" Sexplib.Sexp.pp_hum x

let comment c = `Comment c
let workdir x = `Workdir x
let run x = `Run { shell = x }
let copy src dst = `Copy { src; dst }
let env k v = `Env (k, v)
let user ~uid ~gid = `User { uid; gid }

let root = { uid = 0; gid = 0 }
