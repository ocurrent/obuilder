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

(* Note: we remove the extra () in the sexp string format,
   formatting them as if they were in-line records. e.g.

   (copy ((src ...) (dst ...))) becomes (copy (src ...) (dst ...)).

   (comment ATOM) stays as it is.

   But if we had a constructor that would normally serialise as (foo (ATOM))
   then we'd convert it to (foo ATOM) and then it would look like the same as
   if it had been (foo ATOM) to start with. Records are fine because the first
   argument is always a list (not an atom). Tuples are fine because they don't
   give a single atom. But a list of strings would be a problem, if there was
   only one string.
*)
type op = [
  | `Comment of string
  | `Workdir of string
  | `Run of run
  | `Copy of copy
  | `User of user
  | `Env of (string * string)
] [@@deriving show, sexp]

let sexp_of_op x : Sexplib.Sexp.t =
  match sexp_of_op x with
  | List [Atom name; args] ->
    let name = Sexplib.Sexp.Atom (String.lowercase_ascii name) in
    let args =
      match args with
      | List [Atom _] as x -> Fmt.failwith "Invalid args: %a" Sexplib.Sexp.pp_hum x
      | List args -> args       (* Result is not an atom *)
      | Atom _ as x -> [x]      (* Result is an atom *)
    in
    Sexplib.Sexp.List (name :: args)
  | x -> Fmt.failwith "Invalid op: %a" Sexplib.Sexp.pp_hum x

let op_of_sexp =
  let open Sexplib.Sexp in
  function
  | List (Atom name :: args) ->
    let name = String.capitalize_ascii name in
    let args = 
      match args with
      | [Atom _] as x -> x
      | args -> [List args]
    in
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
