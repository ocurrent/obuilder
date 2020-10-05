open Sexplib.Std

type cache_id = string

let cache_id = function
  | "" -> Fmt.error_msg "Cache ID is empty!"
  | name ->
    let p = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_' -> true
      | _ -> false
    in
    if Astring.String.for_all p name then (
      match name.[0] with
      | 'A' .. 'Z' | 'a' .. 'z' -> Ok name
      | _ -> Fmt.error_msg "Cache ID %S should start with [A-Za-z]" name
    ) else Fmt.error_msg "Invalid character in cache ID %S (should be [-._A-Za-z0-9])" name

(* Convert fields matched by [p] from (name v1 v2 ...) to (name (v1 v2 ...)) *)
let inflate_record p =
  let open Sexplib.Sexp in function
  | Atom _ as x -> Fmt.failwith "Invalid record field: %a" Sexplib.Sexp.pp_hum x
  | List xs ->
    let expand = function
      | List (Atom name :: vs) when p name -> List [Atom name; List vs]
      | x -> x
    in
    List (List.map expand xs)

(* Convert fields matched by [p] from (name (v1 v2 ...)) to (name v1 v2 ...) *)
let deflate_record p =
  let open Sexplib.Sexp in function
  | Atom _ as x -> Fmt.failwith "Invalid record field: %a" Sexplib.Sexp.pp_hum x
  | List xs ->
    let deflate = function
      | List [Atom name; List vs] when p name -> List (Atom name :: vs)
      | x -> x
    in
    List (List.map deflate xs)

type copy = {
  src : string list;
  dst : string;
  exclude : string list [@sexp.list];
} [@@deriving sexp]

let copy_inlined = function
  | "src" | "exclude" -> true
  | _ -> false

let copy_of_sexp x = copy_of_sexp (inflate_record copy_inlined x)
let sexp_of_copy x = deflate_record copy_inlined (sexp_of_copy x)

type user = { uid : int; gid : int }
[@@deriving sexp]

type cache = {
  id : string;
  target : string;
} [@@deriving sexp]

let cache_of_sexp x =
  let open Sexplib.Sexp in
  match x with
  | List (Atom id :: fields) -> cache_of_sexp (List (List [Atom "id"; Atom id] :: fields))
  | x -> Fmt.failwith "Invalid cache: %a" Sexplib.Sexp.pp_hum x

let sexp_of_cache x =
  let open Sexplib.Sexp in
  match sexp_of_cache x with
  | List (List [Atom "id"; Atom id] :: fields) -> List (Atom id :: fields)
  | x -> Fmt.failwith "Invalid cache: %a" Sexplib.Sexp.pp_hum x

type run = {
  cache : cache list [@sexp.list];
  shell : string;
}
[@@deriving sexp]

let run_inlined = function
  | "cache" -> true
  | _ -> false

let run_of_sexp x = run_of_sexp (inflate_record run_inlined x)
let sexp_of_run x = deflate_record run_inlined (sexp_of_run x)

type op = [
  | `Comment of string
  | `Workdir of string
  | `Shell of string list [@sexp.list]
  | `Run of run
  | `Copy of copy
  | `User of user
  | `Env of (string * string)
] [@@deriving sexp]

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
}

let sexp_of_stage { from; ops } =
  let open Sexplib.Sexp in
  List (List [ Atom "from"; Atom from ] :: List.map sexp_of_op ops)

let stage_of_sexp = function
  | Sexplib.Sexp.List (List [ Atom "from"; Atom from ] :: ops) ->
    { from; ops = List.map op_of_sexp ops }
  | x -> Fmt.failwith "Invalid stage: %a" Sexplib.Sexp.pp_hum x

let comment c = `Comment c
let workdir x = `Workdir x
let run ?(cache=[]) x = `Run { shell = x; cache }
let copy ?(exclude=[]) src dst = `Copy { src; dst; exclude }
let env k v = `Env (k, v)
let user ~uid ~gid = `User { uid; gid }

let root = { uid = 0; gid = 0 }
