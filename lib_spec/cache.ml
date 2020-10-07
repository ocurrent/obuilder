open Sexplib.Std
open Sexplib.Sexp

type t = {
  id : string;
  target : string;
  buildkit_options : (string * string) list [@sexp.list];
} [@@deriving sexp]

let t_of_sexp x =
  match x with
  | List (Atom id :: fields) -> t_of_sexp (List (List [Atom "id"; Atom id] :: fields))
  | x -> Fmt.failwith "Invalid cache: %a" Sexplib.Sexp.pp_hum x

let sexp_of_t x =
  match sexp_of_t x with
  | List (List [Atom "id"; Atom id] :: fields) -> List (Atom id :: fields)
  | x -> Fmt.failwith "Invalid cache: %a" Sexplib.Sexp.pp_hum x

let v ?(buildkit_options=[]) ~target id = { id; target; buildkit_options }
