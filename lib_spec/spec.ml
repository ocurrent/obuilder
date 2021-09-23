open Sexplib.Std

module Scope = Set.Make(String)         (* Nested builds in scope *)

type sexp = Sexplib.Sexp.t =
  | Atom of string
  | List of sexp list

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

type data_source = [
  | `Context
  | `Build of string
]

let sexp_of_data_source = function
  | `Context -> Atom "context"
  | `Build name -> List [Atom "build"; Atom name]

let data_source_of_sexp = function
  | Atom "context" -> `Context
  | List [Atom "build"; Atom name] -> `Build name
  | x -> Fmt.failwith "Invalid data source: %a" Sexplib.Sexp.pp_hum x

type copy = {
  from : data_source [@default `Context] [@sexp_drop_default (=)];
  src : string list;
  dst : string;
  exclude : string list [@sexp.list];
} [@@deriving sexp]

let copy_inlined = function
  | "src" | "exclude" -> true
  | _ -> false

let copy_of_sexp x = copy_of_sexp (inflate_record copy_inlined x)
let sexp_of_copy x = deflate_record copy_inlined (sexp_of_copy x)

type unix_user = {
  uid : int;
  gid : int;
} [@@deriving sexp]

type windows_user = {
  name : string;
} [@@deriving sexp]

type user = [
  | `Unix of unix_user
  | `Windows of windows_user
] [@@deriving sexp]

let user_of_sexp x =
  let open Sexplib.Sexp in
  match x with
  | List [List [Atom "name"; _]] ->
    `Windows (windows_user_of_sexp x)
  | List [List [Atom "uid"; _]; List [Atom "gid"; _]] ->
    `Unix (unix_user_of_sexp x)
  | x -> Fmt.failwith "Invalid op: %a" Sexplib.Sexp.pp_hum x

let sexp_of_user x : Sexplib.Sexp.t =
  let x = sexp_of_user x in
  match x with
  | List [Atom _os; List args] -> List args
  | x -> Fmt.failwith "Invalid op: %a" Sexplib.Sexp.pp_hum x

type run = {
  cache : Cache.t list [@sexp.list];
  network : string list [@sexp.list];
  secrets : Secret.t list [@sexp.list];
  shell : string;
} [@@deriving sexp]

let run_inlined = function
  | "cache" | "network" | "secrets" -> true
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
    List (Atom name :: args)
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

type t = {
  child_builds : (string * t) list;
  from : string;
  ops : op list;
}

let rec sexp_of_t { child_builds; from; ops } =
  let child_builds =
    child_builds |> List.map (fun (name, spec) ->
        List [ Atom "build"; Atom name; sexp_of_t spec ]
      )
  in
  List (child_builds @ List [ Atom "from"; Atom from ] :: List.map sexp_of_op ops)

let rec t_of_sexp = function
  | Atom _ as x -> Fmt.failwith "Invalid spec: %a" Sexplib.Sexp.pp_hum x
  | List items ->
    let rec aux acc = function
      | List [ Atom "build"; Atom name; child_spec ] :: xs ->
        let child = (name, t_of_sexp child_spec) in
        aux (child :: acc) xs
      | List [ Atom "from"; Atom from ] :: ops ->
        let child_builds = List.rev acc in
        { child_builds; from; ops = List.map op_of_sexp ops }
      | x :: _ -> Fmt.failwith "Invalid spec item: %a" Sexplib.Sexp.pp_hum x
      | [] -> Fmt.failwith "Invalid spec: missing (from)"
    in
    aux [] items

let comment fmt = fmt |> Printf.ksprintf (fun c -> `Comment c)
let workdir x = `Workdir x
let shell xs = `Shell xs
let run ?(cache=[]) ?(network=[]) ?(secrets=[]) fmt = fmt |> Printf.ksprintf (fun x -> `Run { shell = x; cache; network; secrets })
let copy ?(from=`Context) ?(exclude=[]) src ~dst = `Copy { from; src; dst; exclude }
let env k v = `Env (k, v)
let user_unix ~uid ~gid = `User (`Unix { uid; gid })
let user_windows ~name = `User (`Windows { name })

let root_unix = `Unix { uid = 0; gid = 0 }
let root_windows = `Windows { name = "ContainerAdministrator" }
let root = if Sys.win32 then root_windows else root_unix

let rec pp_no_boxes f : Sexplib.Sexp.t -> unit = function
  | List xs -> Fmt.pf f "(%a)" (Fmt.list ~sep:Fmt.sp pp_no_boxes) xs
  | Atom _ as a -> Sexplib.Sexp.pp_hum f a

let pp_one_line = Fmt.hbox pp_no_boxes

let rec pp_op_sexp f : Sexplib.Sexp.t -> unit = function
  | List [(Atom "build" as op); (Atom _ as name); List ops] ->
    Fmt.pf f "(%a @[<v>%a@,(@[<v>%a@])@])"
      Sexplib.Sexp.pp_hum op
      Sexplib.Sexp.pp_hum name
      (Fmt.list ~sep:Fmt.cut pp_op_sexp) ops
  | List (Atom "copy" as op :: args) ->
    Fmt.pf f "(%a @[<hv>%a@])"
      Sexplib.Sexp.pp_hum op
      (Fmt.list ~sep:Fmt.sp pp_one_line) args
  | List (Atom ("run") as op :: args) ->
    Fmt.pf f "(%a @[<v>%a@])"
      Sexplib.Sexp.pp_hum op
      (Fmt.list ~sep:Fmt.cut pp_one_line) args
  | x -> pp_one_line f x

let pp f t =
  match sexp_of_t t with
  | List lines ->
    Fmt.pf f "(@[<v>%a@]@,)" (Fmt.list ~sep:Fmt.cut pp_op_sexp) lines
  | x -> Sexplib.Sexp.pp_hum f x

let pp_op = Fmt.using sexp_of_op pp_op_sexp

let rec validate ?(scope=Scope.empty) { child_builds; from = _; ops } =
  let scope =
    List.fold_left (fun scope (name, spec) ->
        validate ~scope spec;
        Scope.add name scope
      ) scope child_builds in
  ops |> List.iter (function
      | `Copy { from = `Build name; src = _; _ } as copy ->
        if not (Scope.mem name scope) then (
          let hints = Scope.elements scope in
          let post f () = Fmt.pf f " in %a" pp_op copy in
          Fmt.failwith "%a"
            Fmt.(did_you_mean ~kind:"build" ~post (quote string)) (name, hints)
        )
      | _ -> ()
    )

let stage ?(child_builds=[]) ~from ops =
  let t = { child_builds; from; ops } in
  validate t;
  t

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  validate t;
  t
