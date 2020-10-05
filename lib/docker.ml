open Dockerfile

type ctx = {
  user : Spec.user;
}

let default_ctx = {
  user = Spec.root;
}

let of_op (acc, ctx) : Spec.op -> Dockerfile.t list * ctx = function
  | `Comment x -> comment "%s" x :: acc, ctx
  | `Workdir x -> workdir "%s" x :: acc, ctx
  | `Shell xs -> shell xs :: acc, ctx
  | `Run { cache = _; shell } -> run "%s" shell :: acc, ctx
  | `Copy { src; dst; exclude = _ } ->
    if ctx.user = Spec.root then copy ~src ~dst () :: acc, ctx
    else (
      let { Spec.uid; gid } = ctx.user in
      let chown = Printf.sprintf "%d:%d" uid gid in
      copy ~chown ~src ~dst () :: acc, ctx
    )
  | `User ({ uid; gid } as u) -> user "%d:%d" uid gid :: acc, { user = u }
  | `Env b -> env [b] :: acc, ctx

let dockerfile_of_spec { Spec.from; ops } =
  let ops', _ctx = List.fold_left of_op ([], default_ctx) ops in
  Dockerfile.from from @@@ List.rev ops'
