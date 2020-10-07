open Dockerfile

type ctx = {
  user : Spec.user;
}

let default_ctx = {
  user = Spec.root;
}

(* Note: could do with some escaping here, but the rules are not clear. *)
let pp_pair f (k, v) =
  Fmt.pf f "%s=%s" k v

let of_op ~buildkit (acc, ctx) : Spec.op -> Dockerfile.t list * ctx = function
  | `Comment x -> comment "%s" x :: acc, ctx
  | `Workdir x -> workdir "%s" x :: acc, ctx
  | `Shell xs -> shell xs :: acc, ctx
  | `Run { cache = (_ :: _) as cache; shell } when buildkit ->
    let mounts =
      cache |> List.map (fun { Cache.id; target; buildkit_options } ->
          let buildkit_options =
            ("--mount=type", "cache") ::
            ("id", id) ::
            ("target", target) ::
            ("uid", string_of_int ctx.user.uid) ::
            buildkit_options
          in
          Fmt.strf "@[<h>%a@]" Fmt.(list ~sep:(unit ",") pp_pair) buildkit_options
        )
    in
    run "%s %s" (String.concat " " mounts) shell :: acc, ctx
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

let dockerfile_of_spec ~buildkit { Spec.from; ops } =
  let ops', _ctx = List.fold_left (of_op ~buildkit) ([], default_ctx) ops in
  Dockerfile.from from @@@ List.rev ops'
