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

let wrap x =
  x
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> String.concat " \\\n    "

let of_op ~buildkit (acc, ctx) : Spec.op -> Dockerfile.t list * ctx = function
  | `Comment x -> comment "%s" x :: acc, ctx
  | `Workdir x -> workdir "%s" x :: acc, ctx
  | `Shell xs -> shell xs :: acc, ctx
  | `Run { cache = (_ :: _) as cache; shell; network = _ } when buildkit ->
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
    run "%s %s" (String.concat " " mounts) (wrap shell) :: acc, ctx
  | `Run { cache = _; network = _; shell } -> run "%s" (wrap shell) :: acc, ctx
  | `Copy { from; src; dst; exclude = _ } ->
    let from = match from with
      | `Build name -> Some name
      | `Context -> None
    in
    if ctx.user = Spec.root then copy ?from ~src ~dst () :: acc, ctx
    else (
      let { Spec.uid; gid } = ctx.user in
      let chown = Printf.sprintf "%d:%d" uid gid in
      copy ?from ~chown ~src ~dst () :: acc, ctx
    )
  | `User ({ uid; gid } as u) -> user "%d:%d" uid gid :: acc, { user = u }
  | `Env b -> env [b] :: acc, ctx

let rec convert ?name ~buildkit { Spec.child_builds; from; ops } =
  let stages = child_builds |> List.map (fun (name, spec) -> convert ~name ~buildkit spec) |> List.flatten in
  let ops', _ctx = List.fold_left (of_op ~buildkit) ([], default_ctx) ops in
  stages @ [Dockerfile.from ?alias:name from @@@ List.rev ops']

let dockerfile_of_spec ~buildkit t =
  Dockerfile.empty @@@ convert ~buildkit t
