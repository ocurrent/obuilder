type ctx = {
  user : Spec.user;
}

let default_ctx = {
  user = Spec.root;
}

(* Note: could do with some escaping here, but the rules are not clear. *)
let pp_pair f (k, v) =
  Fmt.pf f "%s=%s" k v

let pp_wrap =
  Fmt.using (String.split_on_char '\n')
    Fmt.(list ~sep:(unit " \\@\n    ") (using String.trim string))

let pp_cache ~ctx f { Cache.id; target; buildkit_options } =
  let buildkit_options =
    ("--mount=type", "cache") ::
    ("id", id) ::
    ("target", target) ::
    ("uid", string_of_int ctx.user.uid) ::
    buildkit_options
  in
  Fmt.pf f "%a" Fmt.(list ~sep:(unit ",") pp_pair) buildkit_options

let pp_mount_secret ~ctx f { Secret.id; target; buildkit_options } =
  let buildkit_options =
    ("--mount=type", "secret") ::
    ("id", id) ::
    ("target", target) ::
    ("uid", string_of_int ctx.user.uid) ::
    buildkit_options
  in
  Fmt.pf f "%a" Fmt.(list ~sep:(unit ",") pp_pair) buildkit_options

let pp_run ~ctx f { Spec.cache; shell; secrets; network = _ } =
  Fmt.pf f "RUN %a%a%a"
    Fmt.(list (pp_mount_secret ~ctx ++ const string " ")) secrets
    Fmt.(list (pp_cache ~ctx ++ const string " ")) cache
    pp_wrap shell

let pp_copy ~ctx f { Spec.from; src; dst; exclude = _ } =
  let from = match from with
    | `Build name -> Some name
    | `Context -> None
  in
  let chown =
    if ctx.user = Spec.root then None
    else (
      let { Spec.uid; gid } = ctx.user in
      Some (Printf.sprintf "%d:%d" uid gid)
    )
  in
  Fmt.pf f "COPY %a%a%a %s"
    Fmt.(option (fmt "--chown=%s ")) chown
    Fmt.(option (fmt "--from=%s ")) from
    Fmt.(list ~sep:sp string) src
    dst

let pp_op ~buildkit ctx f : Spec.op -> ctx = function
  | `Comment x                -> Fmt.pf f "# %s" x; ctx
  | `Workdir x                -> Fmt.pf f "WORKDIR %s" x; ctx
  | `Shell xs                 -> Fmt.pf f "SHELL [ %a ]" Fmt.(list ~sep:comma (quote string)) xs; ctx
  | `Run x when buildkit      -> pp_run ~ctx f x; ctx
  | `Run x                    -> pp_run ~ctx f { x with cache = []; secrets = []}; ctx
  | `Copy x                   -> pp_copy ~ctx f x; ctx
  | `User ({ uid; gid } as u) -> Fmt.pf f "USER %d:%d" uid gid; { user = u }
  | `Env (k, v)               -> Fmt.pf f "ENV %s %s" k v; ctx

let rec convert ~buildkit f (name, { Spec.child_builds; from; ops }) =
  child_builds |> List.iter (fun (name, spec) ->
      convert ~buildkit f (Some name, spec);
      Format.pp_print_newline f ();
    );
  Fmt.pf f "@[<h>FROM %s%a@]@." from Fmt.(option (const string " as " ++ string)) name;
  let (_ : ctx) = List.fold_left (fun ctx op ->
      Format.pp_open_hbox f ();
      let ctx = pp_op ~buildkit ctx f op in
      Format.pp_close_box f ();
      Format.pp_print_newline f ();
      ctx
    ) default_ctx ops
  in ()

let dockerfile_of_spec ~buildkit t =
  Fmt.strf "%a" (convert ~buildkit) (None, t)
