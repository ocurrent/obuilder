(** Configuration for a single sandboxed build step.
    This is passed by the builder to the sandbox. *)

open Sexplib.Std

type env = (string * string) list [@@deriving sexp]

module Mount = struct
  type t = { (* TODO: options *)
    src : string;              (* In host namespace *)
    dst : string;              (* In container namespace *)
    readonly : bool;
  }
end

module Secret = struct
  type t = {
    value: string;
    target: string;
  } [@@deriving sexp]
end

type t = {
  cwd : string;
  entrypoint : string option;
  argv : string list;
  hostname : string;
  user : Obuilder_spec.user;
  env : env;
  mounts : Mount.t list;
  network : string list;
  mount_secrets : Secret.t list;
}

let v ~cwd ~argv ~hostname ~user ~env ~mounts ~network ~mount_secrets ?entrypoint () =
  { cwd; argv; hostname; user; env; mounts; network; mount_secrets; entrypoint; }
