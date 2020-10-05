module Mount = struct
  type t = { (* TODO: options *)
    src : string;              (* In host namespace *)
    dst : string;              (* In container namespace *)
  }
end

type t = {
  cwd : string;
  argv : string list;
  hostname : string;
  user : Obuilder_spec.user;
  env : Os.env;
  mounts : Mount.t list;
}

let v ~cwd ~argv ~hostname ~user ~env ~mounts =
  { cwd; argv; hostname; user; env; mounts }
