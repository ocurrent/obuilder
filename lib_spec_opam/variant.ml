module Ocaml_version = struct
  include Ocaml_version
  (* Extend Ocaml_version with json support. *)

  let arch_to_yojson arch = `String (string_of_arch arch)

  let arch_of_yojson j =
    match j with
    | `String a -> (
        match arch_of_string a with
        | Ok v -> Ok v
        | Error _ -> Error ("unknown arch " ^ a))
    | _ -> Error "unknown json type for arch"

  let compare_arch = Stdlib.compare
  let equal_arch = ( = )
  let to_yojson t = `String (to_string t)

  let of_yojson j =
    match j with
    | `String a -> (
        match of_string a with
        | Ok v -> Ok v
        | Error _ -> Error ("unknown ocaml version " ^ a))
    | _ -> Error "unknown json for ocaml version"
end

type t = {
  distro : string;
  ocaml_version : Ocaml_version.t;
  arch : Ocaml_version.arch;
  opam_version : Opam_version.t;
}
[@@deriving yojson, ord, eq]

let macos_distributions = [ "macos-homebrew" ]

let os { distro; _ } =
  if List.exists (String.equal distro) macos_distributions then `Macos
  else `Linux

let v ~arch ~distro ~ocaml_version ~opam_version =
  (* Just check we understand all the variants first *)
  match Ocaml_version.Configure_options.of_t ocaml_version with
  | Ok _ -> Ok { arch; distro; ocaml_version; opam_version }
  | Error e -> Error e

let arch t = t.arch
let distro_str t = t.distro
let distro t = Distro.distro_of_tag t.distro |> Option.get
let ocaml_version t = t.ocaml_version
let opam_version t = t.opam_version

let id t =
  Fmt.str "%s-%a" t.distro Ocaml_version.pp t.ocaml_version

let docker_tag t =
  Fmt.str "%s-ocaml-%s" t.distro
    (Ocaml_version.to_string ~sep:'-' t.ocaml_version
    |> String.map (function '+' -> '-' | x -> x))

let id_of_string s =
  match Astring.String.cut ~rev:true ~sep:"-" s with
  | Some (distro, ov) -> Some (distro, Ocaml_version.of_string_exn ov)
  | None -> None

let pp f t =
  let arch =
    match t.arch with
    | `X86_64 -> ""
    | a -> "_" ^ Ocaml_version.to_opam_arch a
  in
  let opam_and_os_family =
    match t.opam_version with
    | `V2_0 -> ""
    | v ->
      match Distro.distro_of_tag t.distro with
      | None -> failwith ("internal error: unknown distro " ^ t.distro)
      | Some distro ->
        "_opam-" ^ Opam_version.to_string (Distro.os_family_of_distro distro) v
  in
  Fmt.pf f "%s%s%s" (id t) arch opam_and_os_family

let to_string = Fmt.str "%a" pp
let err_variant id = failwith ("internal error: unknown variant " ^ id)
let err_arch a = failwith ("internal error: unknown arch " ^ a)
let err_opam_version id = failwith ("internal error: unknown opam version " ^ id)

let arch_of_string a =
  match Ocaml_version.of_opam_arch a with Some a -> a | None -> err_arch a

let opam_version_of_string v =
  match Opam_version.of_string v with
  | Ok v -> v
  | Error _ -> err_opam_version v

let of_string ?(default=`V2_0) s =
  let s, opam_version =
    match Astring.String.cut ~rev:true ~sep:"_opam-" s with
    | None -> (s, default)
    | Some (s, v) -> (s, opam_version_of_string v)
  in
  let id, arch =
    match Astring.String.cut ~sep:"_" s with
    | None -> (s, `X86_64)
    | Some (id, a) -> (id, arch_of_string a)
  in
  match id_of_string id with
  | None -> err_variant id
  | Some (distro, ocaml_version) ->
      { arch; distro; ocaml_version; opam_version }
