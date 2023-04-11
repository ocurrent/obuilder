module OV = Ocaml_version
module D = Dockerfile_opam.Distro

type win10_release = D.win10_release [@@deriving sexp]
type win10_ltsc = D.win10_ltsc [@@deriving sexp]
type win_all = D.win_all [@@deriving sexp]
type win10_lcu = D.win10_lcu [@@deriving sexp]

let win10_current_lcu = D.win10_current_lcu

type win10_revision = D.win10_revision [@@deriving sexp]
type distro = [ D.distro | `Macos of [ `V12 | `V13 ] ] [@@deriving sexp]
type t = [ D.t | `Macos of [ `Latest | `V12 | `V13 ] ] [@@deriving sexp]
type os_family = [ D.os_family | `Macos ] [@@deriving sexp]

let os_family_of_distro (t : t) : os_family =
  match t with
  | #D.t as f -> (D.os_family_of_distro f :> os_family)
  | `Macos _ -> `Macos

let os_family_to_string (os : os_family) =
  match os with
  | #D.os_family as os -> D.os_family_to_string os
  | `Macos -> "macos"

let opam_repository (os : os_family) =
  match os with
  | #D.os_family as os -> D.opam_repository os
  | `Macos -> "https://github.com/ocaml/opam-repository.git"

let personality os_family arch =
  match os_family with
  | #D.os_family as os_family -> D.personality os_family arch
  | `Macos -> None

type status =
  [ `Deprecated
  | `Active of [ `Tier1 | `Tier2 | `Tier3 ]
  | `Alias
  | `Not_available ]
[@@deriving sexp]

let macos_distros = [ `Macos `V12; `Macos `V13 ]
let distros = (D.distros :> t list) @ macos_distros

type win10_release_status = D.win10_release_status

let win10_release_status = D.win10_release_status

type win10_docker_base_image = D.win10_docker_base_image

let win10_latest_image = D.win10_latest_image

let resolve_alias (d : t) : distro =
  match d with
  | #D.t as d -> (D.resolve_alias d :> distro)
  | `Macos (`Latest | `V13) -> `Macos `V13
  | `Macos `V12 -> `Macos `V12

let distro_status_extended (d : t) : status =
  let resolved = resolve_alias d in
  if (resolved : distro :> t) <> d then `Alias else `Active `Tier2

let latest_distros = (D.latest_distros :> t list) @ [ `Macos `Latest ]

let win10_latest_release = D.win10_latest_release
let master_distro = (D.master_distro :> t)

let distro_arches ov (d : t) =
  match d with
  | #D.t as d -> D.distro_arches ov d
  | `Macos _ when OV.(compare Releases.v4_02_0 ov) = -1 ->
      [ `X86_64; `Aarch64 ]
  | _ -> [ `X86_64 ]

let distro_supported_on a ov (d : t) = List.mem a (distro_arches ov d)

let distro_active_for arch (d : t) =
  match (arch, d) with
  | `X86_64, `Windows _ -> true
  | _ -> distro_supported_on arch OV.Releases.latest d

let active_distros arch =
  let extended_distros =
    List.filter
      (fun d ->
        match distro_status_extended d with
        | `Active _ -> true
        | _ -> false)
    macos_distros
    |> List.filter (distro_active_for arch)
  in
  (D.active_distros arch :> t list) @ extended_distros

let active_tier1_distros arch =
  let extended_distros =
    List.filter
      (fun d ->
        match distro_status_extended d with
        | `Active `Tier1 -> true
        | _ -> false)
      macos_distros
    |> List.filter (distro_active_for arch)
  in
  (D.active_tier1_distros arch :> t list) @ extended_distros

let active_tier2_distros arch =
  let extended_distros =
    List.filter
      (fun d ->
        match distro_status_extended d with
        | `Active `Tier2 -> true
        | _ -> false)
      macos_distros
    |> List.filter (distro_active_for arch)
  in
  (D.active_tier2_distros arch :> t list) @ extended_distros

let active_tier3_distros arch =
  let extended_distros =
    List.filter
      (fun d ->
        match distro_status_extended d with
        | `Active `Tier3 -> true
        | _ -> false)
      macos_distros
    |> List.filter (distro_active_for arch)
  in
  (D.active_tier3_distros arch :> t list) @ extended_distros

let builtin_ocaml_of_distro (d : t) =
  match d with #D.t as d -> D.builtin_ocaml_of_distro d | `Macos _ -> None

let win10_release_to_string = D.win10_release_to_string

let win10_release_of_string = D.win10_release_of_string

let win10_revision_to_string = D.win10_revision_to_string

let win10_revision_of_string = D.win10_revision_of_string

let tag_of_distro (d : t) =
  match d with
  | #D.t as d -> D.tag_of_distro d
  | `Macos (`Latest | `V13) -> "macos-homebrew-13"
  | `Macos `V12 -> "macos-homebrew-12"

let distro_of_tag x =
  match D.distro_of_tag x with
  | None -> (
      match x with
      | "macos-homebrew-12" -> Some (`Macos `V12)
      | "macos-homebrew-13" -> Some (`Macos `V13)
      | "macos-homebrew" -> Some (`Macos `Latest)
      | _ -> None)
  | Some _ as x -> (x :> t option)

let human_readable_string_of_distro (d : t) =
  match d with
  | #D.t as d -> D.human_readable_string_of_distro d
  | `Macos _ as d -> (
      match resolve_alias d with
      | `Macos `V12 -> "MacOS 12 (Monterey)"
      | `Macos `V13 -> "MacOS 13 (Ventura)"
      | _ -> failwith "Resolved to non-MacOS distro, bug in [resolve_alias]?")

let human_readable_short_string_of_distro (d : t) =
  match d with
  | #D.t as d -> D.human_readable_short_string_of_distro d
  | `Macos _ -> "MacOS"

let is_same_distro (d1 : t) (d2 : t) =
  match (d1, d2) with
  | (#D.t as d1), (#D.t as d2) -> D.is_same_distro d1 d2
  | `Macos _, `Macos _ -> true
  | _ -> false

let latest_tag_of_distro (t : t) =
  let latest = List.find (is_same_distro t) latest_distros in
  tag_of_distro latest

type package_manager = [ D.package_manager | `Homebrew ] [@@deriving sexp]

let package_manager (d : t) : package_manager =
  match d with #D.t as d -> (D.package_manager d :> package_manager) | `Macos _ -> `Homebrew

let bubblewrap_version (d : t) =
  match d with #D.t as d -> D.bubblewrap_version d | `Macos _ -> None

let win10_base_tag = D.win10_base_tag

let base_distro_tag ?win10_revision ?(arch = `X86_64) (d : t) =
  match d with
  | #D.t as d -> D.base_distro_tag ?win10_revision ~arch d
  | `Macos _ as d -> (
      (* TODO There is no docker image for these yet! *)
      match resolve_alias d with
      | `Macos `V12 -> ("macos/monterey", "12")
      | `Macos `V13 -> ("macos/ventura", "13")
      | _ -> failwith "Resolved to non-MacOS distro, bug in [resolve_alias]?")

let compare a b =
  String.compare
    (human_readable_string_of_distro a)
    (human_readable_string_of_distro b)
