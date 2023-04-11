(** Distribution selections for various OPAM combinations, for Linux, Windows, and MacOS distributions *)

module Basic = Dockerfile_opam.Distro

open Dockerfile_opam

type win10_release = Distro.win10_release [@@deriving sexp]

type win10_ltsc = Distro.win10_ltsc [@@deriving sexp]

type win_all = Distro.win_all [@@deriving sexp]

type win10_lcu = Distro.win10_lcu [@@deriving sexp]

val win10_current_lcu : win10_lcu

type win10_revision = Distro.win10_revision [@@deriving sexp]

type distro = [ Distro.distro | `Macos of [ `V12 | `V13 ] ] [@@deriving sexp]
(** Supported Docker container distributions without aliases. *)

type t = [ Distro.t | `Macos of [ `Latest | `V12 | `V13 ] ] [@@deriving sexp]
(** Supported Docker container distributions with aliases. *)

type os_family = [ Distro.os_family | `Macos ] [@@deriving sexp]
(** The operating system family a distro belongs to. *)

val os_family_of_distro : t -> os_family
(** [os_family_of_distro t] returns the OS family of the distro. *)

val os_family_to_string : os_family -> string
(** [os_family_to_string os] returns a string representing the OS
    family. *)

val opam_repository : os_family -> string
(** [opam_repository os_family] returns the git URL to the default
    Opam repository. *)

val personality : os_family -> Ocaml_version.arch -> string option
(** [personality os_family arch] returns the personality associated to
   the architecture, if [os_family] is [`Linux]. *)

val is_same_distro : t -> t -> bool
(** [is_same_distro d1 d2] returns whether [d1] is the same distro as
    [d2], regardless of their respective versions. *)

val compare : t -> t -> int
(** [compare a b] is a lexical comparison function for {!t}. *)

val resolve_alias : t -> distro
(** [resolve_alias t] will resolve [t] into a concrete version. This removes
    versions such as [Latest]. *)

val distros : t list
(** Enumeration of the supported Docker container distributions. *)

val latest_distros : t list
(** Enumeration of the latest stable (ideally LTS) supported distributions. *)

val win10_latest_release : win10_release
(** Latest Windows 10 release. *)

val win10_latest_image : win10_release
(** Latest Windows 10 Docker image available. May differ from
   {!win10_latest_release} if the Docker repository hasn't been
   updated. *)

val master_distro : t
(** The distribution that is the top-level alias for the [latest] tag
    in the [ocaml/opam2] Docker Hub build. *)

val builtin_ocaml_of_distro : t -> string option
(** [builtin_ocaml_of_distro t] will return the OCaml version
    supplied with the distribution packaging, and [None] if there
    is no supported version. *)

val human_readable_string_of_distro : t -> string
(** [human_readable_string_of_distro t] returns a human readable
    version of the distribution tag, including version information. *)

val human_readable_short_string_of_distro : t -> string
(** [human_readable_short_string_of_distro t] returns a human readable
    short version of the distribution tag, excluding version information. *)

type package_manager =
  [ Distro.package_manager | `Homebrew  (** MacOS homebrew *) ]
[@@deriving sexp]
(** The package manager used by a distro. *)

val package_manager : t -> package_manager
(** [package_manager t] returns the type of package manager used
    by that distribution.  Many derived distributions (such as OracleLinux)
    share the same package manager from a base distribution (such as CentOS). *)

val bubblewrap_version : t -> (int * int * int) option
(** [bubblewrap_version t] returns the version of bubblewrap available on that
    distribution. *)

val tag_of_distro : t -> string
(** [tag_of_distro t] convert a distribution [t] to a Docker Hub tag. *)

val distro_of_tag : string -> t option
(** [distro_of_tag s] parses [s] into a {!t} distribution, and
    [None] otherwise. *)

val latest_tag_of_distro : t -> string
(** [latest_tag_of_distro distro] will generate a Docker Hub
    tag that is a convenient short form for the latest stable
    release of a particular distribution.  This tag will be
    regularly rewritten to point to any new releases of the
    distribution. *)

type win10_docker_base_image = Distro.win10_docker_base_image
(** Windows containers base images.
    @see <https://docs.microsoft.com/en-us/virtualization/windowscontainers/manage-containers/container-base-images> *)

val win10_base_tag :
  ?win10_revision:win10_lcu ->
  win10_docker_base_image ->
  win_all ->
  string * string
(** [win10_base_tag base_image release] will return a tuple of Windows
    container base image and tag for which the base image of a Windows
    base image can be found (e.g.
    [mcr.microsoft.com/windows/servercore],[ltsc2022] which maps to
    [mcr.microsoft.com/windows/servercore:ltsc2022] on the Microsoft
    Container Registry). *)

val base_distro_tag :
  ?win10_revision:Distro.win10_lcu ->
  ?arch:Ocaml_version.arch ->
  t ->
  string * string
(** [base_distro_tag ?arch t] will return a tuple of a Docker Hub
    user/repository and tag for which the base image of a distribution
    can be found (e.g. [opensuse/leap],[15.0] which maps to [opensuse/leap:15.0]
    on the Docker Hub).  This base image is in turn can be used to generate opam
    and other OCaml tool Dockerfiles. [arch] defaults to [x86_64] and can vary
    the base user/repository since some architecture are built elsewhere. *)

val win10_release_to_string : win10_release -> string
(** [win10_release_to_string update] converts a Windows 10 version name to
   string. *)

val win10_release_of_string : string -> win_all option
(** [win10_release_of_string] converts a Windows 10 version name as
   string to its internal representation. Ignores any KB number. *)

val win10_revision_to_string : win10_revision -> string
val win10_revision_of_string : string -> win10_revision option

val distro_arches : Ocaml_version.t -> t -> Ocaml_version.arch list
(** [distro_arches ov t] returns the list of architectures that
    distribution [t] is supported on for OCaml compiler version [ov] *)

val distro_supported_on : Ocaml_version.arch -> Ocaml_version.t -> t -> bool
(** [distro_supported_on arch ov distro] returns [true] if the
    combination of CPU [arch], compiler version [ov] is available
    on the distribution [distro]. *)

type win10_release_status = Distro.win10_release_status
(** Windows 10 release status. *)

val win10_release_status : win_all -> win10_release_status
(** [win10_release_status v channel] returns the Microsoft support
   status of the specified Windows 10 release.
   @see <https://en.wikipedia.org/wiki/Windows_10_version_history#Channels> *)

val active_distros : Ocaml_version.arch -> t list
(** [active_distros arch] returns the list of currently supported
    distributions in the opam build infrastructure.  Distributions
    that are end-of-life upstream will rotate out of this list
    regularly. *)

val active_tier1_distros : Ocaml_version.arch -> t list
(** Tier 1 distributions are those supported for the full matrix
    of compiler versions in the opam build infrastructure.
    The {{:https://github.com/ocurrent/docker-base-images}Docker base images}
    will compile a base image for every OCaml version, so this
    list should be added to sparingly. *)

val active_tier2_distros : Ocaml_version.arch -> t list
(** Tier 2 distributions are those supported for a limited set
    of compiler versions in the opam build infrastructure.  The
    distros in this list are also tested for packages in the
    opam repository. *)

val active_tier3_distros : Ocaml_version.arch -> t list
(** Tier 3 distributions are those supported for a limited set
    of compiler versions in the opam build infrastructure.  While
    these distros will have base images compiled for them, they
    are not widely tested. Distros maybe here as they are on the
    way to being deprecated, or new and still experimental. *)
