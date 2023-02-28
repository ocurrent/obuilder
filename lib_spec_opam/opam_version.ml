type t = [ `V2_0 | `V2_1 | `Dev ] [@@deriving ord, yojson, eq]

let to_string os = function
  | `V2_0 -> "2.0"
  | `V2_1 -> "2.1"
  | `Dev ->
    match os with
    | `Macos -> "2.1" (* TODO: Remove that when macOS has a proper up-to-date docker image *)
    | `Linux -> "dev"
    | `Windows | `Cygwin -> failwith "Windows native and Cygwin are not yet supported"

let to_string_with_patch os = function
  | `V2_0 -> "2.0.10"
  | `V2_1 -> "2.1.2"
  | `Dev ->
    match os with
    | `Macos -> "2.1.2" (* TODO: Remove that when macOS has a proper up-to-date docker image *)
    | `Linux -> "dev"
    | `Windows | `Cygwin -> failwith "Windows native and Cygwin are not yet supported"

let pp os = Fmt.of_to_string (to_string os)

let of_string = function
  | "2.0" -> Ok `V2_0
  | "2.1" -> Ok `V2_1
  | "dev" -> Ok `Dev
  | s -> Error (`Msg (s ^ ": invalid opam version"))
