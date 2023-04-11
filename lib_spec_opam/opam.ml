open Obuilder_spec

let download_cache = "opam-archives"

let opam_init ?(opamrc="") version distro =
  let os_family = Distro.os_family_of_distro distro in
  let version = Opam_version.to_string os_family version in
  let prefix =
    match os_family with
    | `Linux -> "/usr"
    | `Macos -> "~/local"
    | `Windows | `Cygwin -> failwith "Windows native and Cygwin are not yet supported"
  in
  let ln =
    match os_family with
    | `Linux -> "sudo ln"
    | `Macos -> "ln"
    | `Windows | `Cygwin -> failwith "Windows native and Cygwin are not yet supported"
  in
  (* Add space between `reinit` and `opamrc` if necessary;
     needed for spec-file generation tests to pass in OCaml-CI *)
  let opamrc =
    if String.length opamrc > 0 && String.get opamrc 0 != ' ' then
      " " ^ opamrc
    else
      opamrc
  in
  [
    run "%s -f %s/bin/opam-%s %s/bin/opam" ln prefix version prefix;
    run "opam init --reinit%s -ni" opamrc;
  ]

let caches ?(extra_caches = []) distro =
  let main_caches =
    match Distro.os_family_of_distro distro with
    | `Linux ->
        [
          Obuilder_spec.Cache.v download_cache
            ~target:"/home/opam/.opam/download-cache";
        ]
    | `Macos ->
        [
          Obuilder_spec.Cache.v download_cache
            ~target:"/Users/mac1000/.opam/download-cache";
        ]
    | `Windows | `Cygwin -> failwith "Windows native and Cygwin are not yet supported"
  in
  main_caches @ (List.map (fun (name, target) -> Obuilder_spec.Cache.v name ~target) extra_caches)

let set_personality arch =
  if Ocaml_version.arch_is_32bit arch then
    [ shell [ "/usr/bin/linux32"; "/bin/sh"; "-c" ] ]
  else []
