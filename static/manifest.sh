# shellcheck shell=bash

# An implementation of the Manifest module in bash, to run inside
# Docker containers. Outputs a list of S-expressions representing a
# sequence of {Manifest.t}.

# Depends on bash, basename, readlink, sha256sum.

set -e

shopt -s nullglob

# https://stackoverflow.com/a/8574392
function mem () {
    local e match="$1"
    shift
    for e; do [[ "$e" == "$match" ]] && return 0; done
    return 1
}

function generate() {
    local src=$1
    local path hash target

    path=$(printf '%s%s%s' "$src_dir" "$dir_sep" "$src")
    if [[ -d "$path" ]]; then
        printf '(Dir ("%s" (' "$src"
        for item in "$path"/*; do       # Let's hope Bash file iteration is stable.
            if ! item=$(basename "$item"); then return 1; fi
            printf "path:%s item:%s\n" "$path" "$item" 1>&2
            if ! mem "$item" "${exclude[@]}"; then
                if ! generate "$(printf '%s%s%s' "$src" "$dir_sep" "$item")"; then
                    return 1
                fi
            fi
        done
        printf ')))'
    elif [[ -L "$path" ]]; then
        if ! target=$(readlink "$path"); then return 1; fi
        printf '(Symlink ("%s" %s))' "$src" "$target"
    elif [[ -f "$path" ]]; then
        if ! hash=$(sha256sum "$path" | head -c 64); then return 1; fi
        printf '(File ("%s" %s))' "$src" "$hash"
    elif [[ ! -e "$path" ]]; then
        printf 'manifest.sh: file %s not found in source directory\n' "$src" 1>&2
        return 1
    else
        printf 'manifest.sh: unsupported file type for %s\n' "$src" 1>&2
        return 1
    fi
}

function check_path() {
    local acc=$1; shift
    local base=$1; shift
    local segs=( "$@" )
    local x path
    local -a xs

    x=${segs[0]}
    xs=("${segs[@]:1}")

    if [[ ${#segs[@]} -eq 0 ]]; then
        printf '%s' "$acc"
        return 0
    elif [[ "$x" = "" || "$x" = "." ]]; then
        check_path "$acc" "$base" "${xs[@]}"
    elif [[ "$x" == *"$dir_sep"* ]]; then
        printf "manifest.sh: can't use platform directory separator in path component: %s\n" "$x" 1>&2
        return 1
    else
        path=$(printf '%s%s%s' "$base" "$dir_sep" "$x")
        acc=$(printf '%s%s%s' "$acc" "$dir_sep" "$x")
        if [[ ! -e "$path" ]]; then
            printf 'manifest.sh: not found: %s\n' "$path" 1>&2
            return 1
        elif [[ -d "$path" ]]; then
            check_path "$acc" "$path" "${xs[@]}"
        elif [[ (-f "$path" || -L "$path") && ${#xs[@]} -eq 0 ]]; then
            printf '%s' "$acc"
            return 0
        elif [[ -f "$path" ]]; then
            printf 'manifest.sh: not a directory: %s\n' "$acc" 1>&2
            return 1
        else
            printf 'manifest.sh: not a regular file: %s\n' "$acc" 1>&2
            return 1
        fi
    fi
}

function main() {
    local src
    local -i exclude_length src_length
    local -a srcs

    exclude_length=$1; shift
    while (( exclude_length-- > 0 )); do
        exclude+=( "$1" ); shift
    done
    src_length=$1; shift
    while (( src_length-- > 0 )); do
        srcs+=( "$1" ); shift
    done

    for src in "${srcs[@]}"; do
        IFS='/' read -r -a segs <<< "$src"
        if src=$(check_path "" "$src_dir" "${segs[@]}"); then
            if ! generate "$src"; then
                return 1
            fi
        else
            return 1
        fi
    done
}

src_dir=$1; shift
dir_sep=$1; shift
declare -a exclude

main "$@"
