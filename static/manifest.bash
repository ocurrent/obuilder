# An implementation of the Manifest module in bash, to run inside
# Docker containers. Outputs a list of S-expressions representing a
# sequence of {Manifest.t}.

# Depends on bash, basename, readlink, sha256sum.
# If running on Windows, also depends on cygpath.

shopt -s dotglob nullglob

# https://stackoverflow.com/a/8574392
function mem() {
    local e match="$1"
    shift
    for e; do [[ "$e" == "$match" ]] && return 0; done
    return 1
}

# Filename.concat
function concat() {
    local path=$1
    local dir_sep=$2
    local name=$3

    if [[ -z "$path" ]]; then
        printf "%s" "$name"
    else
        printf '%s%s%s' "$path" "$dir_sep" "$name"
    fi
}

# Cygwin's readlink outputs a Unix path, we prefer mixed paths.
function readlink_wrapper() {
    local path

    if [[ "$OS" = "Windows_NT" ]]; then
        if ! path="$(readlink -- "$1" | cygpath -m -f-)"; then
            return 1
        fi
    else
        if ! path="$(readlink -- "$1")"; then
            return 1
        fi
    fi
    printf "%s" "$path"
}

function generate() {
    local src=$1
    local path hash target

    path=$(concat "$src_dir" "$dir_sep" "$src")
    if [[ -L "$path" ]]; then
        if ! target=$(readlink_wrapper "$path"); then return 1; fi
        printf '(Symlink ("%s" %s))' "$src" "$target"
    elif [[ -d "$path" ]]; then
        printf '(Dir ("%s" (' "$src"
        for item in "$path"/*; do       # Let's hope Bash file iteration is stable.
            if ! item=$(basename -- "$item"); then return 1; fi
            if ! mem "$item" "${exclude[@]}"; then
                if ! generate "$(concat "$src" "$dir_sep" "$item")"; then
                    return 1
                fi
            fi
        done
        printf ')))'
    elif [[ -f "$path" ]]; then
        if ! hash=$(sha256sum -- "$path"); then return 1; fi
        printf '(File ("%s" %s))' "$src" "${hash:0:64}"
    elif [[ ! -e "$path" ]]; then
        printf 'File "%s" not found in source directory' "$src" 1>&2
        return 1
    else
        printf 'Unsupported file type for "%s"' "$src" 1>&2
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
    elif [[ "$x" == ".." ]]; then
        printf "Can't use .. in source paths!" 1>&2
        return 1
    elif [[ "$x" == *"$dir_sep"* ]]; then
        printf "Can't use platform directory separator in path component: %s" "$x" 1>&2
        return 1
    else
        path=$(concat "$base" "$dir_sep" "$x")
        if [[ -z "$acc" ]]; then
            acc="$x"
        else
            acc=$(concat "$acc" "$dir_sep" "$x")
        fi

        if [[ ! -e "$path" ]]; then
            return 2
        elif [[ -d "$path" && ! -L "$path" ]]; then
            check_path "$acc" "$path" "${xs[@]}"
        elif [[ (-f "$path" || -L "$path") && ${#xs[@]} -eq 0 ]]; then
            printf '%s' "$acc"
            return 0
        elif [[ -f "$path" ]]; then
            printf 'Not a directory: %s' "$acc" 1>&2
            return 1
        else
            printf 'Not a regular file: %s' "$x" 1>&2
            return 1
        fi
    fi
}

function main() {
    local src src2 src3
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

    for src1 in "${srcs[@]}"; do
        IFS='/' read -r -a segs <<< "$src1"
        src2=$(check_path "" "$src_dir" "${segs[@]}")
        ret=$?
        if [[ $ret -eq 1 ]]; then
            printf ' (in "%s")' "$src1" 1>&2
            return 1
        elif [[ $ret -eq 2 ]]; then
            src3="$(printf "$dir_sep%s" "${segs[@]}")"
            printf 'Source path "%s" not found' "${src3:1}" 1>&2
            return 1
        elif ! generate "$src2"; then
            return 1
        fi
    done
}

src_dir=$1; shift
dir_sep=$1; shift
declare -a exclude

main "$@"
