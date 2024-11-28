# OBuilder's QEMU Sandbox

This backend should work with any OS which can be booted in QEMU and
which can provide an SSH interface.

# Base Images

`obuilder` requires a base image aka a root file system, to use as
the basis of the container which it creates.  For example the `runc`
backend extracts the root file system from a Docker base image, while
a FreeBSD `jail` uses a FreeBSD installation on a ZFS volume.  `qemu`
requires virtual _hard disks_ with the operating system preinstalled.

In order to use the the [QEMU](https://www.qemu.org) backend with
`obuilder` base images need to be created. A [Makefile](../qemu/Makefile)
is provided which builds several base images:

- ubuntu-noble-amd64-ocaml-4.14.2.qcow2
- ubuntu-noble-amd64-ocaml-5.2.0.qcow2
- ubuntu-noble-riscv64-ocaml-4.14.2.qcow2
- ubuntu-noble-riscv64-ocaml-5.2.0.qcow2
- openbsd-67-amd64-ocaml-4.14.2.qcow2
- openbsd-67-amd64-ocaml-5.2.0.qcow2
- windows-server-2022-amd64-ocaml-4.14.2.qcow2
- windows-server-2022-amd64-ocaml-5.2.0.qcow2

The base images build are build using `make ubuntu`, `make windows` or
`make openbsd`.  The builds are unattended builds requiring no manual
intervention. Cloud Init is used on Ubuntu, `autounattend.xml` on Windows
and `autoinstall` on OpenBSD.

# Operation

A spec references the required base image using the `from` directive,
then runs whatever commands are required.  An trivial to install the
`tar` package from opam on Windows example is given below.

```
(
 (from windows-server-2022-amd64-ocaml-4.14.2)
 (run
  (run (cache (opam-archives (target "c:\\Users\\opam\\AppData\\local\\opam\\download-cache")))
  (shell "opam install tar")
 )
)
```

If this spec is saved in the file `test.spec` then a typical invocation
via `obuilder build` would be as below.

The base images should have been built already and moved to the
`base-image` folder below the folder specified by `--store`.
i.e. `/var/cache/obuilder/base-image/*.qcow2`.

```
obuilder build --store=qemu:/var/cache/obuilder -v -f test.spec --qemu-memory 16 --qemu-cpus 8 --qemu-guest-os windows .
```

The `from` directive causes `qemu-img` to create a snapshot of the base
image and stage it in the `result-tmp` folder.  When this completes
successfully, `result-tmp` is moved to `result`:

```
(from windows-server-2022-amd64-ocaml-4.14)
obuilder: [INFO] Base image not present; importing "windows-server-2022-amd64-ocaml-4.14"…
obuilder: [INFO] Exec "mkdir" "-m" "755" "--" "/var/cache/obuilder/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs"
obuilder: [INFO] Exec "qemu-img" "create" "-f" "qcow2" "-b" "/var/cache/obuilder/base-image/windows-server-2022-amd64-ocaml-4.14.qcow2" "-F" "qcow2" "/var/cache/obuilder/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2"
Formatting '/var/cache/obuilder/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2', fmt=qcow2 cluster_size=65536 extended_l2=off compression_type=zlib size=42949672960 backing_file=/var/cache/obuilder/base-image/windows-server-2022-amd64-ocaml-4.14.qcow2 backing_fmt=qcow2 lazy_refcounts=off refcount_bits=16
obuilder: [INFO] Exec "mv" "/var/cache/obuilder/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101" "/var/cache/obuilder/result/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101"
---> saved as “dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101”
```

Moving on to the next stage in the build which is the `run` directive.
First, `qemu-img` creates a snapshot of the current `result` layer into
`result-tmp`.  Then any cache volumes are copied and `qemu-system-x86_64`
is started with this snapshot as the base image and the cache volumes
available as extra disks.  `ssh` is used to poll the machine until it is
available.  Next, `ssh` commands are executed to create a NTFS junction
point on the directory `c:\Users\opam\AppData\Local\opam\download-cache`.
Finally, the actual commands are sent over `ssh` to install `tar`.
The step completes with an `scp` of the cache back to the host followed
by an ACPI shutdown command sent to the qemu console.

```
/: (run (cache (opam-archives (target "C:\\Users\\opam\\AppData\\Local\\opam\\download-cache")))
        (shell "opam install tar"))
obuilder: [INFO] Exec "qemu-img" "create" "-f" "qcow2" "-b" "/var/cache/obuilder/result/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2" "-F" "qcow2" "/var/cache/obuilder/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3/rootfs/image.qcow2" "40G"
obuilder: [INFO] Exec "cp" "-pRduT" "--reflink=auto" "/var/cache/obuilder/cache/c-opam-archives" "/var/cache/obuilder/cache-tmp/0-c-opam-archives"
obuilder: [INFO] Fork exec "qemu-system-x86_64" "-m" "16G" "-smp" "8" "-machine" "accel=kvm,type=q35" "-cpu" "host" "-nic" "user,hostfwd=tcp::56229-:22" "-display" "none" "-monitor" "stdio" "-drive" "file=/var/cache/obuilder/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3/rootfs/image.qcow2,format=qcow2" "-drive" "file=/var/cache/obuilder/cache-tmp/0-c-opam-archives/rootfs/image.qcow2,format=qcow2"
obuilder: [INFO] Exec "ssh" "opam@localhost" "-p" "56229" "-o" "NoHostAuthenticationForLocalhost=yes" "exit"
obuilder: [INFO] Exec "ssh" "opam@localhost" "-p" "56229" "-o" "NoHostAuthenticationForLocalhost=yes" "cmd" "/c" "rmdir /s /q 'C:\Users\opam\AppData\Local\opam\download-cache'"
obuilder: [INFO] Exec "ssh" "opam@localhost" "-p" "56229" "-o" "NoHostAuthenticationForLocalhost=yes" "cmd" "/c" "mklink /j 'C:\Users\opam\AppData\Local\opam\download-cache' 'd:\'"
Junction created for C:\Users\opam\AppData\Local\opam\download-cache <<===>> d:\
obuilder: [INFO] Fork exec "ssh" "opam@localhost" "-p" "56229" "-o" "NoHostAuthenticationForLocalhost=yes" "cd" "/" "&&" "opam install tar"
The following actions will be performed:
=== install 8 packages
  - install checkseum         0.5.2  [required by decompress]
  - install cmdliner          1.3.0  [required by decompress]
  - install csexp             1.5.2  [required by dune-configurator]
  - install decompress        1.5.3  [required by tar]
  - install dune              3.16.0 [required by tar]
  - install dune-configurator 3.16.0 [required by checkseum]
  - install optint            0.3.0  [required by decompress]
  - install tar               3.1.2

<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
-> retrieved checkseum.0.5.2  (cached)
-> retrieved cmdliner.1.3.0  (cached)
-> retrieved csexp.1.5.2  (cached)
-> retrieved decompress.1.5.3  (cached)
-> retrieved optint.0.3.0  (cached)
-> retrieved tar.3.1.2  (cached)
-> retrieved dune.3.16.0, dune-configurator.3.16.0  (cached)
-> installed cmdliner.1.3.0
-> installed dune.3.16.0
-> installed csexp.1.5.2
-> installed optint.0.3.0
-> installed dune-configurator.3.16.0
-> installed checkseum.0.5.2
-> installed decompress.1.5.3
-> installed tar.3.1.2
Done.
# Run eval $(opam env) to update the current shell environment
obuilder: [INFO] Exec "cp" "-pRduT" "--reflink=auto" "/var/cache/obuilder/cache-tmp/0-c-opam-archives" "/var/cache/obuilder/cache/c-opam-archives"
obuilder: [INFO] Exec "rm" "-r" "/var/cache/obuilder/cache-tmp/0-c-opam-archives"
obuilder: [INFO] Exec "mv" "/var/cache/obuilder/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3" "/var/cache/obuilder/result/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
---> saved as "8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
Got: "8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
```

# Machine architectures

QEMU support a variety of machine architectures.  The target architecture
can be selected using `--qemu-guest-arch` parameter.  At the moment only
AMD64 and RISCV64 are implemented in obuilder.

```
obuilder build --store=qemu:/var/cache/obuilder -v -f test.spec --qemu-memory 16 --qemu-cpus 8 --qemu-guest-os linux --qemu-guest-arch riscv64 .
```

By default, guests are given 30 seconds to boot and respond to SSH.
If you have slower hardware, you can add `--qemu-boot-time` to allow more
time of the machine to boot.

# Cache

Caching is implemented using additional hard disks which are added
to the machine and mounted on the cache location.  Different guest
operating systems will require different filesystems to be available.
The `Makefile` builds suitable empty disks to be used as cache disks.

The `spec` file could account for the different cache disks by using
`opam-archives-XXX` rather than just `opam-archives`.  e.g.

```
run (cache (opam-archives-ntfs (target "C:\\Users\\opam\\AppData\\Local\\opam\\download-cache")))
```

# Importing the project source

Obuilder uses `tar` to copy the project source into the sandbox.
Attempts to use `tar -xf - . | ssh opam@localhost -p 60022 tar -xf -`
fail as the data is corrupted.  This can be show also with `cat test.file
| ssh opam@localhost -p 60022 sha256sum -` where files of < 1M work most
of the time, but larger test files give a different hash everytime.

An alternative would be to use `guestfish` as below.  This works, albeit
the NTFS file permissions aren't clean, but I'm not happy with it as
it requires knowing the partition number ahead of time - `/dev/sda2` -
which impacts the ability of this to work more generically.

```
let tar_in ~cancelled ?stdin ~log:_ _ config result_tmp =
  let proc =
    let cmd = ["guestfish";
               "add-drive"; result_tmp / "rootfs" / "image.qcow2"; ":";
               "run"; ":";
               "mount"; "/dev/sda2"; "/"; ":";
               "tar-in"; "-"; config.Config.cwd; ] in
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f ("", config.Config.argv) in
    Os.sudo_result ?stdin ~pp cmd in
  proc >>= fun r ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled
```

Windows ships with BSD tar in `System32` and that does work with an `ssh` pipe.

