# OBuilder's QEMU Sandbox

This backend should work with any OS which can be booted in QEMU and
which can provide an SSH interface.

# Base Images

These need to be provided as boot disks.  There is a `Makefile` in the
`qemu` directory which builds two base images:

- ubuntu-noble-x86_64-ocaml-4.14.img
- windows-server-2022-x86_64-ocaml-4.14.img

The base images build automatically using Cloud Init on Ubuntu and
`autounattend.xml` on Windows.

# Operation


A spec which reference the required base image in using the `from`
directive, then run the whatever commands are required.  An trivial
example is given below.

```
(
 (from windows-server-2022-x86_64-ocaml-4.14)
 (run
  (cache (opam-archives (target /Users/opam/AppData/Local/opam/download-cache)))
  (shell "opam install tar")
 )
)
```

A typical invocation via `obuilder build` would be as below.  Note that
in this example, the base images would be in `/data/base-image/*.img`.

```
./_build/install/default/bin/obuilder build --store=qemu:/data -v -f test.spec --qemu-memory 16 --qemu-cpus 8 .
```

The `from` directive causes `qemu-img` to create a snapshot of the base
image and stage it in the `result-tmp` folder.  When this completes
successfully, `result-tmp` is moved to `result`:

```
(from windows-server-2022-x86_64-ocaml-4.14)
obuilder: [INFO] Base image not present; importing "windows-server-2022-x86_64-ocaml-4.14"…
obuilder: [INFO] Exec "mkdir" "-m" "755" "--" "/var/lib/docker/test/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs"
obuilder: [INFO] Exec "qemu-img" "create" "-f" "qcow2" "-b" "/var/lib/docker/test/base-image/windows-server-2022-x86_64-ocaml-4.14.img" "-F" "qcow2" "/var/lib/docker/test/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2"
Formatting '/var/lib/docker/test/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2', fmt=qcow2 cluster_size=65536 extended_l2=off compression_type=zlib size=42949672960 backing_file=/var/lib/docker/test/base-image/windows-server-2022-x86_64-ocaml-4.14.img backing_fmt=qcow2 lazy_refcounts=off refcount_bits=16
obuilder: [INFO] Exec "mv" "/var/lib/docker/test/result-tmp/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101" "/var/lib/docker/test/result/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101"
---> saved as “dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101”
```

Moving on to the next stage in the build which is the `run` directive.
First, `qemu-img` creates a snapshot of the current `result` layer into
`result-tmp`.  Then `qemu-system-x86_64` is started with this snapshot as
the base image.  `ssh` is used to poll the machine until it is available.
Next, `scp` runs to copy the cache `opam-archives` over to the target
directory `/Users/opam/AppData/Local/opam/download-cache`.  Finally,
the actual commands are sent over `ssh` to install `tar`.  The step
completes with an `scp` of the cache back to the host followed by an
ACPI shutdown command sent to the qemu console.

```
/: (run (cache (opam-archives (target /Users/opam/AppData/Local/opam/download-cache)))
        (shell "opam install tar"))
obuilder: [INFO] Exec "qemu-img" "create" "-f" "qcow2" "-b" "/var/lib/docker/test/result/dce4336e183de81da7537728ed710f2906e9f75431694d9de80b95a9d9ff1101/rootfs/image.qcow2" "-F" "qcow2" "/var/lib/docker/test/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3/rootfs/image.qcow2" "40G"
obuilder: [INFO] Exec "cp" "-pRduT" "--reflink=auto" "/var/lib/docker/test/cache/c-opam-archives" "/var/lib/docker/test/cache-tmp/0-c-opam-archives"
obuilder: [INFO] Fork exec "qemu-system-x86_64" "-m" "16G" "-smp" "8" "-machine" "accel=kvm,type=q35" "-cpu" "host" "-nic" "user,hostfwd=tcp::34649-:22" "-display" "none" "-monitor" "stdio" "-drive" "file=/var/lib/docker/test/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3/rootfs/image.qcow2,format=qcow2"
obuilder: [INFO] Exec "ssh" "opam@localhost" "-p" "34649" "-o" "BatchMode=yes" "-o" "NoHostAuthenticationForLocalhost=yes" "exit"
obuilder: [INFO] Exec "scp" "-P" "34649" "-o" "NoHostAuthenticationForLocalhost=yes" "-prq" "/var/lib/docker/test/cache-tmp/0-c-opam-archives/md5" "/var/lib/docker/test/cache-tmp/0-c-opam-archives/sha512" "/var/lib/docker/test/cache-tmp/0-c-opam-archives/sha256" "opam@localhost:/Users/opam/AppData/Local/opam/download-cache"
obuilder: [INFO] Fork exec "ssh" "opam@localhost" "-p" "34649" "-o" "NoHostAuthenticationForLocalhost=yes" "cd" "/" "&&" "opam install tar"
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
obuilder: [INFO] Exec "scp" "-P" "34649" "-o" "NoHostAuthenticationForLocalhost=yes" "-prq" "opam@localhost:/Users/opam/AppData/Local/opam/download-cache/*" "/var/lib/docker/test/cache-tmp/0-c-opam-archives"
obuilder: [INFO] Sending QEMU an ACPI shutdown event
obuilder: [INFO] Exec "cp" "-pRduT" "--reflink=auto" "/var/lib/docker/test/cache-tmp/0-c-opam-archives" "/var/lib/docker/test/cache/c-opam-archives"
obuilder: [INFO] Exec "rm" "-r" "/var/lib/docker/test/cache-tmp/0-c-opam-archives"
obuilder: [INFO] Exec "mv" "/var/lib/docker/test/result-tmp/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3" "/var/lib/docker/test/result/8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
---> saved as "8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
Got: "8a897f21e54db877fc971c757ef7ffc2e1293e191dc60c3a18f24f0d3f0926f3"
```

# Note

While this initial version only runs on x86_64 targetting x86_64
processors it would be entirely possibly to extend this to other
architectures.
