# Porting OBuilder to FreeBSD

## The problem

OBuilder is a tool used to perform arbitrary, reproduceable builds of
OCaml-related software within a sandboxed environment.

It has been written for Linux, with support for Windows and MacOS systems being
added later. Porting to FreeBSD is the next logical step, since FreeBSD
(at least on amd64 and arm64 hardware) is a Tier 1 platform in the OCaml
ecosystem.

## The challenge

Being initially Linux-centric, OBuilder is architected around three major
requirements:

- initial build environments are `docker` images.
- sandboxing is performed using the Open Container Initiative tool `runc`.
- a filesystem with snapshot capabilities is needed, and acts as a cache of
  identical build steps.

Neither of the first two items are available under FreeBSD (a `docker` client
is available but quite useless as there is no native `docker` server),
therefore alternative solutions must be found. As for the filesystem
requirement, FreeBSD has been supporting Sun's ZFS filesystem out of the box for
many releases now.

Fortunately, the existing archicture in OBuilder encapsulates these needs as
`Fetcher`, `Sandbox` and `Store` modules, respectively, so the only work
required would be write FreeBSD-specific `Fetcher` and `Sandbox` modules.

## Porting to FreeBSD

### The fetcher

An initial attempt was made to fetch Docker images without using the `docker`
command. An existing script, `download-frozen-image`, can be found in the
`moby` Github project (the open source parts of `docker`) to that effect.

However, although using that script to fetch the various layers of the `docker`
image and apply them in order, the result will be useless, from a FreeBSD
perspective, as all the `docker` images available are filled with Linux
binaries, which can run under FreeBSD with the help of the compatibility module,
but would mislead the OCaml toolchain into believing it is running under Linux,
and thus would build Linux binaries.

Until `docker` is available under FreeBSD, there won't be a repository of
FreeBSD images suitable for use for OBuilder. Such images will, at least
in the beginning, be built locally in the Tarides CI network. It therefore
makes sense to expect `.tar.gz` archives to be available from the CI network,
and simply download and extract them to implement the `Fetcher` module.
Moreover, FreeBSD provides its own `fetch` command which is able to download
files over `http` and `https`, and can also use `file://` URIs, which turned
out to be very helpful during development.

There is currently no attempt to support aliases or canonical names, so all the
`(from ...)` stanza in OBuilder command files will need to be adjusted for use
with FreeBSD. This limitation can be overcome by pre-populating the OBuilder
cache with the most used images under their expected names on the OBuilder
worker systems.

### The sandbox

FreeBSD comes with its own sandboxing mechanism, named `jail`, since the late
1990s. In addition to only having access to a subset of the filesystem, jails
can also be denied network access, which fits the OBuilder usage pattern, where
network access is only allowed to fetch build dependencies.

In order to start a jail, the `jail` command is invoked with either a plain text
configuration file providing its configuration, or with the configuration
parameters (in the "name=value" form) on its commandline.

In order to keep things simple in OBuilder, and since the jail configuration
will only need a few parameters, they are all passed on the commandline. This
might be a problem, should the length of the command line to run, as specified
in the OBuilder command file, reach the FreeBSD command line size limit, but
as this limit is a few hundred kilobytes, this does not seem to be a serious
concern.

The `jail` invocation will provide:

- a unique jail name.
- the absolute path of the jail filesystem.
- the command (or shell script) to run in the jail.
- the user on behalf of which the command will be run. This requires the user to
  exist within the jail filesystem (`/etc/passwd` and `/etc/group` entries).

More options may be used to allow for network access, or specify commands to run
on the host or within the jail at various states of the jail lifecycle.

Also, for processes running under the jail to behave correctly, a stripped-down
`devfs` pseudo-filesystem needs to be mounted on the `/dev` directory within
the jail, and while this can be done automatically by `jail(8)` using the proper
`mount.devfs` option, care must be taken to correctly unmount this directory
after the command run within the jail has exited. In order to be sure there will
be no leftover `devfs` mounts, which would prevent removal of the jail
filesystem at cleanup time, an `umount` command is run unconditionally by
OBuilder after the `jail` command exits.

Lastly, since most, if not all, OBuilder commands will expect a proper `opam`
environment configuration, it is necessary to run the commands within a login
shell, and such a shell can only be run as root. Therefore the command which
will run within the jail is:

```
  /usr/bin/su -l obuilder_user_name -c "cd obuilder_directory && obuilder_command"
```

# Solving the chicken-and-egg problem

With the fetcher and the sandbox modules written, a complete OBuilder run can be
attempted. But in order to do this, two more pieces are needed:

- a base FreeBSD image, to be used by OBuilder runs.
- a native FreeBSD build of OBuilder.

The base FreeBSD image will require a proper `opam switch` to be created.
The commands used to create it can also be turned into an OBuilder command
file, in order to be able to quickly build any particular switch version.

### FreeBSD setup

The process of setting up a FreeBSD system, combined with the use of zfs
snapshots, allows various stages of the FreeBSD setup to be archived and
used as starting point for OBuilder operations.

We can take snapshots at the following points:

- after an initial simple FreeBSD installation, with a few external packages
  required by OBuilder itself added, and an `opam` user created. This will be
  known henceforth as "stage0".
- after `opam` has been installed and an initial `opam switch` created. This
  setup can be directly used as a starting point for OBuilder operations, and
  will be known henceforth as "stage1".

### Optional: setting up a FreeBSD Virtual Machine

If no physical machine is available to install FreeBSD on, one may use `qemu`
to run a virtual machine instead.

Simply download the installation dvd, and setup a disk image:

```
qemu-img create -f qcow2 da0.qcow2 10G
```

Then launch a virtual machine with a few CPUs and a few gigabytes of memory:

```
qemu-system-x86_64 \
    -smp 4 \
    -m 4G \
    -drive file=da0.qcow2,format=qcow2 \
    -drive file=FreeBSD-13.2-RELEASE-amd64-dvd1.iso,media=cdrom
```

Since the disk image has been freshly created, this will boot into the
installation dvd.

### Stage 0

FreeBSD installation is straightforward. There is nothing special to choose
within the installer, except for the use of ZFS as the default filesystem
(which is the default choice nowadays) and, of course, proper network
configuration (although the defaults of DHCP for IPv4 and SLAAC for IPv6 ought
to work in most networks).

The default ZFS setup creates one single pool for the disk, and separate
filesystems in it. OBuilder will however require its own work pool, so the
default ZFS settings ("guided root on zfs" disk layout) in the installer can't
be used. It will be easier to build base images anyway if there is no separate
zfs pool for `/usr/home`.

In order to save space, one might want to disable all optional system components
when asked which ones to install.

After the installation completes and the system reboots, one may log in as root.

A quick check of `/etc/rc.conf` should confirm that:

- there is a `zfs_enable="YES"` line.
- network configuration is similar to
  ```
  ifconfig_DEFAULT="DHCP inet6 accept_rtadv"
  ```
  or
  ```
  ifconfig_em0="DHCP"
  ifconfig_em0_ipv6="inet6 accept_rtadv"
  ```

At this point, it is possible to add an `opam` user with `adduser`:

```
echo 'opam:1000:::::::/bin/sh:' | adduser -q -w random -f -
```
(that's seven colons between the numerical uid and the shell.)

Note that the assigned random password of the `opam` user won't be displayed,
but this does not really matter since all uses of this account will be performed
through `/usr/bin/su -l opam`.

A few packages need to be installed at this point:

```
pkg install -y bash curl git gmake patch rsync sudo zstd
```

Note that `zstd` is an optional dependency when building an OCaml 4 switch,
but a required dependency when building an OCaml 5 switch.

Now that `sudo` has been installed, it should be configured to let the `opam`
user be able to use `sudo` for any command without entering any password, as
OBuilder depends on this:

```
echo "opam ALL=(ALL:ALL) NOPASSWD: ALL" > /usr/local/etc/sudoers.d/opam
chmod 440 /usr/local/etc/sudoers.d/opam
```

It is then time to take a snapshot of the system. Assuming the name of the
zfs pool for the root directory is `zroot`, run:

```
zfs snapshot zroot@stage0
```

The first snapshot can be cloned in order to build a filesystem archive of
`stage0`, suitable to be used by OBuilder.

```
zfs clone zroot@stage0 zroot/clone
```

However, some of the files have been made immutable during the installation
(for security reasons), and would cause errors while attempting to clean up,
so it is preferrable to remove these flags:

```
chflags -R 0 /zroot/clone
```

Then, the image can be made significantly smaller by removing several file sets:
- rescue binaries and kernel modules (won't be used by OBuilder)
- profiling libraries
- manual pages

```
cd /zroot/clone
rm -fR rescue boot usr/share/games
rm usr/bin/fortune usr/lib/lib*_p.a
find usr/share/man -type f -delete
cd /
```

The last step consists of setting the usual permissions on the `/tmp` directory,
since there will be no specific filesystem mounted there.

```
chmod 1777 /zroot/clone/tmp
```

It is now possible to create the `stage0` archive and destroy the snapshot
clone:

```
mkdir /archive
tar -C /zroot/clone -czf /archive/stage0.tar.gz .
zfs destroy zroot/clone
```

The warnings regarding the inability to archive sockets in `/var` can be safely
ignored.

### Stage 1

Once an archive of `stage0` is available, it is possible to install `opam` and
build an initial switch with the following OBuilder command file:

```
((build dev
  ((from file:///path/to/stage0.tar.gz)
    (workdir /home/opam)
    (user (name opam))
    (run
      (network host)
      (shell "fetch -q https://github.com/ocaml/opam/releases/download/2.1.4/opam-full-2.1.4.tar.gz"))
    (run
      (shell "tar xzf opam-full-2.1.4.tar.gz"))
    (run
      (network host) ; needed to fetch compiler and libraries
      (shell
        "cd opam-full-2.1.4 && \
         gmake compiler &&
         ./configure --prefix=/home/opam &&
         gmake lib-ext"))
    (run
      (shell "cd opam-full-2.1.4 && gmake && gmake install"))
    (run
      (network host)
      (shell "opam init -y -a --bare"))
    (run
      (network host)
      (shell "opam switch create 4.14.1"))
))
 ; nothing to do after build, but a valid from stanza is required
 (from file:///path/to/stage0.tar.gz)
)
```

The first time, of course, OBuilder is not available yet, so these commands need
to be run manually:

```
/usr/bin/su -l opam
```

Then:

```
fetch -q https://github.com/ocaml/opam/releases/download/2.1.4/opam-full-2.1.4.tar.gz &&
tar xzf opam-full-2.1.4.tar.gz &&
cd opam-full-2.1.4 &&
gmake compiler &&
./configure --prefix=/home/opam &&
gmake lib-ext &&
gmake &&
gmake install &&
opam init -y -a --bare
. ~/.opam/opam-init/init.sh
opam switch create 4.14.1
exit
```

Once this is done, stage1 is complete; a zfs snapshot can be created, then
a new archive can be built; one may consider removing sources
in order to make the archive smaller.

```
rm /usr/home/opam/opam-full-2.1.4.tar.gz
rm -fR /usr/home/opam/opam-full-2.1.4
rm -fR /usr/home/opam/.opam/*/.opam-switch/sources
rm -fR /usr/home/opam/.opam/download-cache
zfs snapshot zroot@stage1
mkdir /archive/stage1
cd /archive/stage1
tar xzpf /archive/stage0.tar.gz
tar -C /usr/home -cf - opam | tar -C ./usr/home -xpf -
tar czf /archive/stage1.tar.gz .
cd /archive
rm -fR stage1
```

### Building OBuilder under FreeBSD

This step is quite straightforward (and will be even simpler once the FreeBSD
support bits are merged into the main repository):

```
pkg install -y pkgconf sqlite3
/usr/bin/su -l opam
git clone https://github.com/dustanddreams/obuilder.git
cd obuilder
git checkout freebsdjail-sandbox
sed -i.orig 's/(Docker_extract)/(Archive_extract)/' main.ml
opam install -y dune
opam install -y --deps-only -t obuilder
opam install -y crunch extunix fpath # missed by the above step
dune build && dune install
```

or, using an existing OBuilder setup, adapted from `obuilder.spec` found in
the OBuilder source repository:

```
((build dev
  ((from file:///path/to/stage1.tar.gz)
   (workdir /src)
   (user (uid 1000) (gid 1000))
   (run (shell "sudo chown opam /src"))
   ; Copy just the opam file first (helps caching)
   (copy (src obuilder-spec.opam obuilder.opam) (dst ./))
   (run (shell "opam pin add -yn ."))
   ; Install OCaml dependencies
   (run
     (network host)
     (shell "sudo pkg install -y pkgconf sqlite3"))
   (run
     (network host)
     (cache (opam-archives (target /home/opam/.opam/download-cache)))
     (shell "opam install -y --deps-only -t obuilder"))
     (copy
      (src .)
      (dst /src/)
      (exclude .git _build _opam))
     ; Build and test
     (run (shell "opam exec -- dune build @install @runtest"))))
 ; Now generate a small runtime image with just the resulting binary:
 (from file:///path/to/stage0.tar.gz)
 (run
   (network host)
   (shell "pkg install -y libedit pkgconf sqlite3"))
   (copy (from (build dev))
      (src /src/_build/default/main.exe)
      (dst /usr/local/bin/obuilder))
   (run (shell "obuilder --help")))
```

## Integrating with OCluster

OCluster is a larger framework which processes build requests on a cluster
of systems, each running OBuilder. In order to make the FreeBSD systems
compatible with OCluster needs, a few more adjustments are necessary:

- a `docker` client needs to be installed on the OBuilder machine, even if it
  will not be used, as part of OCluster checks. Fortunately the `docker` client
  is available as a FreeBSD package, `pkg install -y docker` will do.
- an `obuilder` zfs pool needs to be created (on a separate disk or a separate
  partition).

In addition to this, the `stage0` and `stage1` snapshots can be used to set up
an initial image cache on each OBuilder worker machine, using `zfs send` on the
source machine and `zfs recv` on the build machine, to make the `stage0` and
`stage1` snapshots available as `/obuilder/base-image/busybox` and
`/obuilder/base-image/ocaml-4.14.1` respectively.

Since the cache will take precedence over the fetcher action, this will allow
OBuilder spec files to keep referring to the names from the docker registry.


## Conclusion

The modular design of OBuilder has allowed for it to be easily adapted to run
under FreeBSD. A few FreeBSD systems are currently being set up as OBuilder
workers within the OCluster orchestrator used by Tarides for automated OCaml
package testing, and will hopefully benefit the OCaml FreeBSD community.
