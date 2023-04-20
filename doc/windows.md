# OBuilder's Docker backend

> OBuilder takes a build script (similar to a Dockerfile) and performs
> the steps in it in a sandboxed environment.

> After each step, OBuilder uses the snapshot feature of the
> filesystem to store the state of the build. […] Repeating a build
> will reuse the cached results where possible.

## Motivation

Windows offers [native containers][windowscontainers] for sandboxing.
Finding a snapshotting filesystem might be more involved, there's
[Volume Shadow Copy Service (VSS)][VSS] and [WinBtrfs][]. There's
however no direct API for these services, they're not stable yet and
have few users, and composing them seems a temerary endeavour.

The choice was made to use Docker as a sandboxing and storage solution
for OBuilder. Docker has considerably more users, and hides some of the
complexity of interfacing with the operating system.

Docker being a portable system (with some caveats), the OBuilder
Docker backend can itself be run in theory over any system Docker
supports. Using native components, wherever available, should be
preferred. On Windows, Docker can run sunboxed applications using
either containerization or virtual machines (VM) with Hyper-V. On
macOS Docker currently works using virtual machines.
The virtualization layer makes it more costly to run code, compared to
containerization under Linux. Virtual machines provide more effective
isolation and stability under Windows, prompting OBuilder to default
to VMs.

[windowscontainers]: https://learn.microsoft.com/en-us/virtualization/windowscontainers/about/
[VSS]: https://learn.microsoft.com/en-us/windows-server/storage/file-server/volume-shadow-copy-service
[WinBtrfs]: https://github.com/maharmstone/btrfs

## Comparing Docker and native backends

The distinction between the sandboxing engine and the storage layer in
OBuilder doesn't exactly map to the Docker backend, as both sandbox
and store are provided by Docker and can't be swapped out for another
implementation. As such, OBuilder will now differentiate between a
_native_ sandboxing solution, such as [runc][] under Linux, coupled
with a storage engine, and the Docker backend, providing all-in-one
sandbox and storage.

The underlying `Store` and `Sandbox` modules can't be as decoupled
either with the Docker backend, they need to share more information.
This distinction is however useful enough for modularity that it is
retained.

The main difference resides in the fact that with the usual native
sandbox and storage solution, OBuilder is totally in charge: it
creates its own build identifiers, manages the filesystem, and spawns
containers. With the Docker backend however, Docker's the second
player of the game. Docker has its own view of the global state,
assigns its own identifiers to images and containers. Extra care must
be taken to ensure that OBuilder and Docker have a consistent view of
objects they're tracking.

Objects residing in the file system are "namespaced" using OBuilder's
state directory; with the Docker backend a small unique identifer for
each running OBuilder process is computed based on the instance
working directory, and makes up a prefix for all Docker objects
managed by this instance. This allows to track objects more easily,
and a clean table sweep of any left-overs.

[runc]: https://github.com/opencontainers/runc

Another notable difference is that with runc and traditional
filesystems, OBuilder can use tools from the host filesystem, for
instance to copy or compress files, as well as tools from inside the
guest filesystem, chosing or not to run them in the sandbox. With the
Docker backend, guest data isn't accessible from the host, thus tools
must be present in the guest image, or mounted in volumes in running
containers to operate on data in guest images.

## OBuilder operation

Using volumes is oftentimes problematic as standard users of the host
don't have read/write permissions on them by default, which involves
some system administration to set up. It's difficult to retain the
settings, and difficult to port, which is why OBuilder's Docker
backend tries to refrain from using Docker volumes as much as
possible, or only interact with them whilst mounted in containers.

## Copying files in & out of guests

There's two mode of operation for copying files in OBuilder: from the
context (the host filesystem), or from a previous build stage. As
Docker images are not directly writable from the host filesystem, this
involves communicating the data to a running container. The data could
either be given through a mounted volume, with [`docker cp`][docker
cp], or with a container executing `tar`. Volumes management is hard,
`docker cp` fails on some files in Windows. For stability, we prefer
using a container and tar files (preserving permissions, file
attributes, and allowing easy rewrite of paths). In some cases, it is
necessary to backup the permissions of the destination directory to
restore them after the tarball's extraction.

[docker cp]: https://docs.docker.com/engine/reference/commandline/cp/

Creating a tar file in OBuilder involves creating a _manifest_. It's
a tree data structure describing the file hierachy, with node types
(file, directory, symlink), names, and checksum for file content. The
manifest is generated in a fully-reproducible way, so that its
checksum can uniquely identify the data being copyied, in order to
cache the copy step.

Copying files from a previous build step is a bit more involved, as
once again the host doesn't have direct read access to the content of
Docker images. A solution could have been to run the manifest creation
code of OBuilder itself in a container, by mounting a volume
containing an simple OCaml executable with this code. It would sadly
be difficult to accomplish[^1] in the general case, as the OCaml
executable would need to correspond to the Docker image (arch, glibc).
The choice was made instead of porting the manifest creation code,
originally in OCaml, to bash. It produces the same output and errors.
It is assumed that Linux distributions ship a bash interpretor, and
tar. For Windows, OBuilder starts by creating a volume, nicknamed
_obuilder-libexec_, in which it copies the shell script, and necessary
tools from Cygwin to execute it (the shell executable, some coreutils,
tar). OBuilder can then run a container based on the source image,
with the _libexec_ volume mounted read-only, to create and output the
manifest. After the manifest is created, OBuilder calls `tar` in the
same fashion to extract data from the previous image, rewrites the tar
headers with the correct destination on-the-fly, and pipes the result
to the destination container, running tar in extraction mode, reading
from stdin.

Windows 10 ships [BSD tar][], but it doesn't understand symlinks.

[BSD tar]: https://ss64.com/nt/tar.html

[^1]: maybe not so much with [Esperanto][]?

[Esperanto]: https://github.com/dinosaure/esperanto

## OBuilder's snapshots and caches

When OBuilder executes a build step _B_ for the first time with a
snapshotting filesystem, it'll first look up or fetch the base image
_A_ of _B_. OBuilder then creates a snapshot _B'_ of _A_, and execute
the build using _B'_. If the build step succeeds, _B'_ is promoted as
_B_; if not, _B'_ is discarded.

Using the Docker backend, this resolves to checking whether a Docker
image _B_ associated with an OBuilder build exists. If not, tag _A_ as
_tmp-B_, and run the build _B_ in a Docker container with the tag
_tmp-B_. If it succeeds, _tmp-B_ can be commited as the Docker image
_B_, then _tmp-B_ is condemned to _damnatio memoriae_. Special care
must be taken as committing the container replaces the _entrypoint_
and _cmd_ fields of the Docker image by the commands given to run the
container. This is usually not intended, so these fields are retrieved
and restored from the base image.

Below is a sample build script and OBuilder logs, run on Windows.

```text
((from mcr.microsoft.com/windows/servercore:ltsc2022) ; A
 (run (shell "echo hello > world")) ; B
 (run (shell "type world"))) ; C
```

```sh
$ obuilder build -f simple.spec --store=docker:./var --docker-cpus=8 --docker-memory=4g -v .
```

```tex
obuilder.exe: [INFO] Exec "docker" "container" "ls" "--all" "--filter" "name=^obuilder-3b98949" "-q"
obuilder.exe: [INFO] Removing left-over Docker images
obuilder.exe: [INFO] Exec "docker" "images" "--format={{ .Repository }}" "obuilder-3b98949-image-tmp-*"
obuilder.exe: [INFO] Removing left-over Docker volumes
obuilder.exe: [INFO] Exec "docker" "volume" "ls" "--quiet" "--filter" "name=^obuilder-3b98949-cache-tmp-"
obuilder.exe: [INFO] Exec "docker" "volume" "inspect" "--" "obuilder-3b98949-libexec"
(from mcr.microsoft.com/windows/servercore:ltsc2022)
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--" "obuilder-3b98949-image-bc3bc8408e84c12c2b5f24aa91b444894b55e26069b66e8034890634b08aef1d"
Error: No such image: obuilder-3b98949-image-bc3bc8408e84c12c2b5f24aa91b444894b55e26069b66e8034890634b08aef1d
obuilder.exe: [INFO] Base image not present; importing "mcr.microsoft.com/windows/servercore:ltsc2022"…
obuilder.exe: [INFO] Exec "docker" "pull" "mcr.microsoft.com/windows/servercore:ltsc2022"
ltsc2022: Pulling from windows/servercore
Digest: sha256:3949614905ddf2c4451b18894563c36f0c0aa93ab0e17ea6f8ca3791313e4e4f
Status: Image is up to date for mcr.microsoft.com/windows/servercore:ltsc2022
mcr.microsoft.com/windows/servercore:ltsc2022
obuilder.exe: [INFO] Exec "docker" "tag" "mcr.microsoft.com/windows/servercore:ltsc2022" "obuilder-3b98949-image-bc3bc8408e84c12c2b5f24aa91b444894b55e26069b66e8034890634b08aef1d"
---> saved as "bc3bc8408e84c12c2b5f24aa91b444894b55e26069b66e8034890634b08aef1d"
C:/: (run (shell "echo hello > world"))
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--" "obuilder-3b98949-image-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
Error: No such image: obuilder-3b98949-image-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd
obuilder.exe: [INFO] Exec "docker" "tag" "obuilder-3b98949-image-bc3bc8408e84c12c2b5f24aa91b444894b55e26069b66e8034890634b08aef1d" "obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=container" "--" "obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
Error: No such container: obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd
obuilder.exe: [INFO] Exec "docker" "run" "-i" "--name" "obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd" "--cpus" "8.000000" "--isolation" "hyperv" "--hostname" "builder" "--workdir" "C:/" "--entrypoint" "cmd" "--memory" "4g" "--user" "ContainerAdministrator" "obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd" "/S" "/C" "echo hello > world"
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--format={{json .Config.Entrypoint }}" "--" "obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--format={{json .Config.Cmd }}" "--" "obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
obuilder.exe: [INFO] Exec "docker" "commit" "--change=CMD ["c:\\windows\\system32\\cmd.exe"]" "--" "obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd" "obuilder-3b98949-image-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
obuilder.exe: [INFO] Exec "docker" "rm" "--force" "--" "obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
sha256:31d1fcc968e21a34fca97a73b56500b0e0208df9c8be60f5eed8369f107878ab
obuilder.exe: [INFO] Exec "docker" "image" "rm" "obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
obuilder-3b98949-container-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd
Untagged: obuilder-3b98949-image-tmp-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd:latest
---> saved as "ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd"
C:/: (run (shell "type world"))
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--" "obuilder-3b98949-image-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
Error: No such image: obuilder-3b98949-image-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153
obuilder.exe: [INFO] Exec "docker" "tag" "obuilder-3b98949-image-ac4488b2ca69de829c9a8bbcd9efa2ddff493a3b5888a53ec20a1343ea34b2bd" "obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=container" "--" "obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
Error: No such container: obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153
obuilder.exe: [INFO] Exec "docker" "run" "-i" "--name" "obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153" "--cpus" "8.000000" "--isolation" "hyperv" "--hostname" "builder" "--workdir" "C:/" "--entrypoint" "cmd" "--memory" "4g" "--user" "ContainerAdministrator" "obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153" "/S" "/C" "type world"
hello
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--format={{json .Config.Entrypoint }}" "--" "obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
obuilder.exe: [INFO] Exec "docker" "inspect" "--type=image" "--format={{json .Config.Cmd }}" "--" "obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
obuilder.exe: [INFO] Exec "docker" "commit" "--change=CMD ["c:\\windows\\system32\\cmd.exe"]" "--change=ENTRYPOINT ["cmd"]" "--" "obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153" "obuilder-3b98949-image-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
obuilder.exe: [INFO] Exec "docker" "rm" "--force" "--" "obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
sha256:fa67558f979026a08c63c56215253bec59da6d7dff67bf083fb580f96fe1a820
obuilder-3b98949-container-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153
obuilder.exe: [INFO] Exec "docker" "image" "rm" "obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
Untagged: obuilder-3b98949-image-tmp-7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153:latest
---> saved as "7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
Got: "7332a0565a4047bdd2c0b778bf3a9175518218879547eb8ddde6832d57861153"
```

There's also the shared build cache which can be used to mount one or
more persistent caches for the command. It is also usually implemented
with the snapshotting filesystem. With Docker, this feature is
implemented by [mounting volumes][] in Docker containers. They have
the major disadvantage that there's no copy-on-write or snapshotting
available for volumes. They first have to be copied, and testing has
proved that copying on the host is unreliable because of permissions,
so the source volume is tar'ed in a container, and the tar is streamed
into a second container extracting it to the destination volume.

[mounting volumes]: https://docs.docker.com/storage/volumes/

A piece of advice: if you try to implement any feature with Docker on
Windows, make sure it works first in a shell script, if possible.

_Mettez Docker à l'ouvrage!_
