#!/bin/bash
set -eux
export OPAMYES=true

sudo chmod a+x /usr/local/bin/runc

sudo sh -c "cat > /usr/local/bin/uname" << EOF
#!/bin/sh

if test "\$1" = '-r'; then
  echo '5.08.0-6-amd64'
else
  exec /usr/bin/uname \$@
fi
EOF
sudo chmod a+x /usr/local/bin/uname

opam exec -- make

case "$1" in
    btrfs)
        dd if=/dev/zero of=/tmp/btrfs.img bs=100M count=50
        BTRFS_LOOP=$(sudo losetup -f)
        sudo losetup -P "$BTRFS_LOOP" /tmp/btrfs.img
        sudo mkfs.btrfs -f "$BTRFS_LOOP"
        sudo mkdir /btrfs
        sudo mount -t btrfs "$BTRFS_LOOP" /btrfs
        sudo chown "$(whoami)" /btrfs

        opam exec -- dune exec -- obuilder healthcheck --store=btrfs:/btrfs
        opam exec -- dune exec -- ./stress/stress.exe --store=btrfs:/btrfs

        # Populate the caches from our own GitHub Actions cache
        btrfs subvolume create /btrfs/cache/c-opam-archives
        cp -r ~/.opam/download-cache/* /btrfs/cache/c-opam-archives/
        sudo chown -R 1000:1000 /btrfs/cache/c-opam-archives

        opam exec -- dune exec -- obuilder build -f example.spec . --store=btrfs:/btrfs --color=always

        sudo umount /btrfs
        sudo losetup -d "$BTRFS_LOOP"
        sudo rm -f /tmp/btrfs.img
        ;;

    zfs)
        dd if=/dev/zero of=/tmp/zfs.img bs=100M count=50
        ZFS_LOOP=$(sudo losetup -f)
        sudo losetup -P "$ZFS_LOOP" /tmp/zfs.img
        sudo /sbin/modprobe zfs
        sudo zpool create zfs "$ZFS_LOOP"

        opam exec -- dune exec -- obuilder healthcheck --store=zfs:zfs
        opam exec -- dune exec -- ./stress/stress.exe --store=zfs:zfs

        # Populate the caches from our own GitHub Actions cache
        sudo zfs create zfs/cache/c-opam-archives
        sudo cp -r ~/.opam/download-cache/* /zfs/cache/c-opam-archives/
        sudo chown -R 1000:1000 /zfs/cache/c-opam-archives
        sudo zfs snapshot zfs/cache/c-opam-archives@snap

        opam exec -- dune exec -- obuilder build -f example.spec . --store=zfs:zfs --color=always

        sudo zpool destroy zfs
        sudo losetup -d "$ZFS_LOOP"
        sudo rm -f /tmp/zfs.img
        ;;

    # rsync_hardlink_unsafe)
    #     sudo mkdir /rsync
    #     sudo chown "$(whoami)" /rsync

    #     opam exec -- dune exec -- obuilder healthcheck --store=rsync:/rsync --rsync-mode hardlink_unsafe
    #     opam exec -- dune exec -- ./stress/stress.exe --store=rsync:/rsync --rsync-mode hardlink_unsafe

    #     # Populate the caches from our own GitHub Actions cache
    #     sudo mkdir -p /rsync/cache/c-opam-archives
    #     sudo cp -r ~/.opam/download-cache/* /rsync/cache/c-opam-archives/
    #     sudo chown -R 1000:1000 /rsync/cache/c-opam-archives

    #     opam exec -- dune exec -- obuilder build -f example.spec . --store=rsync:/rsync --rsync-mode hardlink_unsafe

    #     sudo rm -rf /rsync
    #     ;;

    rsync_hardlink)
        sudo mkdir /rsync
        sudo chown "$(whoami)" /rsync

        opam exec -- dune exec -- obuilder healthcheck --store=rsync:/rsync --rsync-mode hardlink
        opam exec -- dune exec -- ./stress/stress.exe --store=rsync:/rsync --rsync-mode hardlink

        # Populate the caches from our own GitHub Actions cache
        sudo mkdir -p /rsync/cache/c-opam-archives
        sudo cp -r ~/.opam/download-cache/* /rsync/cache/c-opam-archives/
        sudo chown -R 1000:1000 /rsync/cache/c-opam-archives

        opam exec -- dune exec -- obuilder build -f example.spec . --store=rsync:/rsync --rsync-mode hardlink --color=always

        sudo rm -rf /rsync
        ;;

   rsync_copy)
        sudo mkdir /rsync
        sudo chown "$(whoami)" /rsync

        opam exec -- dune exec -- obuilder healthcheck --store=rsync:/rsync --rsync-mode copy
        opam exec -- dune exec -- ./stress/stress.exe --store=rsync:/rsync --rsync-mode copy

        # Populate the caches from our own GitHub Actions cache
        sudo mkdir -p /rsync/cache/c-opam-archives
        sudo cp -r ~/.opam/download-cache/* /rsync/cache/c-opam-archives/
        sudo chown -R 1000:1000 /rsync/cache/c-opam-archives

        opam exec -- dune exec -- obuilder build -f example.spec . --store=rsync:/rsync --rsync-mode copy --color=always

        sudo rm -rf /rsync
        ;;
    *)
        printf "Usage: .run-gha-tests.sh [btrfs|rsync_hardlink|rsync_copy|zfs]" >&2
        exit 1
esac
