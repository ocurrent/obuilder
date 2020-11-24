#!/bin/bash
set -eux
export OPAMYES=true

sudo wget https://github.com/opencontainers/runc/releases/download/v1.0.0-rc92/runc.amd64 -O /usr/local/bin/runc
sudo chmod a+x /usr/local/bin/runc

ZFS_LOOP=$(sudo losetup -f)
dd if=/dev/zero of=/tmp/zfs.img bs=100M count=50
sudo losetup -P $ZFS_LOOP /tmp/zfs.img
sudo zpool create zfs $ZFS_LOOP

BTRFS_LOOP=$(sudo losetup -f)
dd if=/dev/zero of=/tmp/btrfs.img bs=100M count=50
sudo losetup -P $BTRFS_LOOP /tmp/btrfs.img
sudo mkfs.btrfs -f $BTRFS_LOOP
sudo mkdir /btrfs
sudo mount -t btrfs $BTRFS_LOOP /btrfs
sudo chown $(whoami) /btrfs

[ -d ~/.opam/4.11.1 ] || opam init --compiler=4.11.1
opam install --deps-only -t .
opam exec -- make
opam exec -- dune exec -- ./stress/stress.exe btrfs:/btrfs
opam exec -- dune exec -- ./stress/stress.exe zfs:zfs

# Populate the caches from our own Travis cache
btrfs subvolume create /btrfs/cache/c-opam-archives
cp -r ~/.opam/download-cache/* /btrfs/cache/c-opam-archives/
sudo chown -R 1000:1000 /btrfs/cache/c-opam-archives

sudo zfs create zfs/cache/c-opam-archives
sudo cp -r ~/.opam/download-cache/* /zfs/cache/c-opam-archives/
sudo chown -R 1000:1000 /zfs/cache/c-opam-archives
sudo zfs snapshot zfs/cache/c-opam-archives@snap

opam exec -- dune exec -- obuilder build -f example.spec . --store=btrfs:/btrfs
opam exec -- dune exec -- obuilder build -f example.spec . --store=zfs:zfs
