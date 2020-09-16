#!/bin/bash
set -eux
export OPAMYES=true

ZFS_LOOP=$(sudo losetup -f)
dd if=/dev/zero of=/tmp/zfs.img bs=100M count=10
sudo losetup -P $ZFS_LOOP /tmp/zfs.img
sudo zpool create zfs $ZFS_LOOP

BTRFS_LOOP=$(sudo losetup -f)
dd if=/dev/zero of=/tmp/btrfs.img bs=100M count=10
sudo losetup -P $BTRFS_LOOP /tmp/btrfs.img
sudo mkfs.btrfs -f $BTRFS_LOOP
sudo mkdir /btrfs
sudo mount -t btrfs $BTRFS_LOOP /btrfs
sudo chown $(whoami) /btrfs

[ -d ~/.opam/4.11.1 ] || opam init --compiler=4.11.1
opam install --deps-only -t .
opam exec -- make
opam exec -- dune exec -- obuilder build -f test/test1.spec test/test1 --store=btrfs:/btrfs
opam exec -- dune exec -- obuilder build -f test/test1.spec test/test1 --store=zfs:zfs
