#!/bin/bash
set -eu
if [ "$#" -lt 1 ]; then
	echo "usage: $0 STORE..."
	echo "e.g. $0 btrfs:/btrfs/stress zfs:stress"
	exit 1;
fi;
stores="$*"
echo "Remove everything that depends on busybox..."
for store in $stores; do
	echo Clean $store
	dune exec -- obuilder delete 9d75f0d7c398df565d7ac04c6819b62d6d8f9560f5eb4672596ecd8f7e96ae91 --store=$store
done;
for store in $stores; do
	echo Test $store
	dune exec ./stress/stress.exe --store=$store
done;
echo PASS
