include S.FETCHER
(** The ZFS Clone fetcher assumes given some [base] "image" that
    there is a corresponding ZFS volume [obuilder/base-image/<base>]
    and [zfs clones] this directory over to the provided [rootfs]. *)
