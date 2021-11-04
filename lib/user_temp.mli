include S.FETCHER
(** The user template fetcher assumes given some [base] "image" that
    there is a corresponding directory [/Users/<base>] and [rsync]s
    this directory over to the provided [rootfs] directory. *)
