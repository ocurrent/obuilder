(** Store builds results using {b XFS} with the reflink feature.

    XFS is intended to behave consistently as it scales to large storage and many files, modern-day XFS was originally from SGI Irix. This store uses the {b reflink} feature in XFS to share blocks between files, to support fast snapshots of directory trees and deduplicate file data for more efficient use of storage hardware.

    For more details on the XFS implementation see {{: https://blogs.oracle.com/linux/post/xfs-data-block-sharing-reflink} XFS - Data Block Sharing (Reflink)} and {{: https://blogs.oracle.com/linux/post/upcoming-xfs-work-in-linux-v48-v49-and-v410-by-darrick-wong} Upcoming XFS Work in Linux v4.8 v4.9 and v4.10+}. *)

include S.STORE

val create : path:string -> t Lwt.t
(** [create ~path] creates a new XFS store where everything will
    be stored under [path]. *)
