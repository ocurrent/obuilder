module Make (Raw : S.STORE) : sig
  include S.STORE

  val wrap : Raw.t -> t
end
