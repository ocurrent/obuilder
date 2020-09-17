module Make (Raw : S.STORE) : sig
  include S.STORE with type id = Raw.id

  val wrap : Raw.t -> t
end
