module basis_idx: {
  type t
  type qubit_idx = i64
  val zeros: t
  val set: t -> qubit_idx -> bool -> t
  val get: t -> qubit_idx -> bool
  val flip: t -> qubit_idx -> t
} = {
  type t = u64
  type qubit_idx = i64

  def zeros: t = 0

  def selector qi =
    u64.(1 << i64 qi)
  
  def set bidx qi b =
    u64.(
      let cleared = bidx & not (selector qi)
      in ((if b then 1 else 0) << i64 qi) | cleared
    )

  def get bidx qi =
    u64.((bidx & selector qi) != 0)

  def flip bidx qi =
    u64.(bidx ^ selector qi)
}