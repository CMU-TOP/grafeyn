signature BASIS_IDX =
sig
  type qubit_idx = int
  type t

  val maxNumQubits: int option (* NONE: unlimited *)

  val toString: {numQubits: int} -> t -> string

  val zeros: t

  val setTo: bool -> t -> qubit_idx -> t
  val set: t -> qubit_idx -> t
  val unset: t -> qubit_idx -> t
  val flip: t -> qubit_idx -> t
  val swap: t -> qubit_idx * qubit_idx -> t
  val get: t -> qubit_idx -> bool

  (* not a very meaningful comparison; this is just for storing it in
   * ordered maps and whatever.
   *)
  val compare: t * t -> order
  val equal: t * t -> bool
  val hash: t -> int

  val fromDenseIndex: int -> t
  val toDenseIndex: t -> int
end