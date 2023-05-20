signature SPARSE_STATE_TABLE =
sig
  type t
  type table = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int, numQubits: int} -> table
  val singleton: {numQubits: int} -> BasisIdx.t * Complex.t -> table

  val size: table -> int
  val nonZeroSize: table -> int
  val zeroSize: table -> int
  val capacity: table -> int

  (* val insertIfNotPresent: table -> BasisIdx.t * Complex.t -> bool *)

  val insertAddWeights: table -> BasisIdx.t * Complex.t -> unit

  val insertAddWeightsLimitProbes: {probes: int}
                                   -> table
                                   -> BasisIdx.t * Complex.t
                                   -> unit

  val forceInsertUnique: table -> BasisIdx.t * Complex.t -> unit

  (* not safe for concurrency with insertions *)
  val lookup: table -> BasisIdx.t -> Complex.t option

  val compact: table -> (BasisIdx.t * Complex.t) DelayedSeq.t

  val increaseCapacityByFactor: real -> table -> table

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: table -> (BasisIdx.t * Complex.t) option DelayedSeq.t
end
