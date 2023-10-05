signature SPARSE_STATE_TABLE =
sig
  structure C: COMPLEX

  type t
  type table = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int, numQubits: int} -> table
  val singleton: {numQubits: int} -> BasisIdx.t * C.t -> table

  val size: table -> int
  val nonZeroSize: table -> int
  val zeroSize: table -> int
  val capacity: table -> int

  (* val insertIfNotPresent: table -> BasisIdx.t * C.t -> bool *)

  val insertAddWeights: table -> BasisIdx.t * C.t -> unit

  val insertAddWeightsLimitProbes: {probes: int}
                                   -> table
                                   -> BasisIdx.t * C.t
                                   -> unit

  val forceInsertUnique: table -> BasisIdx.t * C.t -> unit

  (* not safe for concurrency with insertions *)
  val lookup: table -> BasisIdx.t -> C.t option

  val compact: table -> (BasisIdx.t * C.t) DelayedSeq.t

  val increaseCapacityByFactor: real -> table -> table

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: table -> (BasisIdx.t * C.t) option DelayedSeq.t
end
