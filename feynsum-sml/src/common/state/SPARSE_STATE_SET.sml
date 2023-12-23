signature SPARSE_STATE_SET =
sig
  structure B: BASIS_IDX

  type t
  type set = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int, numQubits: int} -> set
  val singleton: {numQubits: int} -> B.t -> set

  val size: set -> int
  val nonZeroSize: set -> int
  val zeroSize: set -> int
  val capacity: set -> int

  val insert: set -> B.t -> unit

  val insertLimitProbes: {probes: int} -> set -> B.t -> unit

  val insertForceUnique: set -> B.t -> unit

  (* not safe for concurrency with insertions *)
  val contains: set -> B.t -> bool

  val compact: set -> B.t DelayedSeq.t

  val increaseCapacityByFactor: real -> set -> set

  (* Unsafe because underlying array is shared. If the set is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * set as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: set -> B.t option DelayedSeq.t
end
