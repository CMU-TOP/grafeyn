signature SPARSE_STATE_TABLE =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX

  type t
  type table = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int, numQubits: int} -> table
  val singleton: {numQubits: int} -> (B.t * C.t) -> table

  val size: table -> int
  val sizeInclZeroAmp: table -> int
  val capacity: table -> int

  val insert: table -> (B.t * C.t) -> unit

  val insertLimitProbes: {probes: int} -> table -> (B.t * C.t) -> unit

  val insertForceUnique: table -> (B.t * C.t) -> unit

  val insertAndAdd: table -> (B.t * C.t) -> unit

  (* not safe for concurrency with insertions *)
  val lookup: table -> B.t -> C.t option

  val sub: table -> int -> (B.t * C.t) option

  val compact: table -> (B.t * C.t) DelayedSeq.t

  val increaseCapacityByFactor: real -> table -> table

  structure SSS: SPARSE_STATE_SET
  sharing SSS.B = B
  val fromKeys: SSS.t -> (B.t -> C.t) -> table
  val fromKeysWith: SSS.t -> (B.t -> C.t * 'a) -> ('a * 'a -> 'a) -> 'a -> table * 'a

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: table -> (B.t * C.t) option DelayedSeq.t
end
