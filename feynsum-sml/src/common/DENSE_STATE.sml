signature DENSE_STATE =
sig
  structure C: COMPLEX

  type t
  type state = t

  val make: {numQubits: int} -> state

  val pull: {numQubits: int}
            -> (BasisIdx.t -> {weight: C.t, count: int})
            -> {result: state, nonZeroSize: int, totalCount: int}

  val size: state -> int
  val nonZeroSize: state -> int
  val zeroSize: state -> int
  val capacity: state -> int

  val unsafeViewContents: state -> (BasisIdx.t * C.t) option DelayedSeq.t

  val lookup: state -> BasisIdx.t -> C.t option
  val lookupDirect: state -> BasisIdx.t -> C.t
  val insertAddWeights: state -> BasisIdx.t * C.t -> unit
end
