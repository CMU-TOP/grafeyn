signature HYBRID_STATE =
sig
  structure SST: SPARSE_STATE_TABLE
  structure DS: DENSE_STATE
  sharing SST.B = DS.B
  sharing SST.C = DS.C

  (*type t
  type state = t*)
  datatype state =
    Sparse of SST.t
  | Dense of DS.t
  | DenseKnownNonZeroSize of DS.t * int

  type t = state

end
