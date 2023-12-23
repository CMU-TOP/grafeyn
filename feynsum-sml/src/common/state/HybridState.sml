functor HybridState
  (structure SST: SPARSE_STATE_TABLE
   structure DS: DENSE_STATE
   sharing SST.B = DS.B
   sharing SST.C = DS.C): HYBRID_STATE =
struct
  structure SST = SST
  structure DS = DS
  structure B = DS.B
  structure C = DS.C

  datatype state =
    Sparse of SST.t
  | Dense of DS.t
  | DenseKnownNonZeroSize of DS.t * int

  type t = state
end
