functor PushPull
  (structure HS: HYBRID_STATE
   structure G: GATE
   sharing HS.B = G.B
   sharing HS.C = G.C
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real) :>
sig
  structure B = HS.B
  structure C = HS.C
  val push: G.t Seq.t * (B, C) HashTable.t -> B HashSet.t
  val pull: G.t Seq.t * (B, C) HashTable.t -> B HashSet.t -> (B, C) HashTable.t
end =
struct

  structure B = HS.B
  structure C = HS.C


  fun push ((kern, amps): G.t Seq.t * (B, C) HashTable.t) =
    TODO

  fun pull ((kern, amps): G.t Seq.t * (B, C) HashTable.t) (tgts: B HashSet.t) =
    TODO

end
