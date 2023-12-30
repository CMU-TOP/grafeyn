functor PushPull
  (structure SST: SPARSE_STATE_TABLE2
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real) :>
sig
  structure SST: SPARSE_STATE_TABLE2
  structure G: GATE2
  structure B: BASIS_IDX
  sharing SST.B = G.B = B
  sharing SST.C = G.C
  val push: G.t * B.t Seq.t -> SST.SSS.t
  val pull: G.t * SST.t -> SST.SSS.t -> SST.t
end =
struct

  structure SST = SST
  structure B = SST.B
  structure C = SST.C
  structure G = Gate2 (structure B = B
                       structure C = C)
  structure SSS = SST.SSS

  val INITIAL_CAPACITY = 128

  fun push ((kern, amps): G.t * B.t Seq.t) =
    let val cap = G.maxBranchingFactor kern * Seq.length amps
        val sss = SSS.make { capacity = cap, numQubits = #numQubits kern }
        fun pushB b = Seq.iterate (fn ((), b) => SSS.insert sss b) () (G.push kern b)
        val _ = Seq.iterate (fn ((), b) => pushB b)
    in
      sss
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeys tgts (fn b => let val bs = G.pull kern b in
                                   SeqBasis.reduce
                                     1000
                                     (C.+)
                                     C.zero
                                     (0, Seq.length bs)
                                     (fn i => let val (b, c) = Seq.nth bs i in
                                                case SST.lookup amps b of
                                                    NONE => C.zero
                                                  | SOME c' => C.* (c, c')
                                              end)
                                 end)

end
