signature PUSH_PULL =
sig
  structure SST: SPARSE_STATE_TABLE2
  structure G: GATE2
  structure B: BASIS_IDX
  structure C: COMPLEX
  sharing SST.B = G.B = B
  sharing SST.C = G.C = C
  val push: G.t * B.t DelayedSeq.t -> SST.SSS.t
  val pull: G.t * SST.t -> SST.SSS.t -> SST.t
  val apply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val pullCount: G.t * SST.t -> SST.SSS.t -> SST.t * int Seq.t
  val applyAll: G.t Seq.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  (*val run: 'dfg * SST.t * (SST.t * 'dfg -> (G.t * 'dfg) option) -> {state: SST.t, numVerts: int, numEdges: int}*)
  (*val sampleStates: SST.t -> int -> B.t Seq.t*)
end

functor PushPull
  (structure SST: SPARSE_STATE_TABLE2
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real): PUSH_PULL =
struct

  structure SST = SST
  structure B = SST.B
  structure C = SST.C
  structure G = Gate2 (structure B = B
                       structure C = C)
  structure SSS = SST.SSS



  (*
  val seed = Random.rand (50, 14125)
  (* Randomly selects k distinct elements from array xs *)
  fun sampleArray (xs: 'a array) (k: int) =
      let val len = Array.length xs
          fun swapAndRet i j =
              let val xi = Array.sub (xs, i)
                  val xj = Array.sub (xs, j)
                  val _ = Array.update (xs, i, xj)
                  val _ = Array.update (xs, j, xi)
              in
                xj
              end
      Array.tabulate (k, fn i => swapAndRet i (Random.randRange (i, len)))

  fun sampleStates st k =
      let val cst = SST.compact st
          val arr = Array.tabulate (Seq.length cst, fn i => Seq.nth cst i)
          val shuf = sampleArray arr k
      in
        Seq.tabulate (fn i => Array.sub (shuf, i)) k
      end
  *)

  fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val sss = SSS.make { capacity = cap * 4, numQubits = #numQubits kern }
        (*fun pushB b = Seq.iterate (fn ((), b) => SSS.insert sss b) () (G.push kern b)*)
        fun pushB b = (Seq.map (fn b => SSS.insert sss b) (G.push kern b); ())
        val _ = Seq.map pushB (Seq.tabulate (DelayedSeq.nth amps) (DelayedSeq.length amps))
        (* val _ = DelayedSeq.map (fn b => pushB b) amps *)
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

  fun pullCount ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeysWith
        tgts
        (fn b => let val bs = G.pull kern b in
                   (SeqBasis.reduce
                      1000
                      (C.+)
                      C.zero
                      (0, Seq.length bs)
                      (fn i => let val (b, c) = Seq.nth bs i in
                                 case SST.lookup amps b of
                                     NONE => C.zero
                                   | SOME c' => C.* (c, c')
                               end),
                    Seq.length bs)
                 end)

  fun apply ((kern, state): G.t * SST.t) =
      let val nonzeros = SST.compact state
          (* val _ = print ("Num nonzeros: " ^ Int.toString (DelayedSeq.length nonzeros) ^ "\n") *)
          val pushed = push (kern, DelayedSeq.map (fn (b, c) => b) nonzeros)
          (* val _ = print ("Num pushed: " ^ Int.toString (SSS.size pushed) ^ "\n") *)
          val (pulled, nums) = pullCount (kern, state) pushed
          val numVerts = Seq.length nums
          val numEdges = Seq.reduce op+ 0 nums
      in
        { state = pulled,
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun applyAll ((kerns, state): G.t Seq.t * SST.t) =
      let val numkerns = Seq.length kerns
          fun iter i (old as {state, numVerts, numEdges}) =
              if i >= numkerns then
                old
              else
                let val kern = Seq.nth kerns i
                    val {state = state', numVerts = numVerts', numEdges = numEdges'} =
                        apply (kern, state)
                    val new = { state = state',
                                numVerts = numVerts + numVerts',
                                numEdges = numEdges + numEdges' }
                in
                  print ("kernel = " ^ Int.toString i ^ "/" ^ Int.toString numkerns ^ ", verts = " ^ Int.toString numVerts' ^ ", edges = " ^ Int.toString numEdges' ^ "\n");
                  iter (i + 1) new
                end
      in
        iter 0 {state = state, numVerts = 0, numEdges = 0}
      end


  (*fun run ((kerns, state, next): 'dfg * SST.t * (SST.t * 'dfg -> (G.t * 'dfg) option)) =
      let fun iter i (old as {state, numVerts, numEdges}) =
              if i >= numkerns then
                old
              else
                let val kern = Seq.nth kerns i
                    val {state = state', numVerts = numVerts', numEdges = numEdges'} =
                        apply (kern, state)
                    val new = { state = state',
                                numVerts = numVerts + numVerts',
                                numEdges = numEdges + numEdges' }
                in
                  print ("kernel = " ^ Int.toString i ^ ", verts = " ^ Int.toString numVerts' ^ ", edges = " ^ Int.toString numEdges' ^ "\n");
                  iter (i + 1) new
                end
      in
        iter 0 {state = state, numVerts = 0, numEdges = 0}
      end*)

end
