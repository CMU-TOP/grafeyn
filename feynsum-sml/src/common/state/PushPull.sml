signature PUSH_PULL =
sig
  structure SST: SPARSE_STATE_TABLE
  structure G: GATE
  structure B: BASIS_IDX
  structure C: COMPLEX
  sharing SST.B = G.B = B
  sharing SST.C = G.C = C
  val push: G.t * B.t DelayedSeq.t -> SST.SSS.t
  val pull: G.t * SST.t -> SST.SSS.t -> SST.t
  val apply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val pushApply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val pullCount: G.t * SST.t -> SST.SSS.t -> SST.t * int
  val applyAll: G.t Seq.t * SST.t -> DataFlowGraph.t -> {state: SST.t, numVerts: int, numEdges: int}
  val sampleStates: SST.t -> int -> B.t DelayedSeq.t
end

functor PushPull
  (structure SST: SPARSE_STATE_TABLE
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real): PUSH_PULL =
struct

  structure SST = SST
  structure B = SST.B
  structure C = SST.C
  structure G = Gate (structure B = B
                      structure C = C)
  structure SSS = SST.SSS

  
  val seed = Random.rand (50, 14125)
  (* Randomly selects k distinct elements from array xs *)
  fun sampleArray (xs: 'a array) (k: int) =
      let val len = Array.length xs
          fun swapAndRet i j =
              let val xi = Array.sub (xs, i)
                  val xj = Array.sub (xs, j) in
                Array.update (xs, i, xj);
                Array.update (xs, j, xi);
                xj
              end
      in
        Array.tabulate (k, fn i => swapAndRet i (Random.randRange (i, len) seed))
      end

  fun sampleStates st k =
      let val cst = SST.compact st
          val arr = Array.tabulate (DelayedSeq.length cst, #1 o DelayedSeq.nth cst)
          val shuf = sampleArray arr k
      in
        DelayedSeq.tabulate (fn i => Array.sub (shuf, i)) k
      end

  fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val sss = SSS.make { capacity = cap * 2, numQubits = #numQubits kern }
        fun pushB b = DelayedSeq.applyIdx (G.push kern b) (fn (_, b) => SSS.insert sss b)
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeys tgts (fn b => let val bs = G.pull kern b in
                                   SeqBasis.reduce
                                     1000
                                     C.+
                                     C.zero
                                     (0, DelayedSeq.length bs)
                                     (fn i => let val (b, c) = DelayedSeq.nth bs i in
                                                case SST.lookup amps b of
                                                    NONE => C.zero
                                                  | SOME c' => C.* (c, c')
                                              end)
                                 end)

  fun pushApply ((kern, state) : G.t * SST.t) =
      let val pushF = G.pull kern (* TODO: implement push that stores amps too *)
          val cap = G.maxBranchingFactor kern * SST.size state * 2
          val state' = SST.make { capacity = cap, numQubits = #numQubits kern }
          fun f i = case SST.sub state i of
                        NONE => 0
                      | SOME (b, c) =>
                        let val ds = pushF b in
                          DelayedSeq.applyIdx ds (fn (_, (b', c')) => SST.insertAndAdd state' (b', C.* (c, c')));
                          DelayedSeq.length ds
                        end
          val numEdges = SeqBasis.reduce 1000 op+ 0 (0, SST.capacity state) f
          val numVerts = SST.sizeInclZeroAmp state'
      in
        { state = state',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun pullCount ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeysWith
        tgts
        (fn b => let val bs = G.pull kern b in
                   (SeqBasis.reduce 1000 C.+ C.zero (0, DelayedSeq.length bs)
                         (fn i => let val (b, c) = DelayedSeq.nth bs i in
                                    case SST.lookup amps b of
                                        NONE => C.zero
                                      | SOME c' => C.* (c, c')
                                  end),
                   DelayedSeq.length bs)
                 end)
        op+ 0

  fun apply' ((kern, state, pushFrom): G.t * SST.t * B.t DelayedSeq.t) =
      let val pushed = push (kern, pushFrom)
          val (state', numEdges) = pullCount (kern, state) pushed
          val numVerts = SST.sizeInclZeroAmp state'
      in
        { state = state',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun apply ((kern, state): G.t * SST.t) =
      apply' (kern, state, SST.compactKeys state)

  val NUM_SAMPLES = 10
  val MAX_BRANCHING = 32
  val NUM_BASES = 500

  fun kernelizeThese max_branching (gs: G.t Seq.t) =
      let val gateCount = Seq.length gs
          fun iter (kerns: (G.t * int) list) (acc: G.t list) (cbf: int) (i: int) =
              if i >= gateCount then
                List.rev
                  (if List.null acc then
                     kerns
                   else
                     (G.fuses (Seq.rev (Seq.fromList acc)), List.length acc) :: kerns)
              else
                let val next = Seq.nth gs i
                    val nextBF = G.maxBranchingFactor next
                    val totalBF = cbf * nextBF
                in
                  if totalBF > max_branching then
                    iter ((G.fuses (Seq.rev (Seq.fromList acc)), List.length acc) :: kerns)
                         nil nextBF (i + 1)
                  else
                    iter kerns (next :: acc) totalBF (i + 1)
                end
          val g0 = Seq.nth gs 0
      in
        List.rev (iter nil (g0 :: nil) (G.maxBranchingFactor g0) 1)
      end

  fun applyAll ((gates, state): G.t Seq.t * SST.t) (dfg: DataFlowGraph.t) =
      let val visited = DataFlowGraphUtil.initState dfg
          val sampler = { gen = Random.rand (123, 456),
                          max_branching = MAX_BRANCHING,
                          num_samples = NUM_SAMPLES }
          val gateCount = Seq.length gates
          val numQubits = #numQubits dfg

          fun divPow2 r n = if n <= 0 then r else divPow2 (r / 2.0) (n - 1)

          fun statusUpdate {state, numVerts, numEdges, numGates} =
              let val size = SST.size state in
                print ("gate " ^ Int.toString numGates ^ "/" ^ Int.toString gateCount ^ ", "
                       ^ Int.toString numVerts ^ " vertices, "
                       ^ Int.toString numEdges ^ " edges, "
                       ^ Int.toString size ^ " states, "
                       ^ Real.fmt (StringCvt.FIX (SOME 8))
                                  (divPow2 (Real.fromInt size) numQubits) ^ " density\n")
              end

          val mbf = G.maxBranchingFactor o Seq.nth gates

          fun applyKernels (kerns: (G.t * int) list)
                           (old as {state, numVerts, numEdges, numGates}) =
              case kerns of
                  nil => old
                | (kern, numGates') :: remKerns =>
                  let val {state = state',
                           numVerts = numVerts',
                           numEdges = numEdges'} = apply (kern, state)
                      val new = { state = state',
                                  numVerts = numVerts + numVerts',
                                  numEdges = numEdges + numEdges',
                                  numGates = numGates + numGates' }
                  in
                    statusUpdate new;
                    applyKernels remKerns new
                  end

          fun iter (old as {state, numVerts, numEdges, numGates}) =
              (* Note: length of paths might be less than
               * NUM_SAMPLES because we remove duplicate paths *)
              let val paths = DataFlowGraphUtil.samplePaths sampler dfg mbf visited in
                if Seq.length paths = 0 then
                  { state = state, numVerts = numVerts, numEdges = numEdges }
                else
                  let val substate = sampleStates state (#num_samples sampler)
                      val stateEsts =
                          SeqBasis.tabulate
                            1 (0, Seq.length paths)
                            (fn i =>
                                (* TODO: maybe we shouldn't PURELY optimize for state size,
                                 * but also consider the number of verts/edges inside? *)
                                (* TODO: also, need to make this a weighted expectation *)
                                let val kern = G.fuses (Seq.map (Seq.nth gates)
                                                                (Seq.nth paths i))
                                    val { state = state',
                                          numVerts = numVerts',
                                          numEdges = numEdges' } =
                                        apply' (kern, state, substate) in
                                  SST.size state'
                                end)
                      val (_, best) = Array.foldri
                                        (fn (i, size, (best_size, best_i)) =>
                                            if size < best_size then
                                              (size, i)
                                            else
                                              (best_size, best_i))
                                   (Array.sub (stateEsts, 0), 0) stateEsts
                      val bestPath = Seq.nth paths best
                      val gs = Seq.map (Seq.nth gates) bestPath
                      val nextKernels = kernelizeThese (#max_branching sampler) gs
                      val new = applyKernels nextKernels old
                  in
                    iter new
                  end
              end
      in
        iter { state = state,
               numVerts = 0,
               numEdges = 0,
               numGates = 0 }
      end
      

  fun applyAllOld ((kerns, state): G.t Seq.t * SST.t) =
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
                    fun divPow2 r n = if n <= 0 then r else divPow2 (r / 2.0) (n - 1)
                in
                  print ("kernel " ^ Int.toString (i + 1) ^ "/" ^ Int.toString numkerns ^ ", " ^ Int.toString numVerts' ^ " vertices, " ^ Int.toString numEdges' ^ " edges, " ^ Int.toString (SST.size state') ^ " states, " ^ Real.fmt (StringCvt.FIX (SOME 8)) (divPow2 (Real.fromInt (SST.size state')) (#numQubits kern)) ^ " density\n");
                  iter (i + 1) new
                end
      in
        iter 0 {state = state, numVerts = 0, numEdges = 0}
      end
end
