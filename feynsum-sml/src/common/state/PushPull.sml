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
  val applyAllOld: G.t Seq.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val sampleStates: SST.SSS.t -> int -> B.t DelayedSeq.t
end

functor PushPull
  (structure SST: SPARSE_STATE_TABLE
   val maxBranchingStride: int
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

  type gate_idx = int

  val seed = Random.rand (50, 14125)

  fun sampleArray (xs: 'a array) (k: int) =
      let val len = Array.length xs in
        SeqBasis.tabulate 1000 (0, k) (fn i => Array.sub (xs, Random.randRange (0, len - 1) seed))
      end  

  (* Randomly selects k distinct elements from array xs *)
  (*fun sampleArray (xs: 'a array) (k: int) =
      if k >= Array.length xs then
        xs
      else
        let val len = Array.length xs
            fun swapAndRet i j =
                let val xi = Array.sub (xs, i)
                    val xj = Array.sub (xs, j) in
                  Array.update (xs, i, xj);
                  Array.update (xs, j, xi);
                  xj
                end
        in
          Array.tabulate (k, fn i => swapAndRet i (Random.randRange (i, len - 1) seed))
        end*)

  fun sampleStates st k =
      let val cst = SSS.compact st
          val arr = SeqBasis.tabulate 5000 (0, DelayedSeq.length cst) (DelayedSeq.nth cst)
          val shuf = sampleArray arr k
      in
        DelayedSeq.tabulate (fn i => Array.sub (shuf, i)) (Array.length shuf)
      end

  fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val sss = SSS.make { capacity = cap * 2, numQubits = #numQubits kern }
        fun pushB b = G.push kern (SSS.insert sss) b
        (*fun pushB b = DelayedSeq.applyIdx (G.push kern b) (fn (_, b) => SSS.insert sss b)*)
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromSet tgts (fn b => let val bs = G.pull kern b in
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
      SST.fromSetWith
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

  fun apply ((kern, state): G.t * SST.t) =
      let val pushFrom = SST.compactKeys state
          val pushed = push (kern, pushFrom)
          val (state', numEdges) = pullCount (kern, state) pushed
          val numVerts = SST.sizeInclZeroAmp state'
      in
        { state = state',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun apply' ((kern, state, substate): G.t * SST.t * B.t DelayedSeq.t) =
      let val pushed = push (kern, substate)
          val pushed_size = SSS.size pushed
          val kbf = G.maxBranchingFactor kern
          val num_samples = Int.max (1, pushed_size div kbf)
          val subpushed = sampleStates pushed num_samples
          val substate' = SSS.make { capacity = DelayedSeq.length subpushed * 2, numQubits = G.numQubits kern }
          val _ = DelayedSeq.applyIdx subpushed (fn (_, b) => SSS.insert substate' b)
          val (substate'', numEdges) = pullCount (kern, state) substate'
          val numVerts = SST.sizeInclZeroAmp substate''
      in
        { state = substate'',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  val NUM_SAMPLES = 10
  val MAX_BRANCHING = 32
  val NUM_BASES = 1000

  fun kernelizeThese (allGates: G.t Seq.t) (theseGates: gate_idx Seq.t) =
      let val gateCount = Seq.length theseGates
          fun mbf gidx = G.maxBranchingFactor (Seq.nth allGates gidx)
          fun iter (kerns: (gate_idx Seq.t) list) (acc: gate_idx list) (cbf: int) (i: int) =
              if i >= gateCount then
                List.rev
                  (if List.null acc then
                     kerns
                   else
                     Seq.rev (Seq.fromList acc) :: kerns)
              else
                let val next = Seq.nth theseGates i
                    val nextBF = mbf next
                    val totalBF = cbf * nextBF
                in
                  if totalBF > maxBranchingStride then
                    iter (Seq.rev (Seq.fromList acc) :: kerns)
                         (next :: nil) nextBF (i + 1)
                  else
                    iter kerns (next :: acc) totalBF (i + 1)
                end
          val g0 = Seq.nth theseGates 0
      in
        List.rev (iter nil (g0 :: nil) (mbf g0) 1)
      end

  fun applyAll ((gates, state): G.t Seq.t * SST.t) (dfg: DataFlowGraph.t) =
      let val visited = DataFlowGraphUtil.initState dfg
          val sampler = { gen = Random.rand (123, 456),
                          max_branching = MAX_BRANCHING,
                          num_samples = NUM_SAMPLES,
                          num_bases = NUM_BASES }
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

          fun applyKernels (kerns: (gate_idx Seq.t) list)
                           (old as {state, numVerts, numEdges, numGates}) =
              case kerns of
                  nil => old
                | kern :: remKerns =>
                  let val _ = Seq.map (fn gidx => DataFlowGraphUtil.visit dfg gidx visited) kern
                      val fusedKern = G.fuse (Seq.map (Seq.nth gates) kern)
                      val {state = state',
                           numVerts = numVerts',
                           numEdges = numEdges'} = apply (fusedKern, state)
                      val new = { state = state',
                                  numVerts = numVerts + numVerts',
                                  numEdges = numEdges + numEdges',
                                  numGates = numGates + Seq.length kern }
                  in
                    statusUpdate { state = state', numVerts = numVerts', numEdges = numEdges', numGates = numGates + Seq.length kern };
                    applyKernels remKerns new
                  end

          fun iter (old as {state, numVerts, numEdges, numGates}) =
              (* Note: length of paths might be less than
               * NUM_SAMPLES because we remove duplicate paths *)
              let val paths = DataFlowGraphUtil.samplePaths sampler dfg mbf visited in
                if Seq.length paths = 0 then
                  { state = state, numVerts = numVerts, numEdges = numEdges }
                else if Seq.length paths = 1 then
                  let val onlyPath = Seq.nth paths 0
                      val nextKernels = kernelizeThese gates onlyPath
                      val new = applyKernels nextKernels old
                  in
                    iter new
                  end
                else
                  let val substate = sampleStates (SST.toSet state) (#num_bases sampler)
                      val stateEsts =
                          SeqBasis.tabulate
                            1 (0, Seq.length paths)
                            (fn i =>
                                (* TODO: maybe we shouldn't PURELY optimize for state size,
                                 * but also consider the number of verts/edges inside? *)
                                (* TODO: also, need to make this a weighted expectation *)
                                let val kern = G.fuse (Seq.map (Seq.nth gates)
                                                               (Seq.nth paths i))
                                    val { state = state',
                                          numVerts = numVerts',
                                          numEdges = numEdges' } =
                                        apply' (kern, state, substate) in
                                  (SST.size state', numVerts', numEdges')
                                end)
                      fun better (p1 as (i1, (state1, verts1, edges1))) (p2 as (i2, (state2, verts2, edges2))) =
                          if state1 < state2 orelse
                             (state1 = state2 andalso verts1 + edges1 < verts2 + edges2) then
                            p1
                          else
                            p2
                                    
                      val (best, _) = Array.foldri
                                        (fn (i, sve, bestsve) => better (i, sve) bestsve)
                                        (0, Array.sub (stateEsts, 0)) stateEsts
                      val bestPath = Seq.nth paths best
                      val nextKernels = kernelizeThese gates bestPath
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
