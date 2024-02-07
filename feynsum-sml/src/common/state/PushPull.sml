functor PushPull
  (structure B: BASIS_IDX
   structure C: COMPLEX
   val numQubits: int
   val maxBranchingStride: int
   val schedSamplePathDepth: int
   val schedSamplePaths: int
   val schedSampleStates: int
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real):
sig
  structure G: GATE
  structure B: BASIS_IDX
  structure C: COMPLEX
  sharing G.B = B
  sharing G.C = C

  structure SSS: HASH_TABLE
  structure SST: HASH_TABLE
  structure SSC: HASH_TABLE

  val push: G.t * B.t DelayedSeq.t -> SSS.t
  val pushCount: G.t * B.t DelayedSeq.t -> SSC.t
  val pullCount: G.t * SSS.t * SSC.t -> SSC.t
  val pull: G.t * SST.t -> SSS.t -> SST.t
  val apply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val applySub: G.t * SST.t * B.t DelayedSeq.t -> real * int
  (*val pushApply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}*)
  val pullInfo: G.t * SST.t -> SSS.t -> SST.t * int
  val applyAll: G.t Seq.t * SST.t -> DataFlowGraph.t
                -> {state: SST.t, numVerts: int, numEdges: int}
  val applyAllOld: G.t Seq.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val dumbFusion: G.t Seq.t -> G.t Seq.t
  (*val sampleStates: SSS.t -> int -> B.t DelayedSeq.t*)
end =
struct
  structure B = B
  structure C = C
  structure G = Gate (structure B = B
                      structure C = C)

  type gate_idx = int

  val seed = Random.rand (50, 14125)


  structure SSS = SparseStateSet
                    (structure B = B
                     val numQubits = numQubits)
  structure SST = SparseStateTable
                    (structure B = B
                     structure C = C
                     val numQubits = numQubits)
  structure SSC = SparseStateCounter
                    (structure B = B
                     val numQubits = numQubits)

  fun sampleStates cst k =
      let val arr = SeqBasis.tabulate 5000 (0, DelayedSeq.length cst) (DelayedSeq.nth cst)
          val shuf = Helpers.sampleArray seed arr k
      in
        DelayedSeq.tabulate (fn i => Array.sub (shuf, i)) (Array.length shuf)
      end

  fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val sss = SSS.make { capacity = SSS.defaultPadding cap }
        fun pushB b = G.push kern (fn x => SSS.insert sss (x, ())) b
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end

  fun pushCount ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val ssc = SSC.make { capacity = SSC.defaultPadding cap }
        val _ = DelayedSeq.applyIdx amps (G.push kern (fn x => SSC.update ssc (x, 1)) o #2)
    in
      ssc
    end

  fun pullCount ((kern, substate, amps): G.t * SSS.t * SSC.t) =
    let val amps' = SSC.compactKeys amps
        val ssc = SSC.make { capacity = SSC.defaultPadding (DelayedSeq.length amps') }
        fun doB (b: B.t) = G.pull kern
                                  (fn b' =>
                                      ((if SSS.contains (substate, b') then
                                         SSC.update ssc (b, 1) else ()); C.zero)) b
        val _ = DelayedSeq.applyIdx amps' (fn (_, b) => (doB b; ()))
    in
      ssc
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeys
        (SSS.toKeys tgts,
         G.pull kern (fn b => Option.getOpt (SST.lookup (amps, b), C.zero)))

(*  fun pushApply ((kern, state) : G.t * SST.t) =
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
      end*)

  fun pullInfo ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeysWith
        (SSS.toKeys tgts,
         (fn b =>
             let val count = ref 0
                 fun get b = (count := !count + 1;
                              Option.getOpt (SST.lookup (amps, b), C.zero))
                 val amp = G.pull kern get b
             in
               (amp, !count)
             end))
        (0, op+)

  fun apply ((kern, state): G.t * SST.t) =
      let val pushFrom = SST.compactKeys state
          val pushed = push (kern, pushFrom)
          val (state', numEdges) = pullInfo (kern, state) pushed
          val numVerts = SST.nonEmptyKeys state'
      in
        { state = state',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun applySub ((kern, state, substate): G.t * SST.t * B.t DelayedSeq.t) =
      let val k = DelayedSeq.length substate
          (*val kbf = G.maxBranchingFactor kern*)

          val substate' = SSS.make { capacity = SSS.defaultPadding k }
          val _ = DelayedSeq.applyIdx substate (fn (_, b) => SSS.insert substate' (b, ()))

          val pushCounts = pushCount (kern, substate)
          val pullCounts = pullCount (kern, substate', pushCounts)
          val (pullAmps, edges) = pullInfo (kern, state) (SSS.fromKeys (SSC.toKeys pushCounts, fn _ => ()))
      in
        (DelayedSeq.reduce
           op+ 0.0
           (DelayedSeq.map
              (fn b => Real.fromInt (SSC.lookupElse (pushCounts, b, 0)) /
                       Real.fromInt (SSC.lookupElse (pullCounts, b, 0)))
              (SST.compactKeys pullAmps)) / Real.fromInt k,
         edges)
      end
        (*
          val pushed_size = SSC.size pushCounts

          val num_samples = Int.max (1, pushed_size div kbf)


          val subpushed = sampleStates (SSC.compactKeys pushed) num_samples
          val substate' = SSC.make { capacity = (DelayedSeq.length subpushed * 3) div 2 }
          val _ = DelayedSeq.applyIdx subpushed (fn (_, b) => SSC.insert substate' (b, 1))
          val (substate'', numEdges) = pullInfo (kern, state) substate'
          val numVerts = SST.nonEmptyKeys substate''
      in
        { state = substate'',
          numVerts = numVerts,
          numEdges = numEdges }
      end*)

  fun kernelizeThese (allGates: G.t Seq.t) (theseGates: gate_idx Seq.t) =
      let val gateCount = Seq.length theseGates
          fun mbf gidx = G.maxBranchingFactor (Seq.nth allGates gidx)
          fun iter (kerns: (gate_idx Seq.t) list) (acc: gate_idx list) (cbf: int) (i: int) =
              if i >= gateCount then
                List.rev (Seq.rev (Seq.fromList acc) :: kerns)
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
        iter nil (g0 :: nil) (mbf g0) 1
      end

  fun dumbFusion (gs: G.t Seq.t) =
      let val ks = Seq.fromList (kernelizeThese gs (Seq.tabulate (fn i => i) (Seq.length gs))) in
        Seq.map (G.fuse o Seq.map (Seq.nth gs)) ks
      end


  fun statusUpdate {size, numVerts, numEdges, numGates, gateCount} =
      print ("gate " ^ Int.toString numGates ^ "/" ^ Int.toString gateCount ^ ", "
             ^ Int.toString numVerts ^ " vertices, "
             ^ Int.toString numEdges ^ " edges, "
             ^ Int.toString size ^ " states, "
             ^ Real.fmt (StringCvt.FIX (SOME 8))
                        (Helpers.divPow2 (Real.fromInt size) numQubits) ^ " density\n")

  fun applyKernels (kerns: (gate_idx Seq.t) list)
                   (old as {state, numVerts, numEdges, numGates})
                   (info as {dfg = dfg, gateCount = gateCount,
                             visited = visited, gates = gates}) =
      case kerns of
          nil => old
        | kern :: remKerns =>
          let val _ = Helpers.forRange
                        (0, Seq.length kern)
                        (fn i => DataFlowGraphUtil.visit dfg (Seq.nth kern i) visited)
              val fusedKern = G.fuse (Seq.map (Seq.nth gates) kern)
              val {state = state',
                   numVerts = numVerts',
                   numEdges = numEdges'} = apply (fusedKern, state)
              val new = { state = state',
                          numVerts = numVerts + numVerts',
                          numEdges = numEdges + numEdges',
                          numGates = numGates + Seq.length kern }
          in
            statusUpdate { size = SST.size state',
                           numVerts = numVerts',
                           numEdges = numEdges',
                           numGates = numGates + Seq.length kern,
                           gateCount = gateCount };
            applyKernels remKerns new info
          end

  fun applyAll ((gates, state): G.t Seq.t * SST.t) (dfg: DataFlowGraph.t) =
      let fun lshift (x, n) = if n <= 0 then x else lshift (2 * x, n - 1)
          val visited = DataFlowGraphUtil.initState dfg
          val sampler = { gen = Random.rand (123, 456),
                          max_branching = lshift (1, schedSamplePathDepth),
                          num_samples = schedSamplePaths }
          val gateCount = Seq.length gates
          val numQubits = #numQubits dfg

          val mbf = G.maxBranchingFactor o Seq.nth gates


          fun bestPath (old as {state, numVerts, numEdges, numGates})
                       (paths: gate_idx Seq.t Seq.t) =
              if Seq.length paths = 1 then
                Seq.nth paths 0
              else
                let val substate = sampleStates (SST.compactKeys state) schedSampleStates
                    val stateEsts =
                        SeqBasis.tabulate
                          1 (0, Seq.length paths)
                          (fn i =>
                              (* TODO: maybe we shouldn't PURELY optimize for state size,
                               * but also consider the number of verts/edges inside? *)
                              (* TODO: also, need to make this a weighted expectation *)
                              applySub (G.fuse (Seq.map (Seq.nth gates) (Seq.nth paths i)),
                                        state, substate))
                    fun better (p1 as (i1, (relDensity1, edges1)))
                               (p2 as (i2, (relDensity2, edges2))) =
                        (*if relDensity1 < relDensity2 orelse
                           (Real.== (relDensity1, relDensity2) andalso edges1 < edges2) then*)
                        (*if edges1 < edges2 orelse (edges1 = edges2 andalso relDensity1 < relDensity2) then*)
                        if Real.fromInt edges1 * relDensity1 < Real.fromInt edges2 * relDensity2 then
                          p1
                        else
                          p2
                            
                    val (best, (relDensity, edges)) =
                        Array.foldri
                          (fn (i, sve, bestsve) => better (i, sve) bestsve)
                          (0, Array.sub (stateEsts, 0)) stateEsts
                in
                  print ("Best path has relative density = " ^ Real.fmt (StringCvt.FIX (SOME 8)) relDensity ^ "\n");
                  Seq.nth paths best
                end

          fun iter (old as {state, numVerts, numEdges, numGates}) =
              (* Note: length of paths might be less than
               * schedSamplePaths because we remove duplicate paths *)
              let val paths = DataFlowGraphUtil.samplePaths sampler dfg mbf visited in
                if Seq.length paths = 0 then
                  (* Nothing left to do; we're finished *)
                  { state = state, numVerts = numVerts, numEdges = numEdges }
                else
                  (* Kernelize gates on best path, apply them,
                   * then continue on with rest of circuit *)
                  iter (applyKernels (kernelizeThese gates (bestPath old paths)) old
                                     {dfg = dfg, gateCount = gateCount,
                                      visited = visited, gates = gates})
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
                in
                  print
                    ("kernel " ^ Int.toString (i + 1) ^ "/" ^ Int.toString numkerns ^ ", " ^
                     Int.toString numVerts' ^ " vertices, " ^
                     Int.toString numEdges' ^ " edges, " ^
                     Int.toString (SST.size state') ^ " states, " ^
                     Real.fmt (StringCvt.FIX (SOME 8))
                              (Helpers.divPow2 (Real.fromInt (SST.size state'))
                                               (#numQubits kern)) ^ " density\n");
                  iter (i + 1) new
                end
      in
        iter 0 {state = state, numVerts = 0, numEdges = 0}
      end
end
