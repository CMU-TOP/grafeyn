functor PushPull
  (structure B: BASIS_IDX
   structure C: COMPLEX
   val numQubits: int
   val maxBranchingStride: int
   val schedSamplePathDepth: int
   val schedSamplePaths: int
   val schedSampleMinStates: int
   val schedSampleStates: real
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

  datatype ST = Sparse of SST.t | Dense of C.r array

  val push: G.t * B.t DelayedSeq.t -> SSS.t
  val pushCount: G.t * B.t DelayedSeq.t -> SSC.t
  val pullCount: G.t * SSS.t * SSC.t -> SSC.t
  val pull: G.t * SST.t -> SSS.t -> SST.t
  val apply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val applySub: G.t * SST.t * B.t DelayedSeq.t -> real
  (*val pushApply: G.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}*)
  val pullInfo: G.t * SST.t -> SSS.t -> SST.t * int
  val applyAll: G.t Seq.t * SST.t -> DataFlowGraph.t
                -> {state: SST.t, numVerts: int, numEdges: int}
  val applyAllOldOld: G.t Seq.t * SST.t -> {state: SST.t, numVerts: int, numEdges: int}
  val applyAllOld: G.t Seq.t * SST.t -> {state: ST, numEdges: int}
  val unsafeViewContents: ST -> (B.t * C.t) option DelayedSeq.t
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
        val cap' = Int.min (Helpers.exp2 numQubits, cap)
        val sss = SSS.make { capacity = SSS.defaultPadding cap' }
        fun pushB b = G.push kern (fn x => SSS.insert sss (x, ())) b
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end

  fun pushCount ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val ssc = SSC.make { capacity = SSC.defaultPadding cap }
        (* fun doB' (b: B.t) = G.push kern (fn b' => SSC.update ssc (b', 1)) b *)
        fun doB (b: B.t) = let val cache = SSS.make { capacity = G.maxBranchingFactor kern } in G.push kern (fn b' => if not (SSS.contains (cache, b')) then (SSS.insert cache (b', ()); SSC.update ssc (b', 1)) else ()) b end
        val _ = DelayedSeq.applyIdx amps (doB o #2)
    in
      ssc
    end

  fun pullCount ((kern, substate, amps): G.t * SSS.t * SSC.t) =
    let val amps' = SSC.compactKeys amps
        val ssc = SSC.make { capacity = SSC.defaultPadding (DelayedSeq.length amps') }
        fun doB (b: B.t) =
            let val cache = SSS.make { capacity = G.maxBranchingFactor kern } in
              G.pull kern
                     (fn b' => if not (SSS.contains (cache, b')) then
                                 (SSS.insert cache (b', ());
                                  (if SSS.contains (substate, b') then
                                     SSC.update ssc (b, 1) else ());
                                  C.zero)
                               else
                                 C.zero) b
            end
        (* fun doB (b: B.t) = G.pull kern *)
        (*                           (fn b' => *)
        (*                               ((if SSS.contains (substate, b') then *)
        (*                                  SSC.update ssc (b, 1) else ()); C.zero)) b *)
        val _ = DelayedSeq.applyIdx amps' (fn (_, b) => (doB b; ()))
    in
      ssc
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeys (SSS.toKeys tgts, G.pull kern (fn b => SST.lookupElse (amps, b, C.zero)))

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

  datatype ST = Sparse of SST.t | Dense of C.r array

  fun denseToSparse (state: C.r array) (nonZeros: int) =
      let val sst = SST.make { capacity = SST.defaultPadding nonZeros }
          val _ = ForkJoin.parfor
                    1000
                    (0, Helpers.exp2 numQubits)
                    (fn i => let val re = Array.sub (state, 2 * i)
                                 val im = Array.sub (state, 2 * i + 1)
                                 val a = C.make (re, im) in
                               if C.isNonZero a then
                                 SST.insert sst (B.fromDenseIndex i, a)
                               else
                                 ()
                             end)
      in
        sst
      end

  fun applyDense' ((kern, get): G.t * (B.t -> C.t)): ST =
      let val size = Helpers.exp2 numQubits
          val state' = ForkJoin.alloc (2 * size)
          fun pullK b = G.pull kern get b
          fun pullI i = let val a = pullK (B.fromDenseIndex i)
                            val (re, im) = C.view a in
                          Array.update (state', 2 * i, re);
                          Array.update (state', 2 * i + 1, im);
                          if C.isNonZero a then 1 else 0
                        end
          val nonZeros = SeqBasis.reduce 1000 op+ 0 (0, size) pullI
      in
        if IntInf.fromInt nonZeros
           < Helpers.riMult denseThreshold (Helpers.exp2inf numQubits) then
          Sparse (denseToSparse state' nonZeros)
        else
          Dense state'
      end

  fun applyDense ((kern, state): G.t * C.r array) =
      let fun get b = let val i = B.toDenseIndex b in
                        C.make (Array.sub (state, 2 * i), Array.sub (state, 2 * i + 1))
                      end in
        applyDense' (kern, get)
      end


  (*fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val cap' = Int.min (Helpers.exp2 numQubits, cap)
        val sss = SSS.make { capacity = SSS.defaultPadding cap' }
        fun pushB b = G.push kern (fn x => SSS.insert sss (x, ())) b
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end*)

  (*
   * We use here a trick to avoid needing to scan the whole table to figure out
   * the load factor. The idea is: if a single insertion takes a long time,
   * then we can assume the load factor is high.
   *
   * At load factor `alpha`, the longest probe sequence is probably going to
   * be approximately 
   *   log(n)/(alpha - 1 - log(alpha))
   * Analysis in this paper:
   *   Linear Probing: The Probable Largest Search Time Grows Logarithmically
   *   with the Number of Records
   *   Author: B. Pittel
   *
   * So, if number of probes witnessed by a single insertion is nearby this
   * amount, then the table is probably nearby `alpha` load factor, so it's
   * time to back off and resize the table.
   *
   * (This seems to work decently, but it's possible we could do better. Need
   * to analyze the probability of the load factor being high, given the
   * observation of a single long probe sequence.)
   *)
  fun probeLimit cap =
      let val probablyLongestProbe = Real.ceil (Helpers.log2 (Real.fromInt cap) /
                                                (maxload - 1.0 - Helpers.log2 maxload)) in
        Int.min (cap, 4 * Int.max (10, probablyLongestProbe))
      end

  fun pushGradually ((kern, state): G.t * SST.t): SSS.t option =
      let val full = Array.array (1, (0w0 : Word8.word))
          fun notFull (): bool = Array.sub (full, 0) = 0w0
          fun isFull (): bool = not (notFull ())
          (* tries to grab the `full` lock, returning if it was successful *)
          fun markFull (): bool = Helpers.bcas (full, 0, 0w0, 0w1)

          val cap = SST.capacity state

          (* block size configuration *)
          val blockSize = Int.max (100, Int.min (cap div 1000, blockSize))
          val numBlocks = Util.ceilDiv cap blockSize

          val blockProgress = SeqBasis.tabulate 5000 (0, numBlocks) (fn i => blockSize * i)
          (* Last block might not have blockSize elements if cap mod blockSize <> 0 *)
          val _ = Array.update (blockProgress, numBlocks - 1, cap - (numBlocks - 1)*blockSize)

          fun blockStart b = Array.sub (blockProgress, b)
          fun blockStop b = Int.min (cap, (b + 1) * blockSize)
          fun blockFinished b = blockStart b >= blockStop b
          fun blockHasRemaining b = blockStart b < blockStop b

          fun pushB sss probes b =
              (G.push kern (fn x => SSS.insertLimitProbes {probes = probes} sss (x, ())) b;
               true)
              handle SSS.Full => false

          val resultSSS = ref NONE

          fun spawnWorkers sss =
              let val probes = probeLimit (SSS.capacity sss)
                  val remainingBlocks =
                      SeqBasis.filter 1000 (0, numBlocks) (fn i => i) blockHasRemaining
                  val numRemaining = Array.length remainingBlocks
              in
                if numRemaining = 0 then
                  (resultSSS := SOME sss)
                else
                  ForkJoin.parfor
                    1 (0, Array.length remainingBlocks)
                    (fn i => workOnBlock sss probes (Array.sub (remainingBlocks, i)))
              end

          and workOnRegion sss probes block bstart bend =
              if bstart >= bend then
                (* block is complete *)
                ()
              else if isFull () then
                (* locked; store progress and stop *)
                Array.update (blockProgress, block, bstart)
              else
                case SST.sub (state, bstart) of
                    NONE => workOnRegion sss probes block (bstart + 1) bend
                  | SOME (bidx, _) =>
                    if pushB sss probes bidx then
                      workOnRegion sss probes block (bstart + 1) bend
                    else
                      (* hashset full; try to grab lock *)
                      if markFull () then
                        (* obtained lock! check if we should resize or just go dense *)
                        let val _ = Array.update (blockProgress, block, bstart)
                            val newcap = SSS.capacity sss * 2 in
                          if IntInf.fromInt newcap < Helpers.riMult denseThreshold (Helpers.exp2inf numQubits) then
                            spawnWorkers (SSS.resize sss newcap)
                          else
                            (* state set has crossed denseThreshold; stop *)
                            ()
                        end
                      else
                        (* another thread has lock; store progress and stop *)
                        Array.update (blockProgress, block, bstart)

          and workOnBlock sss probes block =
              workOnRegion sss probes block (blockStart block) (blockStop block)

          val estsize = SST.estimateSize' {subsection = 0.1, minsection = 100} state
          val sss = SSS.make { capacity = SST.defaultPadding estsize }
      in
        spawnWorkers sss;
        !resultSSS
      end

  fun applyDS ((kern, state) : G.t * ST): ST * int =
      case state of
          Dense dst =>
          (applyDense (kern, dst), G.maxBranchingFactor kern * Helpers.exp2 numQubits)
        | Sparse sst =>
          case pushGradually (kern, sst) of
              NONE =>
              (* Cancelled push because it crossed denseThreshold *)
              (applyDense' (kern, fn b => SST.lookupElse (sst, b, C.zero)),
               G.maxBranchingFactor kern * Helpers.exp2 numQubits)
            | SOME sss =>
              let val (sst', edges) = pullInfo (kern, sst) sss in
                (Sparse sst', edges)
              end

  (* TODO: don't construct intermediate DelayedSeq with SST.compact,
   * instead just pass the whole SST to push *)
  fun apply ((kern, state): G.t * SST.t): {state: SST.t, numVerts: int, numEdges: int} =
      let val pushFrom = SST.compactKeys state
          val pushed = push (kern, pushFrom)
          val (state', numEdges) = pullInfo (kern, state) pushed
          val numVerts = SST.nonEmptyKeys state'
      in
        { state = state',
          numVerts = numVerts,
          numEdges = numEdges }
      end

  fun applySub' ((kern, state, substate): G.t * SST.t * B.t DelayedSeq.t) =
      let val k = DelayedSeq.length substate

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
                       Real.fromInt (SSC.lookupElse (pullCounts, b, 1)))
              (SST.compactKeys pullAmps)) / Real.fromInt k,
         edges)
      end

  fun applySub ((kern, state, substate): G.t * SST.t * B.t DelayedSeq.t) =
      let val k = DelayedSeq.length substate
          val kbf = G.maxBranchingFactor kern
          val pushed = push (kern, substate)

          val subpushed = sampleStates (SSS.compactKeys pushed) k
          val substate' = SSS.make { capacity = SSS.defaultPadding k }
          val _ = DelayedSeq.applyIdx subpushed (fn (_, b) => SSS.insert substate' (b, ()))

          val pulled = pull (kern, state) pushed

          val numPushed = SSS.size pushed
          val numPulled = SST.size pulled
          val numPushedR = Real.fromInt numPushed
          val numPulledR = Real.fromInt numPulled
          val kR = Real.fromInt k
      in
        numPulledR * numPushedR / Real.min (numPushedR, kR)
      end

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


  fun bestPath (gates: G.t Seq.t)
               (old as {state, numVerts, numEdges, numGates})
               (paths: gate_idx Seq.t Seq.t) =
      if Seq.length paths = 1 then
        Seq.nth paths 0
      else
        let val numkeys = SST.estimateSize {subsection = schedSampleMinStates} state in
          if numkeys <= schedSampleMinStates * schedSamplePaths then
            (print ("Skipping sampling, estimated " ^ Int.toString numkeys ^ " states\n");
             Seq.nth paths 0)
          else
            let val keys = SST.compactKeys state
                val numkeys = DelayedSeq.length keys
                val k' = Real.floor (Real.fromInt numkeys * schedSampleStates)
                (* Clamp k within [schedSampleMinStates, size of SST] *)
                val k = Int.min (Int.max (k', schedSampleMinStates), numkeys)
                val substate = sampleStates keys k
                val stateEsts =
                    SeqBasis.tabulate
                      1 (0, Seq.length paths)
                      (fn i =>
                          (* TODO: maybe we shouldn't PURELY optimize for state size,
                           * but also consider the number of verts/edges inside? *)
                          applySub (G.fuse (Seq.map (Seq.nth gates) (Seq.nth paths i)),
                                    state, substate))
                (*fun better (p1 as (i1, (relDensity1, edges1)))
                       (p2 as (i2, (relDensity2, edges2))) =
                let val densityRatio = relDensity1 / relDensity2 in
                  if densityRatio <= 0.9 then
                    p1
                  else if 0.9 < densityRatio andalso densityRatio < 1.11111
                          andalso 10 * edges1 < 9 * edges2 then
                    p1
                  else
                    p2
                end*)
                fun better (p1 as (i1, r1)) (p2 as (i2, r2)) =
                    if r1 < r2 then p1 else p2

                (*val _ = print ("Average path out of " ^ Int.toString (Array.length stateEsts) ^ " paths has relative density = " ^ Real.fmt (StringCvt.FIX (SOME 8)) (Array.foldri (fn (i, (d, _), d') => d + d') 0.0 stateEsts / Real.fromInt (Array.length stateEsts)) ^ "\n")*)
                                              
                val (best, _) =
                    Array.foldri
                      (fn (i, sve, bestsve) => better (i, sve) bestsve)
                      (0, Array.sub (stateEsts, 0)) stateEsts
            in
              (*print ("Best path has relative density = " ^ Real.fmt (StringCvt.FIX (SOME 8)) relDensity ^ "\n");*)
              Seq.nth paths best
            end
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
                  iter (applyKernels (kernelizeThese gates (bestPath gates old paths)) old
                                     {dfg = dfg, gateCount = gateCount,
                                      visited = visited, gates = gates})
              end
      in
        iter { state = state,
               numVerts = 0,
               numEdges = 0,
               numGates = 0 }
      end
      

  fun applyAllOldOld ((kerns, state): G.t Seq.t * SST.t) =
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

  fun applyAllOld ((kerns, state): G.t Seq.t * SST.t) =
      let val numkerns = Seq.length kerns
          fun iter i (old as {state, numEdges}) =
              if i >= numkerns then
                old
              else
                let val kern = Seq.nth kerns i
                    val (state', numEdges') = applyDS (kern, state)
                    val new = { state = state', numEdges = numEdges + numEdges' }
                    val estsize = case state' of
                                      Dense dst => Helpers.exp2 numQubits
                                    | Sparse sst => SST.estimateSize' {subsection = 0.1, minsection = 100} sst
                in
                  print
                    ("kernel " ^ Int.toString (i + 1) ^ "/" ^ Int.toString numkerns ^ ", " ^
                     Int.toString numEdges' ^ " edges, " ^
                     Int.toString estsize ^ " states (estimated), " ^
                     Real.fmt (StringCvt.FIX (SOME 8))
                              (Helpers.divPow2 (Real.fromInt estsize) numQubits)
                     ^ " density\n");
                  iter (i + 1) new
                end
      in
        iter 0 {state = Sparse state, numEdges = 0}
      end

  fun unsafeViewContents (state: ST) =
      case state of
          Dense dst => DelayedSeq.tabulate
                         (fn i => let val c = C.make (Array.sub (dst, 2 * i),
                                                      Array.sub (dst, 2 * i + 1)) in
                                    if C.isNonZero c then
                                      SOME (B.fromDenseIndex i, c )
                                    else
                                      NONE
                                  end)
                         (Helpers.exp2 numQubits)
        | Sparse sst => SST.unsafeViewContents sst
end
