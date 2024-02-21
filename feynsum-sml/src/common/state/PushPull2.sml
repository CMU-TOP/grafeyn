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

  datatype ST = Sparse of SST.t | Dense of C.r array

  val push: G.t * B.t DelayedSeq.t -> SSS.t
  val pull: G.t * SST.t -> SSS.t -> SST.t
  val pullInfo: G.t * SST.t -> SSS.t -> SST.t * int
  val applyAllOld: G.t Seq.t * SST.t -> {state: ST, numEdges: int}
  val unsafeViewContents: ST -> (B.t * C.t) option DelayedSeq.t
  val dumbFusion: G.t Seq.t -> G.t Seq.t
end =
struct
  structure B = B
  structure C = C
  structure G = Gate (structure B = B
                      structure C = C)

  type gate_idx = int

  structure SSS = SparseStateSet
                    (structure B = B
                     val numQubits = numQubits)
  structure SST = SparseStateTable
                    (structure B = B
                     structure C = C
                     val numQubits = numQubits)

  fun push ((kern, amps): G.t * B.t DelayedSeq.t) =
    let val cap = G.maxBranchingFactor kern * DelayedSeq.length amps
        val cap' = Int.min (Helpers.exp2 numQubits, cap)
        val sss = SSS.make { capacity = SSS.defaultPadding cap' }
        fun pushB b = G.push kern (fn x => SSS.insert sss (x, ())) b
        val _ = DelayedSeq.applyIdx amps (fn (_, b) => pushB b)
    in
      sss
    end

  fun pull ((kern, amps): G.t * SST.t) (tgts: SSS.t) =
      SST.fromKeys (SSS.toKeys tgts, G.pull kern (fn b => SST.lookupElse (amps, b, C.zero)))

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
        if Real.fromInt nonZeros < denseThreshold * Real.fromInt (Helpers.exp2 numQubits) then
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
      let val versionBox = Array.array (1, 0)
          fun versionCurrent (v): bool = Array.sub (versionBox, 0) = v
          fun versionOutdated (v): bool = not (versionCurrent v)
          (* tries to grab the `full` lock, returning if it was successful *)
          fun incVersion (v): bool =
              Helpers.bcas (versionBox, 0, v, v + 1)

          val cap = SST.capacity state

          (* block size configuration *)
          val blockSize = Int.max (100, Int.min (cap div 1000, blockSize))
          val numBlocks = Util.ceilDiv cap blockSize

          val blockProgress = SeqBasis.tabulate 5000 (0, numBlocks) (fn i => blockSize * i)

          fun blockStart b = Array.sub (blockProgress, b)
          fun blockStop b = Int.min (cap, (b + 1) * blockSize)

          fun pushB sss probes version b =
              (G.push kern (fn x => SSS.insertLimitProbes {probes = probes} sss (x, ())) b;
               versionCurrent version)
              handle SSS.Full => false

          val resultSSS = ref NONE

          fun spawnWorkers version =
              case !resultSSS of
                  NONE => NONE
                | SOME sss => 
                  let val probes = probeLimit (SSS.capacity sss)
                      val remainingBlocks =
                          SeqBasis.filter 1000 (0, numBlocks)
                                          (fn b => b) (fn b => blockStart b < blockStop b)
                      val numRemaining = Array.length remainingBlocks
                  in
                    if numRemaining = 0 then
                      SOME sss
                    else
                      (ForkJoin.parfor
                         1 (0, Array.length remainingBlocks)
                         (fn i => workOnBlock sss probes version (Array.sub (remainingBlocks, i)));
                       spawnWorkers (version + 1))
              end

          and workOnRegion sss probes version block bstart bend =
              (*(print ("work on region " ^ Int.toString (SSS.capacity sss) ^ ", " ^ Int.toString version ^ ", " ^ Int.toString block ^ ", " ^ Int.toString bstart ^ ", " ^ Int.toString bend ^ "\n");*)
              if bstart >= bend then
                (* block is complete *)
                Array.update (blockProgress, block, bstart)
              else if versionOutdated version then
                (* locked; store progress and stop *)
                Array.update (blockProgress, block, bstart)
              else
                case SST.sub (state, bstart) of
                    NONE => workOnRegion sss probes version block (bstart + 1) bend
                  | SOME (bidx, _) =>
                    if pushB sss probes version bidx then
                      workOnRegion sss probes version block (bstart + 1) bend
                    else
                      (* hashset full; try to grab lock *)
                      if incVersion version then
                        (* obtained lock! check if we should resize or just go dense *)
                        let val _ = Array.update (blockProgress, block, bstart)
                            val newcap = SSS.capacity sss * 2
                        in
                          if Real.fromInt newcap < denseThreshold * Real.fromInt (Helpers.exp2 numQubits) then
                            (resultSSS := SOME (SSS.resize sss newcap))
                          else
                            (* state set has crossed denseThreshold; stop *)
                            (resultSSS := NONE)
                        end
                      else
                        (* another thread has lock; store progress and stop *)
                        Array.update (blockProgress, block, bstart)

          and workOnBlock sss probes version block =
              workOnRegion sss probes version block (blockStart block) (blockStop block)

          val estsize = SST.estimateSize' {subsection = 0.1, minsection = 100} state
          val sss = SSS.make { capacity = SST.defaultPadding estsize }
      in
        (resultSSS := SOME sss;
         spawnWorkers 0)
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
