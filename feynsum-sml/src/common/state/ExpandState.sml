functor ExpandState
  (structure HS: HYBRID_STATE
   structure G: GATE
   sharing HS.SST.B = HS.DS.B = G.B
   sharing HS.SST.C = HS.DS.C = G.C
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real) :>
sig

  val expand:
    { gates: G.t Seq.t
    , numQubits: int
    , maxNumStates: IntInf.int
    , state: HS.state
    , prevNonZeroSize: int
    }
    -> {result: HS.state, method: string, numNonZeros: int, numGateApps: int}

end =
struct

  structure B = G.B
  structure C = G.C

  (* tryPut will fail (and return false) if the table is full, or almost full.
   *
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
  fun tryPut table widx =
    let
      val n = HS.SST.capacity table
      val probablyLongestProbe = Real.ceil
        (MathHelpers.log2 (Real.fromInt n) / (maxload - 1.0 - MathHelpers.log2 maxload))
      val tolerance = 4 * Int.max (10, probablyLongestProbe)
      val tolerance = Int.min (tolerance, n)
    in
      HS.SST.insertAddWeightsLimitProbes {probes = tolerance} table widx;
      true
    end
    handle HS.SST.Full => false


  datatype successors_result =
    AllSucceeded
  | SomeFailed of {widx: B.t * C.t, gatenum: int} list

  fun expandSparse {gates: G.t Seq.t, numQubits, state, expected} =
    let
      val numGates = Seq.length gates
      fun gate i = Seq.nth gates i

      val stateSeq =
        case state of
          HS.Sparse sst => DelayedSeq.map SOME (HS.SST.compact sst)
        | HS.Dense state => HS.DS.unsafeViewContents state
        | HS.DenseKnownNonZeroSize (state, _) => HS.DS.unsafeViewContents state

      (* number of initial elements *)
      val n = DelayedSeq.length stateSeq

      (* block size configuration *)
      val blockSize = Int.min (n div 1000, blockSize)
      val blockSize = Int.max (100, blockSize)
      val numBlocks = Util.ceilDiv n blockSize
      fun blockStart b = blockSize * b
      fun blockStop b =
        Int.min (n, blockSize + blockStart b)

      (* each block keeps track of how many elements have been completed, and
       * any pending insertions (from a previous failure that triggered a resizing)
       *)
      val blockRemainingStarts =
        SeqBasis.tabulate 5000 (0, numBlocks) blockStart
      fun blockHasRemaining b =
        Array.sub (blockRemainingStarts, b) < blockStop b

      val blockPending = SeqBasis.tabulate 5000 (0, numBlocks) (fn _ => [])
      fun blockHasPending b =
        not (List.null (Array.sub (blockPending, b)))
      fun blockPopPending b =
        case Array.sub (blockPending, b) of
          [] => NONE
        | x :: rest => (Array.update (blockPending, b, rest); SOME x)
      fun blockPushPending b pending =
        case Array.sub (blockPending, b) of
          [] => Array.update (blockPending, b, pending)
        | xx => Array.update (blockPending, b, pending @ xx)

      (* fun blockHasPending b = false
      fun blockPopPending b = NONE
      fun blockPushPending b _ = () *)

      (* remainingBlocks: list of block ids that aren't finished yet
       * table: place to put results; this will fill up and need resizing
       *)
      fun loop numGateApps remainingBlocks table =
        let
          val full = ref (0w0 : Word8.word)
          fun notFull () =
            (!full = 0w0)
          fun markFull () = (full := 0w1)

          (* fun notFull () = true
          fun markFull () = () *)

          (* fun doGates (widx, gatenum) =
            if C.isZero (#2 widx) then
              AllSucceeded
            else if gatenum >= numGates then
              if tryPut table widx then AllSucceeded else Util.die ("uh oh!")
            else
              case apply (gatenum, widx) of
                G.OutputOne widx' => doGates (widx', gatenum + 1)
              | G.OutputTwo (widx1, widx2) =>
                  (doGates (widx1, gatenum + 1); doGates (widx2, gatenum + 1)) *)

          (* try insert all successors of `(widx, gatenum)` into `table` *)
          fun doGates apps (widx, gatenum) : int * successors_result =
            if C.isZero (#2 widx) then
              (apps, AllSucceeded)
            else if gatenum >= numGates then
              if notFull () andalso tryPut table widx then
                (apps, AllSucceeded)
              else
                ( if notFull () then markFull () else ()
                ; (apps, SomeFailed [{widx = widx, gatenum = gatenum}])
                )
            else
              case #action (gate gatenum) of
                G.NonBranching apply =>
                  doGates (apps + 1) (apply widx, gatenum + 1)
              | G.Branching apply => doTwo (apps + 1) (apply widx, gatenum + 1)
              | G.MaybeBranching apply =>
                  case apply widx of
                    G.OutputOne widx' => doGates (apps + 1) (widx', gatenum + 1)
                  | G.OutputTwo (widx1, widx) =>
                      doTwo (apps + 2) ((widx1, widx), gatenum + 1)

          and doTwo apps ((widx1, widx2), gatenum) =
            case doGates apps (widx1, gatenum) of
              (apps, AllSucceeded) => doGates apps (widx2, gatenum)
            | (apps, SomeFailed failures) =>
                ( apps
                , SomeFailed ({widx = widx2, gatenum = gatenum} :: failures)
                )


          fun workOnBlock b =
            let
              val start = Array.sub (blockRemainingStarts, b)
              val stop = blockStop b

              fun clearPending apps =
                case blockPopPending b of
                  NONE => (apps, true)
                | SOME {widx, gatenum} =>
                    case doGates apps (widx, gatenum) of
                      (apps, AllSucceeded) => clearPending apps
                    | (apps, SomeFailed failures) =>
                        (blockPushPending b failures; (apps, false))

              fun loop apps i =
                if i >= stop then
                  (Array.update (blockRemainingStarts, b, stop); apps)
                else
                  case DelayedSeq.nth stateSeq i of
                    NONE => loop apps (i + 1)
                  | SOME elem =>
                      case doGates apps (elem, 0) of
                        (apps, AllSucceeded) => loop apps (i + 1)
                      | (apps, SomeFailed failures) =>
                          ( Array.update (blockRemainingStarts, b, i + 1)
                          ; blockPushPending b failures
                          ; apps
                          )

              val (apps, pendingCleared) = clearPending 0
            in
              if pendingCleared then loop apps start else apps
            end


          (* push through blocks *)
          val apps =
            SeqBasis.reduce 1 op+ 0 (0, Seq.length remainingBlocks) (fn bi =>
              let val b = Seq.nth remainingBlocks bi
              in workOnBlock b
              end)

          val remainingBlocks' =
            Seq.filter (fn b => blockHasPending b orelse blockHasRemaining b)
              remainingBlocks

          val numGateApps' = numGateApps + apps
        in
          if Seq.length remainingBlocks' = 0 then
            (numGateApps', table)
          else
            ( (*print
                ("growing from " ^ Int.toString (HS.SST.capacity table) ^ " to "
                 ^
                 Int.toString (Real.ceil
                   (1.5 * Real.fromInt (HS.SST.capacity table))) ^ "\n")
              ;*)
              loop numGateApps' remainingBlocks'
                (HS.SST.increaseCapacityByFactor 1.5 table))
        end

      val initialCapacity = Real.ceil
        (1.1 * (1.0 / maxload) * Real.fromInt (IntInf.toInt expected))
      val initialTable =
        HS.SST.make {capacity = initialCapacity, numQubits = numQubits}
      val initialBlocks = Seq.tabulate (fn b => b) numBlocks

      val (apps, output) = loop 0 initialBlocks initialTable
    in
      {result = HS.Sparse output, numGateApps = apps}
    end


  fun expandPushDense {gates: G.t Seq.t, numQubits, state, expected: IntInf.int} =
    let
      val numGates = Seq.length gates
      fun gate i = Seq.nth gates i

      val stateSeq =
        case state of
          HS.Sparse sst => DelayedSeq.map SOME (HS.SST.compact sst)
        | HS.Dense state => HS.DS.unsafeViewContents state
        | HS.DenseKnownNonZeroSize (state, _) => HS.DS.unsafeViewContents state

      (* number of initial elements *)
      val n = DelayedSeq.length stateSeq

      val output = HS.DS.make {numQubits = numQubits}
      fun put widx = HS.DS.insertAddWeights output widx

      fun doGates apps (widx, gatenum) =
        if C.isZero (#2 widx) then
          apps
        else if gatenum >= numGates then
          (put widx; apps)
        else
          case #action (gate gatenum) of
            G.NonBranching apply => doGates (apps + 1) (apply widx, gatenum + 1)
          | G.Branching apply => doTwo (apps + 1) (apply widx, gatenum + 1)
          | G.MaybeBranching apply =>
              case apply widx of
                G.OutputOne widx' => doGates (apps + 1) (widx', gatenum + 1)
              | G.OutputTwo (widx1, widx) =>
                  doTwo (apps + 1) ((widx1, widx), gatenum + 1)

      and doTwo apps ((widx1, widx2), gatenum) =
        let val apps = doGates apps (widx1, gatenum)
        in doGates apps (widx2, gatenum)
        end

      val numGateApps = SeqBasis.reduce blockSize op+ 0 (0, n) (fn i =>
        case DelayedSeq.nth stateSeq i of
          SOME widx => doGates 0 (widx, 0)
        | NONE => 0)
    in
      {result = HS.Dense output, numGateApps = numGateApps}
    end


  fun expandPullDense {gates: G.t Seq.t, numQubits, state, expected: IntInf.int} =
    let
      val actions = Seq.map (valOf o G.pullAction) gates
      fun action i = Seq.nth actions i
      val numGates = Seq.length gates

      val lookup =
        case state of
          HS.Sparse sst => (fn bidx => Option.getOpt (HS.SST.lookup sst bidx, C.zero))
        | HS.Dense ds => HS.DS.lookupDirect ds
        | HS.DenseKnownNonZeroSize (ds, _) => HS.DS.lookupDirect ds

      fun doGates (bidx, gatenum) =
        if gatenum < 0 then
          {weight = lookup bidx, count = 0}
        else
          case action gatenum of
            G.PullNonBranching apply =>
              let
                val (neighbor, mult) = apply bidx
                val {weight, count} = doGates (neighbor, gatenum - 1)
              in
                {weight = C.* (mult, weight), count = 1 + count}
              end

          | G.PullBranching apply =>
              let
                val ((neighbor1, mult1), (neighbor2, mult2)) = apply bidx
                val {weight = w1, count = c1} = doGates (neighbor1, gatenum - 1)
                val {weight = w2, count = c2} = doGates (neighbor2, gatenum - 1)
                val w1 = C.* (mult1, w1)
                val w2 = C.* (mult2, w2)
              in
                {weight = C.+ (w1, w2), count = 1 + c1 + c2}
              end

      val {result, totalCount, nonZeroSize} =
        HS.DS.pull {numQubits = numQubits} (fn bidx =>
          doGates (bidx, numGates - 1))
    in
      { result = HS.DenseKnownNonZeroSize (result, nonZeroSize)
      , numGateApps = totalCount
      }
    end


  fun expand (xxx as {gates, numQubits, maxNumStates, state, prevNonZeroSize}) =
    let
      val nonZeroSize =
        case state of
          HS.Sparse sst => HS.SST.nonZeroSize sst
        | HS.Dense ds => HS.DS.nonZeroSize ds
        | HS.DenseKnownNonZeroSize (_, nz) => nz

      val rate = Real.max
        (1.0, Real.fromInt nonZeroSize / Real.fromInt prevNonZeroSize)
      val expected = Real.ceil (rate * Real.fromInt nonZeroSize)
      val expected = IntInf.min (IntInf.fromInt expected, maxNumStates)
      val expectedCost = IntInf.max (IntInf.fromInt nonZeroSize, expected)

      fun allGatesPullable () =
        Util.all (0, Seq.length gates) (G.pullable o Seq.nth gates)

      val args =
        { gates = gates
        , numQubits = numQubits
        , state = state
        , expected = expected
        }

      val (method, {result, numGateApps}) =
        if
          denseThreshold >= 1.0 orelse expectedCost < MathHelpers.riMult denseThreshold maxNumStates
        then
          ("push sparse", expandSparse args)

        else if
          expectedCost >= MathHelpers.riMult pullThreshold maxNumStates
          andalso allGatesPullable ()
        then
          ("pull dense", expandPullDense args)

        else
          ("push dense", expandPushDense args)
    in
      { result = result
      , method = method
      , numNonZeros = nonZeroSize
      , numGateApps = numGateApps
      }
    end

end