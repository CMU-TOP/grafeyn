structure FullSimBFS:
sig
  val run: Circuit.t -> (BasisIdx.t * Complex.t) DelayedSeq.t
end =
struct

  structure SST = SparseStateTable
  structure DS = DelayedSeq


  val queryBfsMaxBranchingStride =
    CommandLineArgs.parseInt "query-bfs-max-branching-stride" 1
  val _ = print
    ("query-bfs-max-branching-stride " ^ Int.toString queryBfsMaxBranchingStride
     ^ "\n")


  fun findNextGoal gates gatenum =
    let
      fun loop (i, branching) =
        if i >= Seq.length gates then
          (i, branching)
        else if Gate.expectBranching (Seq.nth gates i) then
          if branching >= queryBfsMaxBranchingStride then (i, branching)
          else loop (i + 1, branching + 1)
        else
          loop (i + 1, branching)
    in
      loop (gatenum, 0)
    end


  fun run {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()
      val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits))

      fun dumpDensity (i, nonZeroSize, capacity) =
        let
          val density = Real.fromInt nonZeroSize / Real.fromInt maxNumStates
          val slackPct =
            case capacity of
              NONE => "(none)"
            | SOME cap =>
                Int.toString (Real.ceil
                  (100.0 * (1.0 - Real.fromInt nonZeroSize / Real.fromInt cap)))
                ^ "%"
        in
          print
            ("gate " ^ Int.toString i ^ ": non-zeros: "
             ^ Int.toString nonZeroSize ^ "; slack: " ^ slackPct ^ "; density: "
             ^ Real.fmt (StringCvt.FIX (SOME 8)) density ^ "\n")
        end

      val impossibleBasisIdx = BasisIdx.flip BasisIdx.zeros 63

      fun makeNewState cap =
        SST.make {capacity = cap, maxload = 0.9, emptykey = impossibleBasisIdx}

      fun tryCapacity capacity (gatenum, goal) state =
        let
          val _ = print
            ("trying " ^ Int.toString gatenum ^ " -> " ^ Int.toString goal
             ^ " with capacity " ^ Int.toString capacity ^ "\n")

          (* val currentElems = SST.unsafeViewContents state *)
          val newState = makeNewState capacity

          fun doGates numGateApps i widx =
            if Complex.isZero (#2 widx) then
              numGateApps
            else if i >= goal then
              (SST.insertAddWeights newState widx; numGateApps)
            else
              case Gate.apply (gate i) widx of
                Gate.OutputOne widx' => doGates (numGateApps + 1) (i + 1) widx'
              | Gate.OutputTwo (widx1, widx2) =>
                  let val numGateApps = doGates (numGateApps + 1) (i + 1) widx1
                  in doGates numGateApps (i + 1) widx2
                  end

          val numGateApps =
            SeqBasis.reduce 10000 op+ 0 (0, DelayedSeq.length state) (fn i =>
              let val (bidx, weight) = DelayedSeq.nth state i
              in doGates 0 gatenum (bidx, weight)
              end)
        in
          SOME (numGateApps, newState)
        end
        handle SST.Full => NONE


      fun advanceTryCapacity capacity (i, goal) countGateApp state =
        case tryCapacity capacity (i, goal) state of
          NONE =>
            advanceTryCapacity (Real.ceil (2.0 * Real.fromInt capacity))
              (i, goal) countGateApp state
        | SOME (gateApps, newState) =>
            loopGuessCapacity goal (countGateApp + gateApps) newState


      and loopGuessCapacity next countGateApp state =
        let
          val capacityHere = SST.capacity state
          val nonZeros = SST.compact state
          val nonZeroSize = DelayedSeq.length nonZeros
          val _ = dumpDensity (next, nonZeroSize, SOME capacityHere)
        in
          if next >= depth then
            (countGateApp, nonZeros)
          else
            let
              val (goal, numBranchingUntilGoal) = findNextGoal gates next
              val multiplier = if numBranchingUntilGoal = 0 then 1.25 else 2.5
              val guess = Real.ceil (multiplier * Real.fromInt nonZeroSize)
              val guess = Int.min (guess, Real.ceil
                (1.25 * Real.fromInt maxNumStates))
            in
              advanceTryCapacity guess (next, goal) countGateApp nonZeros
            end
        end

      val initialState = makeNewState 1
      val _ =
        SST.insertAddWeights initialState (BasisIdx.zeros, Complex.real 1.0)

      val (totalGateApps, finalState) = loopGuessCapacity 0 0 initialState
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      finalState
    end
end
