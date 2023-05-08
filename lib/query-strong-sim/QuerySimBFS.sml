structure QuerySimBFS:
sig
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  structure SST = SparseStateTable
  structure DS = DelayedSeq


  fun nextBranchingGate gates gatenum =
    if gatenum >= Seq.length gates then NONE
    else if Gate.expectBranching (Seq.nth gates gatenum) then SOME gatenum
    else nextBranchingGate gates (gatenum + 1)


  fun query {numQubits, gates} desired =
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
          (* val currentElems = SST.unsafeViewContents state *)
          val newState = makeNewState capacity

          fun doGates i widx =
            if Complex.isZero (#2 widx) then
              ()
            else if i >= goal then
              SST.insertAddWeights newState widx
            else
              case Gate.apply (gate i) widx of
                Gate.OutputOne widx' => doGates (i + 1) widx'
              | Gate.OutputTwo (widx1, widx2) =>
                  (doGates (i + 1) widx1; doGates (i + 1) widx2)

          val numGateApps = (goal - gatenum) * DelayedSeq.length state

          val _ = ForkJoin.parfor 10000 (0, DelayedSeq.length state) (fn i =>
            let val (bidx, weight) = DelayedSeq.nth state i
            in doGates gatenum (bidx, weight)
            end)
        in
          SOME (numGateApps, newState)
        end
        handle SST.Full => NONE


      fun advanceTryCapacity capacity (i, goal) countGateApp state =
        case tryCapacity capacity (i, goal) state of
          NONE =>
            ( print "upping capacity...\n"
            ; advanceTryCapacity (Real.ceil (1.25 * Real.fromInt capacity))
                (i, goal) countGateApp state
            )
        | SOME (gateApps, newState) =>
            loopGuessCapacity (i, goal) (countGateApp + gateApps) newState


      and loopGuessCapacity (prev, next) countGateApp state =
        let
          val capacityHere = SST.capacity state
          val nonZeros = SST.compact state
          val nonZeroSize = DelayedSeq.length nonZeros
          (* val nonZeroSize = SST.nonZeroSize state *)
          val _ = Util.for (prev + 1, next) (fn i =>
            print ("gate " ^ Int.toString i ^ ": non-branching!\n"))
          val _ = dumpDensity (next, nonZeroSize, SOME capacityHere)
        in
          if next >= depth then
            (countGateApp, nonZeros)
          else
            let
              val (goal, multiplier) =
                case nextBranchingGate gates next of
                  NONE => (depth, 1.25)
                | SOME goal => (goal + 1, 2.5)
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

      val (totalGateApps, finalState) = loopGuessCapacity (~1, 0) 0 initialState
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")

      val output =
        SeqBasis.reduce 10000 Complex.+ Complex.zero
          (0, DelayedSeq.length finalState)
          (fn i =>
             let val (bidx, weight) = DelayedSeq.nth finalState i
             in if BasisIdx.equal (bidx, desired) then weight else Complex.zero
             end)
    in
      output
    end
end
