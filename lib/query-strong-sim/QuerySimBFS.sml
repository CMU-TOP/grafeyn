structure QuerySimBFS:
sig
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  structure SST = SparseStateTable
  structure DS = DelayedSeq

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
          val slackPct = Real.ceil
            (100.0 * (1.0 - Real.fromInt nonZeroSize / Real.fromInt capacity))
        in
          print
            ("gate " ^ Int.toString i ^ ": non-zeros: "
             ^ Int.toString nonZeroSize ^ "; slack: " ^ Int.toString slackPct
             ^ "%" ^ "; density: " ^ Real.fmt (StringCvt.FIX (SOME 8)) density
             ^ "\n")
        end

      val impossibleBasisIdx = BasisIdx.flip BasisIdx.zeros 63

      fun makeNewState cap =
        SST.make {capacity = cap, maxload = 0.9, emptykey = impossibleBasisIdx}

      fun tryCapacity capacity i state =
        let
          (* val currentElems = SST.unsafeViewContents state *)
          val newState = makeNewState capacity

          fun doGate widx =
            case Gate.apply (gate i) widx of
              Gate.OutputOne widx' => SST.insertAddWeights newState widx'
            | Gate.OutputTwo (widx1, widx2) =>
                ( SST.insertAddWeights newState widx1
                ; SST.insertAddWeights newState widx2
                )

          val numGateApps = DelayedSeq.length state

          val _ = ForkJoin.parfor 10000 (0, DelayedSeq.length state) (fn i =>
            let val (bidx, weight) = DelayedSeq.nth state i
            in doGate (bidx, weight)
            end)
        in
          SOME (numGateApps, newState)
        end
        handle SST.Full => NONE


      fun loopTryCapacity capacity i countGateApp state =
        case tryCapacity capacity i state of
          NONE =>
            ( print "upping capacity...\n"
            ; loopTryCapacity (Real.ceil (1.25 * Real.fromInt capacity)) i
                countGateApp state
            )
        | SOME (gateApps, newState) =>
            loopGuessCapacity (i + 1) (countGateApp + gateApps) newState


      and loopGuessCapacity i countGateApp state =
        let
          val capacityHere = SST.capacity state
          val nonZeros = SST.compact state
          val nonZeroSize = DelayedSeq.length nonZeros
          (* val nonZeroSize = SST.nonZeroSize state *)
          val _ = dumpDensity (i, nonZeroSize, capacityHere)
        in
          if i >= depth then
            (countGateApp, nonZeros)
          else
            let
              val multiplier =
                if Gate.expectBranching (gate i) then 2.5 else 1.25
              val guess = Real.ceil (multiplier * Real.fromInt nonZeroSize)
              val guess = Int.min (guess, Real.ceil
                (1.25 * Real.fromInt maxNumStates))
            in
              loopTryCapacity guess i countGateApp nonZeros
            end
        end

      val initialState = makeNewState 1
      val _ =
        SST.insertAddWeights initialState (BasisIdx.zeros, Complex.real 1.0)

      val (totalGateApps, finalState) = loopGuessCapacity 0 0 initialState
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
