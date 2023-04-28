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

      val impossibleBasisIdx = BasisIdx.flip BasisIdx.zeros 63

      fun makeNewState cap =
        SST.make {capacity = cap, maxload = 0.75, emptykey = impossibleBasisIdx}

      fun loopTryCapacity capacity i countGateApp state =
        let
          val currentElems = SST.unsafeViewContents state
          val newState = makeNewState capacity

          fun doGate widx =
            case Gate.apply (gate i) widx of
              Gate.OutputOne widx' => SST.insertAddWeights newState widx'
            | Gate.OutputTwo (widx1, widx2) =>
                ( SST.insertAddWeights newState widx1
                ; SST.insertAddWeights newState widx2
                )

          val numGateApps =
            SeqBasis.reduce 100 op+ 0 (0, DS.length currentElems) (fn i =>
              case DS.nth currentElems i of
                NONE => 0
              | SOME (bidx, weight) =>
                  if Complex.isNonZero weight then (doGate (bidx, weight); 1)
                  else 0)
        in
          loopGuessCapacity (i + 1) (countGateApp + numGateApps) newState
        end
        handle SST.Full => loopTryCapacity (2 * capacity) i countGateApp state


      and loopGuessCapacity i countGateApp state =
        if i >= depth then
          (countGateApp, state)
        else
          let
            val currentElems = SST.unsafeViewContents state
            val nonZeroSize =
              SeqBasis.reduce 1000 op+ 0 (0, DS.length currentElems) (fn i =>
                case DS.nth currentElems i of
                  NONE => 0
                | SOME (bidx, weight) =>
                    if Complex.isNonZero weight then 1 else 0)

            val density = Real.fromInt nonZeroSize / Real.fromInt maxNumStates
            val _ = print
              ("gate " ^ Int.toString i ^ ": non-zeros: "
               ^ Int.toString nonZeroSize ^ "; density: "
               ^ Real.fmt (StringCvt.FIX (SOME 8)) density ^ "\n")
            val _ = TextIO.flushOut TextIO.stdOut

            val multiplier = if Gate.expectBranching (gate i) then 3.0 else 1.5
            val guess = Real.ceil (multiplier * Real.fromInt nonZeroSize)
          in
            loopTryCapacity guess i countGateApp state
          end

      val initialState = makeNewState 1
      val _ =
        SST.insertAddWeights initialState (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, state) = loopGuessCapacity 0 0 initialState

      val _ =
        let
          val currentElems = SST.unsafeViewContents state
          val nonZeroSize =
            SeqBasis.reduce 1000 op+ 0 (0, DS.length currentElems) (fn i =>
              case DS.nth currentElems i of
                NONE => 0
              | SOME (bidx, weight) => if Complex.isNonZero weight then 1 else 0)
          val density = Real.fromInt nonZeroSize / Real.fromInt maxNumStates
        in
          print
            ("gate " ^ Int.toString depth ^ ": non-zeros: "
             ^ Int.toString nonZeroSize ^ "; density: "
             ^ Real.fmt (StringCvt.FIX (SOME 8)) density ^ "\n")
        end

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      case SST.lookup state desired of
        NONE => Complex.zero
      | SOME v => v
    end
end
