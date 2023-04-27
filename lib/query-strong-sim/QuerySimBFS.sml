structure QuerySimBFS:
sig
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  structure HT = HashTable

  fun query {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()
      val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits))

      fun makeNewState cap =
        HT.make
          { hash = BasisIdx.hash
          , eq = BasisIdx.equal
          , capacity = cap
          , maxload = 0.75
          }

      fun loopTryCapacity capacity i countGateApp state =
        let
          val currentElems = HT.unsafeViewContents state
          val newState = makeNewState capacity

          fun doGate widx =
            case Gate.apply (gate i) widx of
              Gate.OutputOne widx' => HT.insertWith Complex.+ newState widx'
            | Gate.OutputTwo (widx1, widx2) =>
                ( HT.insertWith Complex.+ newState widx1
                ; HT.insertWith Complex.+ newState widx2
                )

          val numGateApps =
            SeqBasis.reduce 100 op+ 0 (0, Seq.length currentElems) (fn i =>
              case Seq.nth currentElems i of
                NONE => 0
              | SOME (bidx, weight) =>
                  if Complex.isNonZero weight then (doGate (bidx, weight); 1)
                  else 0)
        in
          loopGuessCapacity (i + 1) (countGateApp + numGateApps) newState
        end
        handle HT.Full => loopTryCapacity (2 * capacity) i countGateApp state


      and loopGuessCapacity i countGateApp state =
        if i >= depth then
          (countGateApp, state)
        else
          let
            val currentElems = HT.unsafeViewContents state
            val nonZeroSize =
              SeqBasis.reduce 1000 op+ 0 (0, Seq.length currentElems) (fn i =>
                case Seq.nth currentElems i of
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
        HT.insertIfNotPresent initialState (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, state) = loopGuessCapacity 0 0 initialState

      val _ =
        let
          val currentElems = HT.unsafeViewContents state
          val nonZeroSize =
            SeqBasis.reduce 1000 op+ 0 (0, Seq.length currentElems) (fn i =>
              case Seq.nth currentElems i of
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
      case HT.lookup state desired of
        NONE => Complex.zero
      | SOME v => v
    end
end
