structure SimulateStridedBFS:
sig
  val run: Circuit.t -> SparseState.t
end =
struct

  structure HT = HashTable

  val strideLength = CommandLineArgs.parseInt "stride-length" 5
  val _ = print ("stride-length " ^ Int.toString strideLength ^ "\n")

  fun run {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

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

          val nextStop = Int.min (i + strideLength, depth)

          fun strideLoop gateAppCount j widx =
            if j >= nextStop then
              (HT.insertWith Complex.+ newState widx; gateAppCount)
            else
              case Gate.apply (gate j) widx of
                Gate.OutputOne widx' =>
                  strideLoop (gateAppCount + 1) (j + 1) widx'
              | Gate.OutputTwo (widx1, widx2) =>
                  let val xx = strideLoop (gateAppCount + 1) (j + 1) widx1
                  in strideLoop xx (j + 1) widx2
                  end

          val grain = Int.max (1, 50 div strideLength)
          val numGateApps =
            SeqBasis.reduce grain op+ 0 (0, Seq.length currentElems) (fn k =>
              case Seq.nth currentElems k of
                NONE => 0
              | SOME widx =>
                  if Complex.isNonZero (#2 widx) then strideLoop 0 i widx else 0)
        in
          loopGuessCapacity nextStop (countGateApp + numGateApps) newState
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

            val multiplier = if Gate.expectBranching (gate i) then 3.0 else 1.5
            val guess = Real.ceil (multiplier * Real.fromInt nonZeroSize)
          in
            loopTryCapacity guess i countGateApp state
          end

      val initialState = makeNewState 1
      val _ =
        HT.insertIfNotPresent initialState (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, state) = loopGuessCapacity 0 0 initialState

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      SparseState.compactFromTable state
    end
end
