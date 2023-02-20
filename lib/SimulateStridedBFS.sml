structure SimulateStridedBFS:
sig
  val run: Circuit.t -> SparseState.t
end =
struct

  structure HT = HashTable

  val strideLength = CommandLineArgs.parseInt "stride-length" 5
  val strideSlack = CommandLineArgs.parseReal "stride-slack" 4.0

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

      fun loop i countGateApp state =
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

            (* val _ = print
              ("gate " ^ Int.toString i ^ ": " ^ Int.toString nonZeroSize
               ^ " elems\n") *)

            val nextStop = Int.min (i + strideLength, depth)

            val newStateCapacity = Int.max (100, Real.ceil
              (strideSlack * Real.fromInt nonZeroSize))
            val newState = makeNewState newStateCapacity

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
                    if Complex.isNonZero (#2 widx) then strideLoop 0 i widx
                    else 0)
          in
            loop nextStop (countGateApp + numGateApps) newState
          end

      val initialState = makeNewState 1
      val _ =
        HT.insertIfNotPresent initialState (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, state) = loop 0 0 initialState

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      SparseState.compactFromTable state
    end
end
