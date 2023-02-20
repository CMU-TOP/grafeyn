structure SimulateBFS:
sig
  val run: Circuit.t -> SparseState.t
end =
struct

  structure HT = HashTable

  val branchingSlack = CommandLineArgs.parseReal "branching-slack" 4.0
  val nonBranchingSlack = CommandLineArgs.parseReal "non-branching-slack" 1.5

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

            val slack =
              if Gate.expectBranching (gate i) then branchingSlack
              else nonBranchingSlack
            val newStateCapacity = Real.ceil (slack * Real.fromInt nonZeroSize)
            val newState = makeNewState newStateCapacity

            fun doGate widx =
              case Gate.apply (gate i) widx of
                Gate.OutputOne widx' => HT.insertWith Complex.+ newState widx'
              | Gate.OutputTwo (widx1, widx2) =>
                  ( HT.insertWith Complex.+ newState widx1
                  ; HT.insertWith Complex.+ newState widx2
                  )


            val numGateApps =
              ForkJoin.parfor 100 (0, Seq.length currentElems) (fn i =>
                case Seq.nth currentElems i of
                  NONE => ()
                | SOME (bidx, weight) =>
                    if Complex.isNonZero weight then doGate (bidx, weight)
                    else ())
          in
            loop (i + 1) (countGateApp + nonZeroSize) newState
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
