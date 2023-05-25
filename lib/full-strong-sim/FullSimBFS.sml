functor FullSimBFS
  (structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure G: GATE
   sharing C = SST.C = G.C

   val blockSize: int
   val maxload: real
   val maxBranchingStride: int
   val doMeasureZeros: bool):
sig
  val run: Circuit.t -> (BasisIdx.t * C.t) option DelayedSeq.t
end =
struct

  structure Expander =
    ExpandState
      (structure C = C
       structure SST = SST
       structure G = G
       val blockSize = blockSize
       val maxload = maxload)

  structure DS = DelayedSeq

  fun findNextGoal gates gatenum =
    let
      fun loop (i, branching) =
        if i >= Seq.length gates then
          (i, branching)
        else if G.expectBranching (Seq.nth gates i) then
          if branching >= maxBranchingStride then (i, branching)
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

      fun dumpDensity (i, nonZeroSize, zeroSize, capacity) =
        let
          val densityStr = Real.fmt (StringCvt.FIX (SOME 8))
            (Real.fromInt nonZeroSize / Real.fromInt maxNumStates)
          val zerosStr =
            case zeroSize of
              NONE => "??"
            | SOME x => Int.toString x
          val slackPct =
            case (zeroSize, capacity) of
              (SOME zs, SOME cap) =>
                Int.toString (Real.ceil
                  (100.0
                   * (1.0 - Real.fromInt (nonZeroSize + zs) / Real.fromInt cap)))
                ^ "%"
            | _ => "??"
        in
          print
            ("gate " ^ Int.toString i ^ ": non-zeros: "
             ^ Int.toString nonZeroSize ^ "; zeros: " ^ zerosStr ^ "; slack: "
             ^ slackPct ^ "; density: " ^ densityStr ^ "\n")
        end

      fun makeNewState cap = SST.make {capacity = cap, numQubits = numQubits}

      fun loop next prevNonZeroSize state =
        let
          val capacityHere = SST.capacity state

          val numZeros =
            if doMeasureZeros then SOME (SST.zeroSize state) else NONE

          val (nonZeros, nonZeroSize) =
            (* if dontCompact then
              let
                val elems = SST.unsafeViewContents state
                val nonZeroSize = SST.nonZeroSize state
              in
                (elems, nonZeroSize)
              end
            else *)
            let
              val nonZeros = SST.compact state
              val nonZeroSize = DelayedSeq.length nonZeros
            in
              (DelayedSeq.map SOME nonZeros, nonZeroSize)
            end

          val _ = dumpDensity (next, nonZeroSize, numZeros, SOME capacityHere)
        in
          if next >= depth then
            nonZeros
          else
            let
              val (goal, numBranchingUntilGoal) = findNextGoal gates next

              val rate = Real.max
                (1.0, Real.fromInt nonZeroSize / Real.fromInt prevNonZeroSize)
              val guess = Real.ceil (rate * Real.fromInt nonZeroSize)
              val guess = Int.min (guess, maxNumStates)

              val theseGates = Seq.subseq gates (next, goal - next)
              val (state, tm) = Util.getTime (fn () =>
                Expander.expand
                  { gates = theseGates
                  , numQubits = numQubits
                  , state = nonZeros
                  , expected = guess
                  })

              val seconds = Time.toReal tm
              val millionsOfElements = Real.fromInt nonZeroSize / 1e6
              val throughput = millionsOfElements / seconds
              val throughputStr = Real.fmt (StringCvt.FIX (SOME 3)) throughput
              val _ = print ("throughput " ^ throughputStr ^ "\n")
            in
              loop goal nonZeroSize state
            end
        end

      val initialState =
        SST.singleton {numQubits = numQubits}
          (BasisIdx.zeros, C.defaultReal 1.0)

      (* val (totalGateApps, finalState) = loopGuessCapacity 0 0 initialState 
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n") *)

      val finalState = loop 0 1 initialState
    in
      finalState
    end
end
