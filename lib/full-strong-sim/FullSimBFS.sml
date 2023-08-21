functor FullSimBFS
  (structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure G: GATE
   structure GateScheduler: GATE_SCHEDULER
   sharing C = SST.C = G.C

   val blockSize: int
   val maxload: real
   val doMeasureZeros: bool
   val denseThreshold: real
   val pullThreshold: real):
sig
  val run: Circuit.t -> (BasisIdx.t * C.t) option DelayedSeq.t
end =
struct

  structure DS = DenseState(C)

  structure Expander =
    ExpandState
      (structure C = C
       structure SST = SST
       structure DS = DS
       structure G = G
       val denseThreshold = denseThreshold
       val blockSize = blockSize
       val maxload = maxload
       val pullThreshold = pullThreshold)


  val bits = Seq.fromList [ (*"▏",*)"▎", "▍", "▌", "▊"]

  fun fillBar width x =
    let
      val middleSize = width - 2
      val {frac, whole} = Real.split (x * Real.fromInt middleSize)
      val filledCount = Real.round whole
      val lastBit = Seq.nth bits (Real.floor
        (Real.fromInt (Seq.length bits) * frac))
    in
      String.concat (List.tabulate (middleSize, fn i =>
        if i < filledCount then "█"
        else if i = filledCount then lastBit
        else " "))
    end


  fun leftPad width x =
    CharVector.tabulate (Int.max (0, width - String.size x), fn _ => #" ") ^ x

  fun rightPad width x =
    x ^ CharVector.tabulate (Int.max (0, width - String.size x), fn _ => #" ")


  fun run {numQubits, gates} =
    let
      val gates = Seq.map G.fromGateDefn gates
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val sched = GateScheduler.new
        { numQubits = numQubits
        , numGates = depth
        , gateTouches = #touches o gate
        , gateIsBranching = (fn i =>
            case #action (gate i) of
              G.NonBranching _ => false
            | _ => true)
        }

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()
      val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits))
      val maxCountSize = String.size (Int.toString maxNumStates)
      val maxGateNameSize = String.size (Int.toString depth)

      fun padCount x =
        leftPad maxCountSize (Int.toString x)
      fun padGate x =
        leftPad maxGateNameSize (Int.toString x)

      fun dumpDensity (i, nonZeroSize, zeroSize, capacity) =
        let
          val density = Real.fromInt nonZeroSize / Real.fromInt maxNumStates
          val densityStr = Real.fmt (StringCvt.FIX (SOME 8)) density
          val zStr =
            case zeroSize of
              NONE => ""
            | SOME x => " zero " ^ padCount x
          val slackStr =
            case (zeroSize, capacity) of
              (SOME zs, SOME cap) =>
                " slack "
                ^
                leftPad 3 (Int.toString (Real.ceil
                  (100.0
                   * (1.0 - Real.fromInt (nonZeroSize + zs) / Real.fromInt cap))))
                ^ "%"
            | _ => ""
        in
          print
            (fillBar 12 density ^ " " ^ "gate " ^ padGate i ^ " density "
             ^ densityStr ^ " nonzero " ^ padCount nonZeroSize ^ zStr ^ slackStr);
          TextIO.flushOut TextIO.stdOut
        end


      fun loop numGateApps gatesVisitedSoFar prevNonZeroSize state =
        if gatesVisitedSoFar >= depth then
          let
            val (nonZeros, numNonZeros) =
              case state of
                Expander.Sparse sst =>
                  (SST.unsafeViewContents sst, SST.nonZeroSize sst)
              | Expander.Dense ds =>
                  (DS.unsafeViewContents ds, DS.nonZeroSize ds)
              | Expander.DenseKnownNonZeroSize (ds, nz) =>
                  (DS.unsafeViewContents ds, nz)
          in
            dumpDensity (gatesVisitedSoFar, numNonZeros, NONE, NONE);
            print "\n";
            (numGateApps, nonZeros)
          end

        else
          let
            (* val (goal, numBranchingUntilGoal) = findNextGoal gates next *)
            (* val goal = next + 1 *)
            (* val theseGates = Seq.subseq gates (next, goal - next) *)

            val theseGates = GateScheduler.pickNext sched
            (* val _ = print
              ("visiting: " ^ Seq.toString Int.toString theseGates ^ "\n") *)
            val theseGates = Seq.map (Seq.nth gates) theseGates
            val numGatesVisitedHere = Seq.length theseGates
            val ({result, method, numNonZeros, numGateApps = apps}, tm) =
              Util.getTime (fn () =>
                Expander.expand
                  { gates = theseGates
                  , numQubits = numQubits
                  , state = state
                  , prevNonZeroSize = prevNonZeroSize
                  })

            val seconds = Time.toReal tm
            val millions = Real.fromInt apps / 1e6
            val throughput = millions / seconds
            val throughputStr = Real.fmt (StringCvt.FIX (SOME 2)) throughput
            val _ = dumpDensity (gatesVisitedSoFar, numNonZeros, NONE, NONE)
            val _ = print
              (" hop " ^ leftPad 3 (Int.toString numGatesVisitedHere) ^ " "
               ^ rightPad 11 method ^ " "
               ^ Real.fmt (StringCvt.FIX (SOME 4)) seconds ^ "s throughput "
               ^ throughputStr ^ "\n")
          in
            loop (numGateApps + apps) (gatesVisitedSoFar + numGatesVisitedHere)
              numNonZeros result
          end


      val initialState = Expander.Sparse
        (SST.singleton {numQubits = numQubits}
           (BasisIdx.zeros, C.defaultReal 1.0))

      val (numGateApps, finalState) = loop 0 0 1 initialState
      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
    in
      finalState
    end
end
