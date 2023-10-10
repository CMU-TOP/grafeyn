functor FullSimBFS
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure G: GATE
   sharing B = SST.B = G.B
   sharing C = SST.C = G.C

   val blockSize: int
   val maxload: real
   val gateScheduler: GateScheduler.t
   val doMeasureZeros: bool
   val denseThreshold: real
   val pullThreshold: real):
sig
  val run: Circuit.t
           -> {result: (B.t * C.t) option DelayedSeq.t, counts: int Seq.t}
end =
struct

  structure DS = DenseState (structure C = C structure B = B)

  structure Expander =
    ExpandState
      (structure B = B
       structure C = C
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


  fun dumpState numQubits s =
    let
      val ss =
        case s of
          Expander.Sparse sst => SST.unsafeViewContents sst
        | Expander.Dense ds => DS.unsafeViewContents ds
        | Expander.DenseKnownNonZeroSize (ds, _) => DS.unsafeViewContents ds
    in
      Util.for (0, DelayedSeq.length ss) (fn i =>
        case DelayedSeq.nth ss i of
          NONE => ()
        | SOME (bidx, weight) =>
            let in
              print
                (B.toString {numQubits = numQubits} bidx ^ " "
                 ^ C.toString weight ^ "\n")
            end)
    end


  fun run {numQubits, gates} =
    let
      val gates = Seq.map G.fromGateDefn gates
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val gateSchedulerPickNextGates = gateScheduler
        { numQubits = numQubits
        , numGates = depth
        , gateTouches = #touches o gate
        , gateIsBranching = (fn i =>
            case #action (gate i) of
              G.NonBranching _ => false
            | _ => true)
        }

      (* val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else () *)
      val maxNumStates = IntInf.pow (2, numQubits)
      (* val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits)) *)
      val maxCountSize = Int.min
        (10, String.size (IntInf.toString maxNumStates))
      val maxGateNameSize = String.size (Int.toString depth)

      fun padCount x =
        leftPad maxCountSize (Int.toString x)
      fun padGate x =
        leftPad maxGateNameSize (Int.toString x)

      fun dumpDensity (i, nonZeroSize, zeroSize, capacity) =
        let
          val density = Rat.make (IntInf.fromInt nonZeroSize, maxNumStates)
          val approxDensity = Rat.approx density
          val densityStr = Real.fmt (StringCvt.FIX (SOME 8)) approxDensity
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
            (fillBar 12 approxDensity ^ " " ^ "gate " ^ padGate i ^ " density "
             ^ densityStr ^ " nonzero " ^ padCount nonZeroSize ^ zStr ^ slackStr);
          TextIO.flushOut TextIO.stdOut;
          density
        end


      fun loop numGateApps gatesVisitedSoFar counts prevNonZeroSize state =
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

            (* val _ = dumpState numQubits state *)

            val density =
              dumpDensity (gatesVisitedSoFar, numNonZeros, NONE, NONE)
          in
            print "\n";
            (numGateApps, nonZeros, Seq.fromRevList (numNonZeros :: counts))
          end

        else
          let
            (* val _ = dumpState numQubits state *)

            val theseGates = gateSchedulerPickNextGates ()
            val _ =
              if Seq.length theseGates > 0 then
                ()
              else
                raise Fail "FullSimBFS: gate scheduler returned empty sequence"

            (* val _ = print
              ("visiting: " ^ Seq.toString Int.toString theseGates ^ "\n") *)

            val theseGates = Seq.map (Seq.nth gates) theseGates
            val numGatesVisitedHere = Seq.length theseGates
            val ({result, method, numNonZeros, numGateApps = apps}, tm) =
              Util.getTime (fn () =>
                Expander.expand
                  { gates = theseGates
                  , numQubits = numQubits
                  , maxNumStates = maxNumStates
                  , state = state
                  , prevNonZeroSize = prevNonZeroSize
                  })

            val seconds = Time.toReal tm
            val millions = Real.fromInt apps / 1e6
            val throughput = millions / seconds
            val throughputStr = Real.fmt (StringCvt.FIX (SOME 2)) throughput
            val density =
              dumpDensity (gatesVisitedSoFar, numNonZeros, NONE, NONE)
            val _ = print
              (" hop " ^ leftPad 3 (Int.toString numGatesVisitedHere) ^ " "
               ^ rightPad 11 method ^ " "
               ^ Real.fmt (StringCvt.FIX (SOME 4)) seconds ^ "s throughput "
               ^ throughputStr ^ "\n")
          in
            loop (numGateApps + apps) (gatesVisitedSoFar + numGatesVisitedHere)
              (numNonZeros :: counts) numNonZeros result
          end


      val initialState = Expander.Sparse
        (SST.singleton {numQubits = numQubits} (B.zeros, C.defaultReal 1.0))

      val (numGateApps, finalState, counts) = loop 0 0 [] 1 initialState
      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
    in
      {result = finalState, counts = counts}
    end
end
