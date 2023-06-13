functor FullSimBFS
  (structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure G: GATE
   sharing C = SST.C = G.C

   val blockSize: int
   val maxload: real
   val maxBranchingStride: int
   val doMeasureZeros: bool
   val denseThreshold: real):
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
       val maxload = maxload)


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


  fun findNextGoal gates gatenum =
    if maxBranchingStride = ~1 then
      (gatenum + 1, if G.expectBranching (Seq.nth gates gatenum) then 1 else 0)
    else
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
      val gates = Seq.map G.fromGateDefn gates
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

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


      fun loop numGateApps next prevNonZeroSize state =
        if next >= depth then
          let
            val (nonZeros, numNonZeros) =
              case state of
                Expander.Sparse sst =>
                  (SST.unsafeViewContents sst, SST.nonZeroSize sst)
              | Expander.Dense ds =>
                  (DS.unsafeViewContents ds, DS.nonZeroSize ds)
          in
            dumpDensity (next, numNonZeros, NONE, NONE);
            print "\n";
            (numGateApps, nonZeros)
          end

        else
          let
            val (goal, numBranchingUntilGoal) = findNextGoal gates next

            val theseGates = Seq.subseq gates (next, goal - next)
            val ({result, numNonZeros, numGateApps = apps}, tm) =
              Util.getTime (fn () =>
                Expander.expand
                  { gates = theseGates
                  , numQubits = numQubits
                  , state = state
                  , prevNonZeroSize = prevNonZeroSize
                  })

            val expansionType =
              case result of
                Expander.Sparse _ => "sparse"
              | Expander.Dense _ => "dense "

            val seconds = Time.toReal tm
            val millions = Real.fromInt apps / 1e6
            val throughput = millions / seconds
            val throughputStr = Real.fmt (StringCvt.FIX (SOME 2)) throughput
            val _ = dumpDensity (next, numNonZeros, NONE, NONE)
            val _ = print
              (" hop " ^ leftPad 3 (Int.toString (goal - next)) ^ " "
               ^ expansionType ^ " " ^ Real.fmt (StringCvt.FIX (SOME 4)) seconds
               ^ "s throughput " ^ throughputStr ^ "\n")
          in
            loop (numGateApps + apps) goal numNonZeros result
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
