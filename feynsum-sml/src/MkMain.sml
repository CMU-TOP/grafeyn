functor MkMain
  (structure C: COMPLEX
   structure B: BASIS_IDX
   val numQubits: int
   val disableFusion: bool
   val maxBranchingStride: int
   val schedSamplePathDepth: int
   val schedSamplePaths: int
   val schedSampleMinStates: int
   val schedSampleStates: real
   val blockSize: int
   val maxload: real
   val gateScheduler: string
   val doMeasureZeros: bool
   val denseThreshold: real
   val pullThreshold: real) =
struct

  structure CLA = CommandLineArgs

  structure G = Gate (structure B = B structure C = C)
  structure SST = SparseStateTable (structure B = B
                                    val numQubits = numQubits
                                    structure C = C)

  structure F = Fingerprint (structure B = B structure C = C)

  structure PP = PushPull (structure B = B
                         structure C = C
                         val numQubits = numQubits
                         val maxBranchingStride = maxBranchingStride
                         val schedSamplePathDepth = schedSamplePathDepth
                         val schedSamplePaths = schedSamplePaths
                         val schedSampleMinStates = schedSampleMinStates
                         val schedSampleStates = schedSampleStates
                         val blockSize = blockSize
                         val maxload = maxload
                         val denseThreshold = denseThreshold
                         val pullThreshold = pullThreshold)

structure K = Kernelize (structure B = B
                         structure C = C)

  (* =======================================================================
   * main
   *)


  (*fun main' ((inputName, circuit): string * DataFlowGraph.t) =
    let val numQubits = #numQubits circuit
        val gates = Seq.map (G.fromGateDefn {numQubits = numQubits}) (#gates circuit)
        val initState = SST.singleton (B.zeros, C.one)
        val { state, numVerts, numEdges } = PP.applyAll (gates, initState) circuit
        val (fp, tm) = Util.getTime (fn _ => F.fingerprint (SST.unsafeViewContents state))
        val _ = print ("computed fingerprint in " ^ Time.fmt 4 tm ^ "s\n")
        val _ = Util.for (0, Seq.length fp)
                         (fn i =>
                             let val (b, c) = Seq.nth fp i in
                               print
                                 ("fp" ^ Int.toString i ^ " "
                                  ^ B.toString {numQubits = numQubits, pretty = false} b ^ " "
                                  ^ C.toString c ^ "\n")
                             end)
    in
      print ("Completed with vertices = " ^ Int.toString numVerts ^ " and edges = " ^ Int.toString numEdges ^ "\n")
    end*)


  (*structure FQ = FinishQubitScheduler (val maxBranchingStride = maxBranchingStride
                                       val disableFusion = disableFusion)
  val sched = FQ.scheduler5*)
  (*
  Make a new file for this (PP struct shouldnt contain fxn for fusion)
  *)
  fun main ((inputName, circuit): string * DataFlowGraph.t) =
    let val _ = print ("max branching stride = " ^ Int.toString maxBranchingStride ^ "\n")
        (*val sched' = DataFlowGraphUtil.scheduleWithOracle circuit (fn i => #maxBranchingFactor (Seq.nth gates i) > 1) (sched circuit) disableFusion maxBranchingStride
        val kernels = Seq.map (G.fuse o Seq.map (Seq.nth gates)) sched'*)
        (* val kernels = PP.dumbFusion gates *)
        val kernels = K.performGateFusion circuit
        val initState = SST.singleton (B.zeros, C.one)
        val { state, numEdges } = PP.applyAllOld (kernels, initState)
        val (fp, tm) = Util.getTime (fn _ => F.fingerprint (PP.unsafeViewContents state))
        val _ = print ("computed fingerprint in " ^ Time.fmt 4 tm ^ "s\n")
        val _ = Util.for (0, Seq.length fp)
                         (fn i =>
                             let val (b, c) = Seq.nth fp i in
                               print
                                 ("fp" ^ Int.toString i ^ " "
                                  ^ B.toString {numQubits = numQubits, pretty = false} b ^ " "
                                  ^ C.toString c ^ "\n")
                             end)
    in
      print ("Completed with edges = " ^ Int.toString numEdges ^ "\n")
    end
   

  (*fun main (inputName, circuit) =
    let
      val numQubits = #numQubits circuit
      val impl = CLA.parseString "impl" "lockfree"
      val output = CLA.parseString "output" ""
      val outputDensities = CLA.parseString "output-densities" ""

      val _ = print ("impl " ^ impl ^ "\n")

      fun sim () =
        case impl of
          "lockfree" => BFSLockfree.run circuit
        | "locked" => BFSLocked.run circuit
        | _ =>
            Util.die
              ("unknown impl " ^ impl
               ^ "; valid options are: locked, lockfree\n")

      val {result, counts} = Benchmark.run "full-sim-bfs" (fn _ => sim ())
      val counts = Seq.map IntInf.fromInt counts

      val maxNumStates = IntInf.pow (2, numQubits)
      val numRounds = IntInf.fromInt (Seq.length counts)

      val avgDensity = Rat.normalize (Rat.make
        (Seq.reduce IntInf.+ 0 counts, IntInf.* (maxNumStates, numRounds)))
      val maxDensity = Rat.normalize (Rat.make
        (Seq.reduce IntInf.max 0 counts, maxNumStates))

      val _ = print
        ("avg-density "
         ^ Real.fmt (StringCvt.FIX (SOME 12)) (Rat.approx avgDensity) ^ "\n")
      val _ = print
        ("max-density "
         ^ Real.fmt (StringCvt.FIX (SOME 12)) (Rat.approx maxDensity) ^ "\n")

      val (fp, tm) = Util.getTime (fn _ => F.fingerprint result)
      val _ = print ("computed fingerprint in " ^ Time.fmt 4 tm ^ "s\n")

      val _ = Util.for (0, Seq.length fp) (fn i =>
        let
          val (b, c) = Seq.nth fp i
        in
          print
            ("fp" ^ Int.toString i ^ " "
             ^ B.toString {numQubits = numQubits, pretty = false} b ^ " "
             ^ C.toString c ^ "\n")
        end)

      val _ =
        if output = "" then
          print ("use -output FILE to see output state vector\n")
        else
          let
            val _ = print ("writing to " ^ output ^ "\n")
            val outstream = TextIO.openOut output
          in
            Util.for (0, DelayedSeq.length result) (fn i =>
              case DelayedSeq.nth result i of
                NONE => ()
              | SOME (bidx, weight) =>
                  if C.isZero weight then
                    ()
                  else
                    TextIO.output
                      ( outstream
                      , B.toString {numQubits = numQubits, pretty = false} bidx
                        ^ " " ^ C.toString weight ^ "\n"
                      ));
            TextIO.closeOut outstream;
            print ("output written to " ^ output ^ "\n")
          end

      val _ =
        if outputDensities = "" then
          print ("use -output-densities FILE to see densities\n")
        else
          let
            val outstream = TextIO.openOut outputDensities
          in
            Util.for (0, Seq.length counts) (fn i =>
              let
                val count = Seq.nth counts i
                val density = Rat.normalize (Rat.make (count, maxNumStates))
                val dstr =
                  Real.fmt (StringCvt.FIX (SOME 12)) (Rat.approx density)
              in
                TextIO.output (outstream, dstr ^ "\n")
              end);
            TextIO.closeOut outstream;
            print ("output densities written to " ^ outputDensities ^ "\n")
          end

      val name = (OS.Path.base (OS.Path.file inputName)) handle _ => inputName
    in
      print
        (String.concatWith ","
           [ name
           , Int.toString (#numQubits circuit)
           , Int.toString (Seq.length (#gates circuit))
           , Real.fmt (StringCvt.FIX (SOME 12)) (Rat.approx maxDensity)
           , Real.fmt (StringCvt.FIX (SOME 12)) (Rat.approx avgDensity)
           ] ^ "\n")
    end*)

end
