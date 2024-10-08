structure CLA = CommandLineArgs

val simName = CLA.parseString "sim" "full-bfs"
val inputName = CLA.parseString "input" "random"
val output = CLA.parseString "output" ""

val _ = print ("input " ^ inputName ^ "\n")

val _ = print
  ("-------------------------------\n\
   \--- input-specific specs\n\
   \-------------------------------\n")

val circuit =
  case inputName of
    "random" =>
      let
        val numGates = CLA.parseInt "gates" 60
        val numQubits = CLA.parseInt "qubits" 20

        fun genGate seed =
          let
            val p = Util.hash seed mod 100
            val qi1 = Util.hash (seed + 1) mod numQubits
            val qi2 = Util.hash (seed + 2) mod numQubits
          in
            if p < 50 then Gate.Hadamard qi1
            else if p < 63 then Gate.PauliY qi1
            else if p < 76 then Gate.PauliZ qi1
            else if p < 89 then Gate.T qi1
            else Gate.CX {control = qi1, target = qi2}
          end

        val gates = Seq.tabulate (fn i => genGate (3 * i)) numGates
      in
        {numQubits = numQubits, gates = gates}
      end

  | "google-circuit" =>
      let
        val numRows = CLA.parseInt "google-circuit-num-rows" 9
        val numCols = CLA.parseInt "google-circuit-num-cols" 6
        val numCycles = CLA.parseInt "google-circuit-num-cycles" 14
        val patterns = CLA.parseString "google-circuit-patterns" "EFGH"
        val seed = CLA.parseInt "google-circuit-seed" 15210

        val _ = print ("google-circuit-num-rows " ^ Int.toString numRows ^ "\n")
        val _ = print ("google-circuit-num-cols " ^ Int.toString numCols ^ "\n")
        val _ = print
          ("google-circuit-num-cycles " ^ Int.toString numCycles ^ "\n")
        val _ = print ("google-circuit-patterns " ^ patterns ^ "\n")
        val _ = print ("google-circuit-seed " ^ Int.toString seed ^ "\n")

        val patterns =
          Seq.tabulate
            (fn i =>
               GenerateGoogleCircuit.twoQubitPatternFromChar
                 (String.sub (patterns, i))) (String.size patterns)
      in
        GenerateGoogleCircuit.generate
          { numQubitRows = numRows
          , numQubitCols = numCols
          , numCycles = numCycles
          , patterns = patterns
          , seed = seed
          }
      end

  | _ =>
      (ParseQASM.loadFromFile inputName
       handle ParseQASM.ParseError msg => Util.die (inputName ^ ": " ^ msg))

val _ = print ("-------------------------------\n")

val _ = print ("gates  " ^ Int.toString (Circuit.numGates circuit) ^ "\n")
val _ = print ("qubits " ^ Int.toString (Circuit.numQubits circuit) ^ "\n")

val showCircuit = CLA.parseFlag "show-circuit"
val _ = print ("show-circuit? " ^ (if showCircuit then "yes" else "no") ^ "\n")
val _ =
  if not showCircuit then
    ()
  else
    print
      ("=========================================================\n"
       ^ Circuit.toString circuit
       ^ "=========================================================\n")

fun wrapFullSim runner () =
  let
    val result = runner circuit
  in
    print
      ("num non-zero states " ^ Int.toString (SparseState.size result) ^ "\n")
  end

fun wrapFullSim' runner () =
  let
    val result = runner circuit
  in
    print
      ("num non-zero states " ^ Int.toString (DelayedSeq.length result) ^ "\n")
  end

fun wrapQuerySim runner () =
  let val result = runner circuit BasisIdx.zeros
  in print ("result " ^ Complex.toString result ^ "\n")
  end

fun parseMaxWaveSpace () =
  let
    val spaceConstraint =
      CommandLineArgs.parseInt "max-wave-space" (1000 * 1000)
  in
    print ("max-wave-space " ^ Int.toString spaceConstraint ^ "\n");
    spaceConstraint
  end

val _ = print ("sim " ^ simName ^ "\n")
val simulator =
  case simName of
    "full-seq" => wrapFullSim FullSimSequential.run
  | "full-naive-par" => wrapFullSim FullSimNaivePar.run
  | "full-bfs" => wrapFullSim' FullSimBFS.run
  | "full-strided-bfs" => wrapFullSim FullSimStridedBFS.run

  | "query-seq" => wrapQuerySim QuerySimSequential.query
  | "query-naive-par" => wrapQuerySim QuerySimNaivePar.query
  | "query-bfs" => wrapQuerySim QuerySimBFS.query
  | "query-wave-bfs" =>
      wrapQuerySim (QuerySimWaveBFS.query (parseMaxWaveSpace ()))
  | "query-single-goal-wave-bfs" =>
      wrapQuerySim (QuerySimSingleGoalWaveBFS.query (parseMaxWaveSpace ()))
  | "query-two-wave-bfs" =>
      wrapQuerySim (QuerySimTwoWaveBFS.query (parseMaxWaveSpace ()))

  | _ =>
      Util.die
        ("Unknown -sim " ^ simName ^ "; valid options are: "
         ^ "full-seq, full-naive-par, full-bfs, full-strided-bfs, "
         ^ "query-seq, query-naive-par, query-bfs, query-wave-bfs, "
         ^ "query-single-goal-wave-bfs, query-two-wave-bfs")

val msg = "quantum simulator (" ^ simName ^ ")"
val () = Benchmark.run msg (fn _ =>
  simulator ())
    (*
    val _ =
      if output = "" then
        ()
      else
        let
          val _ = print ("generating output...\n")
    
          val contents =
            SparseState.toString {numQubits = Circuit.numQubits circuit} result
            ^ "\n"
    
          val _ = print ("writing to " ^ output ^ "\n")
          val outstream = TextIO.openOut output
        in
          TextIO.output (outstream, contents);
          TextIO.closeOut outstream;
          print ("output written to " ^ output ^ "\n")
        end
    *)
