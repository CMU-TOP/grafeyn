structure CLA = CommandLineArgs

val inputName = CLA.parseString "input" "random"
val output = CLA.parseString "output" ""

val _ = print ("input " ^ inputName ^ "\n")

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

  | "qitkit_20qbt_45cyc" => HardcodedInputs.qitkit_20qbt_45cyc

  | _ => Util.die ("unknown input " ^ inputName)


val _ = print ("gates  " ^ Int.toString (Circuit.numGates circuit) ^ "\n")
val _ = print ("qubits " ^ Int.toString (Circuit.numQubits circuit) ^ "\n")

val result = Benchmark.run "quantum simulator" (fn _ =>
  Circuit.simulate circuit)
val _ = print
  ("num non-zero states " ^ Int.toString (SparseState.size result) ^ "\n")

val _ =
  if output = "" then
    ()
  else
    let
      val outstream = TextIO.openOut output
      val contents =
        SparseState.toString {numQubits = Circuit.numQubits circuit} result
        ^ "\n"
    in
      TextIO.output (outstream, contents);
      TextIO.closeOut outstream;
      print ("output written to " ^ output ^ "\n")
    end
