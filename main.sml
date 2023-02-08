structure CLA = CommandLineArgs

val inputName = CLA.parseString "input" "random"
val output = CLA.parseString "output" ""

val _ = print ("input " ^ inputName ^ "\n")

val (circuit, numGates, numQubits) =
  case inputName of
    "random" =>
      let
        val numGates = CLA.parseInt "gates" 60
        val numQubits = CLA.parseInt "qubits" 20
        val _ = print ("gates  " ^ Int.toString numGates ^ "\n")
        val _ = print ("qubits " ^ Int.toString numQubits ^ "\n")

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

        val circuit = Seq.tabulate (fn i => genGate (3 * i)) numGates
      in
        (circuit, numGates, numQubits)
      end

  | "qitkit_20qbt_45cyc" =>
      let val (numQubits, circ) = HardcodedInputs.qitkit_20qbt_45cyc
      in (circ, Seq.length circ, numQubits)
      end

  | _ => Util.die ("unknown input " ^ inputName)

fun bench () =
  Circuit.simulate {numQubits = numQubits} circuit

val result = Benchmark.run "quantum simulator" bench
val _ = print
  ("num non-zero states " ^ Int.toString (SparseState.size result) ^ "\n")

val _ =
  if output = "" then
    ()
  else
    let
      val outstream = TextIO.openOut output
      val contents = SparseState.toString {numQubits = numQubits} result ^ "\n"
    in
      TextIO.output (outstream, contents);
      TextIO.closeOut outstream;
      print ("output written to " ^ output ^ "\n")
    end
