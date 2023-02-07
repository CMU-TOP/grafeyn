structure CLA = CommandLineArgs

val numGates = CLA.parseInt "gates" 10
val numQubits = CLA.parseInt "qubits" 10
val output = CLA.parseString "output" ""

fun genGate seed =
  let
    val p = Util.hash seed mod 100
    val qi1 = Util.hash (seed + 1) mod numQubits
    val qi2 = Util.hash (seed + 2) mod numQubits
  in
    if p < 20 then Gate.PauliY qi1
    else if p < 40 then Gate.PauliZ qi1
    else if p < 60 then Gate.Hadamard qi1
    else if p < 80 then Gate.T qi1
    else Gate.CX {control = qi1, target = qi2}
  end

val circuit = Seq.tabulate (fn i => genGate (3 * i)) numGates

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
