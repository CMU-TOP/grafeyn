structure CLA = CommandLineArgs

val circuit: Circuit.t =
  let
    open Circuit
  in
    Seq.map Seq.fromList (Seq.fromList
      [[(0, Hadamard)], [(1, Hadamard)], [(2, Hadamard)], [(3, Hadamard)]])
  end

val result = Circuit.simulate {numQubits = 4} circuit
