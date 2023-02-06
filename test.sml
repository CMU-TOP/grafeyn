structure CLA = CommandLineArgs

val test1 =
  let
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [ [Gate.Hadamard 0]
      , [Gate.Hadamard 1]
      , [Gate.Hadamard 2]
      , [Gate.Hadamard 3]
      ])
  in
    print
      ("=================================================================\n"
       ^ "TEST 1\n"
       ^ "=================================================================\n");
    Circuit.simulate {numQubits = 4} circuit;
    print "\n";
    ()
  end


val test2 =
  let
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [[Gate.Hadamard 0, Gate.Hadamard 1, Gate.Hadamard 2, Gate.Hadamard 3]])
  in
    print
      ("=================================================================\n"
       ^ "TEST 2\n"
       ^ "=================================================================\n");
    Circuit.simulate {numQubits = 4} circuit;
    print "\n";
    ()
  end


val test2 =
  let
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [ [Gate.Hadamard 0]
      , [Gate.CX {control = 0, target = 1}]
      , [Gate.PauliZ 1]
      , [Gate.Hadamard 0]
      , [Gate.Hadamard 1]
      , [Gate.CX {control = 1, target = 0}]
      , [Gate.Hadamard 1]
      ])
  in
    print
      ("=================================================================\n"
       ^ "TEST 3\n"
       ^ "=================================================================\n");
    Circuit.simulate {numQubits = 2} circuit;
    print "\n";
    ()
  end


val test3 =
  let
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [[Gate.Hadamard 0], [Gate.T 0], [Gate.T 0], [Gate.T 0], [Gate.T 0]])
  in
    print
      ("=================================================================\n"
       ^ "TEST 4\n"
       ^ "=================================================================\n");
    Circuit.simulate {numQubits = 1} circuit;
    print "\n";
    ()
  end
