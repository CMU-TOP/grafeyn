structure CLA = CommandLineArgs

val test1 =
  let
    open Circuit
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [[Hadamard 0], [Hadamard 1], [Hadamard 2], [Hadamard 3]])
  in
    print
      ("=================================================================\n"
       ^ "TEST 1\n"
       ^ "=================================================================\n");
    simulate {numQubits = 4} circuit;
    print "\n";
    ()
  end


val test2 =
  let
    open Circuit
    val circuit = Seq.map Seq.fromList (Seq.fromList
      [ [Hadamard 0]
      , [CX {control = 0, target = 1}]
      , [PauliZ 1]
      , [Hadamard 0]
      , [Hadamard 1]
      , [CX {control = 1, target = 0}]
      , [Hadamard 1]
      ])
  in
    print
      ("=================================================================\n"
       ^ "TEST 2\n"
       ^ "=================================================================\n");
    simulate {numQubits = 2} circuit;
    print "\n";
    ()
  end
