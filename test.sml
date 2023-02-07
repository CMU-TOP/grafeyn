structure CLA = CommandLineArgs

fun runTest (testNum, (numQubits, gates)) =
  let
    val circuit = Seq.fromList gates
  in
    print
      ("=======================================================\n" ^ "TEST "
       ^ Int.toString testNum ^ "\n"
       ^ "=======================================================\n");
    let val result = Circuit.simulate {numQubits = numQubits} circuit
    in print (SparseState.toString {numQubits = numQubits} result ^ "\n")
    end
  end

val tests =
  let
    open Gate
  in
    [ (4, [Hadamard 0, Hadamard 1, Hadamard 2, Hadamard 3])

    , ( 2
      , [ Hadamard 0
        , CX {control = 0, target = 1}
        , PauliZ 1
        , Hadamard 0
        , Hadamard 1
        , CX {control = 1, target = 0}
        , Hadamard 1
        ]
      )

    , (1, [Hadamard 0, T 0, T 0, T 0, T 0])
    ]
  end


val _ =
  Seq.iterate (fn (i, xx) => (runTest (i, xx); i + 1)) 0 (Seq.fromList tests)
