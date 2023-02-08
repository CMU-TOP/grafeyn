structure HardcodedInputs =
struct
  val qitkit_20qbt_45cyc: (int * Circuit.t) =
    let
      open Gate
      val numQubits = 20
      val gates =
        [ CX {control = 12, target = 9}
        , CX {control = 6, target = 12}
        , CX {control = 8, target = 6}
        , CX {control = 6, target = 10}
        , CX {control = 7, target = 6}
        , CX {control = 6, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 11, target = 6}
        , CX {control = 11, target = 6}
        , CX {control = 11, target = 6}
        , CX {control = 8, target = 11}
        , CX {control = 8, target = 7}
        , CX {control = 7, target = 6}
        , CX {control = 15, target = 7}
        , CX {control = 7, target = 13}
        , CX {control = 13, target = 9}
        , CX {control = 12, target = 9}
        , CX {control = 12, target = 10}
        , CX {control = 12, target = 10}
        , CX {control = 12, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 9, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 6, target = 9}
        , CX {control = 6, target = 9}
        , CX {control = 6, target = 12}
        , CX {control = 6, target = 12}
        , CX {control = 12, target = 9}
        , CX {control = 12, target = 9}
        , CX {control = 9, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 11, target = 6}
        , CX {control = 11, target = 6}
        , CX {control = 6, target = 9}
        , CX {control = 6, target = 10}
        , CX {control = 9, target = 10}
        , CX {control = 9, target = 10}
        , CX {control = 12, target = 10}
        , CX {control = 12, target = 10}
        , CX {control = 12, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 6, target = 10}
        , CX {control = 9, target = 10}
        ]

      val circuit = Seq.fromList gates
    in
      (numQubits, circuit)
    end

end
