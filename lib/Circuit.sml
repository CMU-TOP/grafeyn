structure Circuit =
struct

  type qubit_idx = int

  datatype gate = PauliY of qubit_idx | Hadamard of qubit_idx

  type circuit_layer = gate Seq.t
  type circuit = circuit_layer Seq.t
  type t = circuit

  fun simulate {numQubits: int} circuit =
    let
      fun applyGate state gate =
        case gate of
          PauliY qi => SparseState.pauliy state qi
        | Hadamard qi => SparseState.hadamard state qi

      fun dump state =
        print
          ("==================================\n"
           ^ SparseState.toString {numQubits = numQubits} state)

      fun doLayer (state, layer) =
        let
          val result = Seq.reduce SparseState.merge SparseState.empty
            (Seq.map (applyGate state) layer)
        in
          dump result;
          result
        end
    in
      dump SparseState.initial;
      Seq.iterate doLayer SparseState.initial circuit
    end

end
