structure Circuit =
struct

  datatype gate = PauliY | Hadamard

  type qubit_idx = int
  type circuit_layer = (qubit_idx * gate) Seq.t
  type circuit = circuit_layer Seq.t
  type t = circuit

  fun simulate {numQubits: int} circuit =
    let
      fun applyGate state (qi, gate) =
        case gate of
          PauliY => SparseState.pauliy state qi
        | Hadamard => SparseState.hadamard state qi

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
