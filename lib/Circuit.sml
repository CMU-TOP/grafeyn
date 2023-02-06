structure Circuit =
struct

  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}

  type circuit_layer = gate Seq.t
  type circuit = circuit_layer Seq.t
  type t = circuit

  fun simulate {numQubits: int} circuit =
    let
      fun applyGate state gate =
        case gate of
          PauliY qi => SparseState.pauliy state qi
        | PauliZ qi => SparseState.pauliz state qi
        | Hadamard qi => SparseState.hadamard state qi
        | CX qis => SparseState.cx state qis

      fun dump isFirst state =
        let val front = if isFirst then "" else "--------\n"
        in print (front ^ SparseState.toString {numQubits = numQubits} state)
        end

      fun doLayer (state, layer) =
        let
          val result =
            SparseState.compact (Seq.reduce SparseState.merge SparseState.empty
              (Seq.map (applyGate state) layer))
        in
          dump false result;
          result
        end
    in
      dump true SparseState.initial;
      Seq.iterate doLayer SparseState.initial circuit
    end

end
