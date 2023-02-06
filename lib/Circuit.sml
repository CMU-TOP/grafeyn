structure Circuit =
struct

  type circuit_layer = Gate.t Seq.t
  type circuit = circuit_layer Seq.t
  type t = circuit

  fun simulate {numQubits: int} circuit =
    let
      fun dump isFirst state =
        let val front = if isFirst then "" else "--------\n"
        in print (front ^ SparseState.toString {numQubits = numQubits} state)
        end

      fun doLayer (state, layer) =
        let
          val result =
            SparseState.compact (Seq.reduce SparseState.merge SparseState.empty
              (Seq.map (fn g => Gate.apply g state) layer))
        in
          dump false result;
          result
        end
    in
      dump true SparseState.initial;
      Seq.iterate doLayer SparseState.initial circuit
    end

end
