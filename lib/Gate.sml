structure Gate :>
sig
  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}

  type t = gate

  val apply: gate -> SparseState.t -> SparseState.t
end =
struct

  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}

  type t = gate


  fun pauliy state qi =
    let
      fun f (bidx, weight) =
        let
          val bidx' = BasisIdx.flip bidx qi
          val multiplier =
            if BasisIdx.get bidx qi then Complex.imag ~1.0 else Complex.imag 1.0
          val weight' = Complex.* (weight, multiplier)
        in
          (bidx', weight')
        end
    in
      SparseState.fromSeq (Seq.map f (SparseState.toSeq state))
    end


  fun pauliz state qi =
    let
      fun f (bidx, weight) =
        let
          val multiplier =
            if BasisIdx.get bidx qi then Complex.real ~1.0 else Complex.real 1.0
          val weight' = Complex.* (weight, multiplier)
        in
          (bidx, weight')
        end
    in
      SparseState.fromSeq (Seq.map f (SparseState.toSeq state))
    end


  fun hadamard state qi =
    let
      fun f (bidx, weight) =
        let
          val bidx1 = bidx
          val bidx2 = BasisIdx.flip bidx qi

          val multiplier1 =
            if BasisIdx.get bidx qi then
              Complex.~ (Complex.real Constants.RECP_SQRT_2)
            else
              Complex.real Constants.RECP_SQRT_2

          val multiplier2 = Complex.real Constants.RECP_SQRT_2

          val weight1 = Complex.* (weight, multiplier1)
          val weight2 = Complex.* (weight, multiplier2)
        in
          Seq.fromList [(bidx1, weight1), (bidx2, weight2)]
        end
    in
      SparseState.fromSeq (Seq.flatten (Seq.map f (SparseState.toSeq state)))
    end


  fun cx state {control = ci, target = ti} =
    let
      fun f (bidx, weight) =
        let
          val bidx' =
            if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
        in
          (bidx', weight)
        end
    in
      SparseState.fromSeq (Seq.map f (SparseState.toSeq state))
    end


  fun apply gate state =
    case gate of
      PauliY qi => pauliy state qi
    | PauliZ qi => pauliz state qi
    | Hadamard qi => hadamard state qi
    | CX qis => cx state qis

end
