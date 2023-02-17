structure Gate :>
sig
  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | X of qubit_idx
  | T of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }

  type t = gate
  type weight = Complex.t
  type weighted_idx = BasisIdx.t * weight

  val expectBranching: gate -> bool

  datatype gate_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  val applyState: gate -> SparseState.t -> SparseState.t

  val apply: gate -> weighted_idx -> gate_output
end =
struct

  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | T of qubit_idx
  | X of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }

  type t = gate
  type weight = Complex.t
  type weighted_idx = BasisIdx.t * weight

  datatype gate_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx


  fun pauliy qi (bidx, weight) =
    let
      val bidx' = BasisIdx.flip bidx qi
      val multiplier =
        if BasisIdx.get bidx qi then Complex.imag ~1.0 else Complex.imag 1.0
      val weight' = Complex.* (weight, multiplier)
    in
      OutputOne (bidx', weight')
    end

  fun pauliz qi (bidx, weight) =
    let
      val multiplier =
        if BasisIdx.get bidx qi then Complex.real ~1.0 else Complex.real 1.0
      val weight' = Complex.* (weight, multiplier)
    in
      OutputOne (bidx, weight')
    end


  fun hadamard qi (bidx, weight) =
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
      OutputTwo ((bidx1, weight1), (bidx2, weight2))
    end


  fun cx {control = ci, target = ti} (bidx, weight) =
    let val bidx' = if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
    in OutputOne (bidx', weight)
    end


  fun t qi (bidx, weight) =
    let
      val multiplier =
        if BasisIdx.get bidx qi then
          Complex.+
            ( Complex.real Constants.RECP_SQRT_2
            , Complex.imag Constants.RECP_SQRT_2
            )
        else
          Complex.real 1.0
    in
      OutputOne (bidx, Complex.* (weight, multiplier))
    end

  fun x qi (bidx, weight) =
    let val bidx' = BasisIdx.flip bidx qi
    in OutputOne (bidx', weight)
    end


  fun cphase {control, target, rot} (bidx, weight) =
    let
      val weight =
        if not (BasisIdx.get bidx control) orelse not (BasisIdx.get bidx target) then
          weight
        else
          Complex.* (Complex.rotateBy rot, weight)
    in
      OutputOne (bidx, weight)
    end


  fun expectBranching gate =
    case gate of
      Hadamard _ => true
    | _ => false


  fun apply gate widx =
    case gate of
      PauliY xx => pauliy xx widx
    | PauliZ xx => pauliz xx widx
    | Hadamard xx => hadamard xx widx
    | T xx => t xx widx
    | X xx => x xx widx
    | CX xx => cx xx widx
    | CPhase xx => cphase xx widx


  fun gateOutputToSeq go =
    case go of
      OutputOne x => Seq.singleton x
    | OutputTwo (x, y) => Seq.fromList [x, y]


  fun gateOutputToOne go =
    case go of
      OutputOne x => x
    | _ => raise Fail "bug: Gate.gateOutputToOne"


  fun applyState gate state =
    if expectBranching gate then
      SparseState.fromSeq (Seq.flatten
        (Seq.map (gateOutputToSeq o apply gate) (SparseState.toSeq state)))
    else
      SparseState.fromSeq
        (Seq.map (gateOutputToOne o apply gate) (SparseState.toSeq state))

end
