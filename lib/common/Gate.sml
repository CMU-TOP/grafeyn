signature GATE =
sig
  structure C: COMPLEX

  type qubit_idx = int

  (* Underlying Gate structure is taken from sml-qasm. We reuse those gate
   * definitions but provide extended functionality here.
   *)
  datatype gate = datatype SMLQasmGate.gate
  (*
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | SqrtY of qubit_idx
  | SqrtX of qubit_idx
  | SqrtW of qubit_idx
  | X of qubit_idx
  | T of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CCX of {control1: qubit_idx, control2: qubit_idx, target: qubit_idx}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }
  
  (** fsim(theta, phi) :=
    *   [ [ 1,   0,              0,              0          ],
    *     [ 0,   cos(theta),     -i sin(theta),  0          ],
    *     [ 0,   -i sin(theta),  cos(theta),     0          ],
    *     [ 0,   0,              0,              e^(-i phi) ] ]
    *)
  | FSim of {left: qubit_idx, right: qubit_idx, theta: real, phi: real}
  | RZ of {rot: real, target: qubit_idx}
  | RY of {rot: real, target: qubit_idx}
  | CSwap of {control: qubit_idx, target1: qubit_idx, target2: qubit_idx}
  *)

  type t = gate
  type weight = C.t
  type weighted_idx = BasisIdx.t * weight

  val expectBranching: gate -> bool

  datatype gate_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  val applyState: gate -> SparseState.t -> SparseState.t

  val apply: gate -> weighted_idx -> gate_output
end


functor Gate(C: COMPLEX): GATE =
struct

  open SMLQasmGate (* from sml-qasm *)

  structure C = C
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type qubit_idx = int

  datatype gate = datatype SMLQasmGate.gate
  type t = gate
  type weight = C.t
  type r = C.r
  type weighted_idx = BasisIdx.t * weight

  datatype gate_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx


  val one = R.fromLarge 1.0
  val half = R.fromLarge 0.5
  val recp_sqrt_2 = R.fromLarge Constants.RECP_SQRT_2

  fun pauliy qi (bidx, weight) =
    let
      val bidx' = BasisIdx.flip bidx qi
      val multiplier =
        if BasisIdx.get bidx qi then C.imag (R.~ one) else C.imag one
      val weight' = C.* (weight, multiplier)
    in
      OutputOne (bidx', weight')
    end


  fun pauliz qi (bidx, weight) =
    let
      val multiplier =
        if BasisIdx.get bidx qi then C.real (R.~ one) else C.real one
      val weight' = C.* (weight, multiplier)
    in
      OutputOne (bidx, weight')
    end


  fun sqrty qi (bidx, weight) =
    let
      val bidx1 = BasisIdx.set bidx qi false
      val bidx2 = BasisIdx.set bidx qi true

      val multiplier1 = C.make (half, R.~ half)
      val multiplier2 =
        if BasisIdx.get bidx qi then C.make (half, R.~ half)
        else C.make (R.~ half, half)

      val weight1 = C.* (weight, multiplier1)
      val weight2 = C.* (weight, multiplier2)
    in
      OutputTwo ((bidx1, weight1), (bidx2, weight2))
    end


  fun sqrtx qi (bidx, weight) =
    let
      val bidx1 = BasisIdx.set bidx qi false
      val bidx2 = BasisIdx.set bidx qi true

      val weightA = C.* (weight, C.make (half, half))
      val weightB = C.* (weight, C.make (half, R.~ half))
    in
      if BasisIdx.get bidx qi then
        OutputTwo ((bidx1, weightB), (bidx2, weightA))
      else
        OutputTwo ((bidx1, weightA), (bidx2, weightB))
    end


  fun sqrtw qi (bidx, weight) =
    let
      val bidx1 = BasisIdx.set bidx qi false
      val bidx2 = BasisIdx.set bidx qi true

      val (mult1, mult2) =
        if BasisIdx.get bidx qi then
          (C.imag recp_sqrt_2, C.make (R.~ half, R.~ half))
        else
          (C.make (R.~ half, R.~ half), C.real (R.~ recp_sqrt_2))

      val weight1 = C.* (weight, mult1)
      val weight2 = C.* (weight, mult2)
    in
      OutputTwo ((bidx1, weight1), (bidx2, weight2))
    end


  fun hadamard qi (bidx, weight) =
    let
      val bidx1 = bidx
      val bidx2 = BasisIdx.flip bidx qi

      val multiplier1 =
        if BasisIdx.get bidx qi then R.~ recp_sqrt_2 else recp_sqrt_2
      val multiplier2 = recp_sqrt_2

      val weight1 = C.scale (multiplier1, weight)
      val weight2 = C.scale (multiplier2, weight)
    in
      OutputTwo ((bidx1, weight1), (bidx2, weight2))
    end


  fun cx {control = ci, target = ti} (bidx, weight) =
    let val bidx' = if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
    in OutputOne (bidx', weight)
    end


  fun ccx {control1 = ci1, control2 = ci2, target = ti} (bidx, weight) =
    let
      val bidx' =
        if BasisIdx.get bidx ci1 andalso BasisIdx.get bidx ci2 then
          BasisIdx.flip bidx ti
        else
          bidx
    in
      OutputOne (bidx', weight)
    end


  fun t qi (bidx, weight) =
    let
      val multiplier =
        if BasisIdx.get bidx qi then C.make (recp_sqrt_2, recp_sqrt_2)
        else C.real one
    in
      OutputOne (bidx, C.* (weight, multiplier))
    end

  fun x qi (bidx, weight) =
    let val bidx' = BasisIdx.flip bidx qi
    in OutputOne (bidx', weight)
    end


  fun cphase {control, target, rot} (bidx, weight) =
    let
      val rot = R.fromLarge rot
      val weight =
        if not (BasisIdx.get bidx control) orelse not (BasisIdx.get bidx target) then
          weight
        else
          C.* (C.rotateBy rot, weight)
    in
      OutputOne (bidx, weight)
    end


  fun fsim {left, right, theta, phi} (bidx, weight) =
    let
      val l = BasisIdx.get bidx left
      val r = BasisIdx.get bidx right

      val theta = R.fromLarge theta
      val phi = R.fromLarge phi
    in
      case (l, r) of
        (false, false) => OutputOne (bidx, weight)
      | (true, true) => OutputOne (bidx, C.* (weight, C.rotateBy (R.~ phi)))
      | _ =>
          let
            val bidx1 = bidx
            val bidx2 = BasisIdx.flip (BasisIdx.flip bidx left) right
            val weight1 = C.* (weight, C.real (R.Math.cos theta))
            val weight2 = C.* (weight, C.imag (R.~ (R.Math.sin theta)))
          in
            if BasisIdx.get bidx left then
              OutputTwo ((bidx1, weight2), (bidx2, weight1))
            else
              OutputTwo ((bidx1, weight1), (bidx2, weight2))
          end
    end


  fun rz {rot, target} (bidx, weight) =
    let
      val rot = R./ (R.fromLarge rot, R.fromLarge 2.0)

      val mult =
        if BasisIdx.get bidx target then C.rotateBy (R.~ rot)
        else C.rotateBy rot
    in
      OutputOne (bidx, C.* (mult, weight))
    end


  fun ry {rot, target} (bidx, weight) =
    let
      val bidx0 = BasisIdx.set bidx target false
      val bidx1 = BasisIdx.set bidx target true
      val rot = R./ (R.fromLarge rot, R.fromLarge 2.0)
      val s = R.Math.sin rot
      val c = R.Math.cos rot
      val (mult0, mult1) =
        if BasisIdx.get bidx target then (R.~ s, c) else (c, s)
    in
      OutputTwo
        ((bidx0, C.scale (mult0, weight)), (bidx1, C.scale (mult1, weight)))
    end


  fun cswap {control, target1, target2} (bidx, weight) =
    let
      val bidx' =
        if BasisIdx.get bidx control then
          let
            val t1 = BasisIdx.get bidx target1
            val t2 = BasisIdx.get bidx target2
          in
            BasisIdx.set (BasisIdx.set bidx target1 t2) target2 t1
          end
        else
          bidx
    in
      OutputOne (bidx', weight)
    end


  fun expectBranching gate =
    case gate of
      Hadamard _ => true
    | SqrtY _ => true
    | SqrtX _ => true
    | SqrtW _ => true
    | FSim _ => true
    | RY _ => true
    | _ => false


  fun apply gate =
    case gate of
      PauliY xx => pauliy xx
    | PauliZ xx => pauliz xx
    | Hadamard xx => hadamard xx
    | T xx => t xx
    | SqrtY xx => sqrty xx
    | SqrtX xx => sqrtx xx
    | SqrtW xx => sqrtw xx
    | X xx => x xx
    | CX xx => cx xx
    | CCX xx => ccx xx
    | CPhase xx => cphase xx
    | FSim xx => fsim xx
    | RZ xx => rz xx
    | RY xx => ry xx
    | CSwap xx => cswap xx
    | Other xx => Util.die ("uh oh")


  fun gateOutputToSeq go =
    case go of
      OutputOne x => Seq.singleton x
    | OutputTwo (x, y) => Seq.fromList [x, y]


  fun gateOutputToOne go =
    case go of
      OutputOne x => x
    | _ => raise Fail "bug: Gate.gateOutputToOne"


  fun applyState gate state = raise Fail "Gate.applyState: deprecated"

end
