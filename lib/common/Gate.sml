signature GATE =
sig
  structure C: COMPLEX

  type qubit_idx = int
  type weight = C.t
  type weighted_idx = BasisIdx.t * weight

  datatype maybe_branching_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  datatype gate_action =
    NonBranching of weighted_idx -> weighted_idx
  | Branching of weighted_idx -> weighted_idx * weighted_idx
  | MaybeBranching of weighted_idx -> maybe_branching_output

  type gate = {touches: qubit_idx Seq.t, action: gate_action}
  type t = gate

  val fromGateDefn: GateDefn.t -> gate

  val expectBranching: gate -> bool
end


functor Gate(C: COMPLEX): GATE =
struct

  structure C = C
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type qubit_idx = int
  type weight = C.t
  type r = C.r
  type weighted_idx = BasisIdx.t * weight

  datatype maybe_branching_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  datatype gate_action =
    NonBranching of weighted_idx -> weighted_idx
  | Branching of weighted_idx -> weighted_idx * weighted_idx
  | MaybeBranching of weighted_idx -> maybe_branching_output

  type gate = {touches: qubit_idx Seq.t, action: gate_action}
  type t = gate

  val one = R.fromLarge 1.0
  val half = R.fromLarge 0.5
  val recp_sqrt_2 = R.fromLarge Constants.RECP_SQRT_2

  fun pauliy qi =
    let
      fun apply (bidx, weight) =
        let
          val bidx' = BasisIdx.flip bidx qi
          val multiplier =
            if BasisIdx.get bidx qi then C.imag (R.~ one) else C.imag one
          val weight' = C.* (weight, multiplier)
        in
          (bidx', weight')
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun pauliz qi =
    let
      fun apply (bidx, weight) =
        let
          val multiplier =
            if BasisIdx.get bidx qi then C.real (R.~ one) else C.real one
          val weight' = C.* (weight, multiplier)
        in
          (bidx, weight')
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun sqrty qi =
    let
      fun apply (bidx, weight) =
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
          ((bidx1, weight1), (bidx2, weight2))
        end
    in
      {touches = Seq.singleton qi, action = Branching apply}
    end


  fun sqrtx qi =
    let
      fun apply (bidx, weight) =
        let
          val bidx1 = BasisIdx.set bidx qi false
          val bidx2 = BasisIdx.set bidx qi true

          val weightA = C.* (weight, C.make (half, half))
          val weightB = C.* (weight, C.make (half, R.~ half))
        in
          if BasisIdx.get bidx qi then ((bidx1, weightB), (bidx2, weightA))
          else ((bidx1, weightA), (bidx2, weightB))
        end

    in
      {touches = Seq.singleton qi, action = Branching apply}
    end


  fun sqrtw qi =
    let
      fun apply (bidx, weight) =
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
          ((bidx1, weight1), (bidx2, weight2))
        end
    in
      {touches = Seq.singleton qi, action = Branching apply}
    end


  fun hadamard qi =
    let
      fun apply (bidx, weight) =
        let
          val bidx1 = bidx
          val bidx2 = BasisIdx.flip bidx qi

          val multiplier1 =
            if BasisIdx.get bidx qi then R.~ recp_sqrt_2 else recp_sqrt_2
          val multiplier2 = recp_sqrt_2

          val weight1 = C.scale (multiplier1, weight)
          val weight2 = C.scale (multiplier2, weight)
        in
          ((bidx1, weight1), (bidx2, weight2))
        end
    in
      {touches = Seq.singleton qi, action = Branching apply}
    end


  fun cx {control = ci, target = ti} =
    let
      fun apply (bidx, weight) =
        let
          val bidx' =
            if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
        in
          (bidx', weight)
        end
    in
      {touches = Seq.fromList [ci, ti], action = NonBranching apply}
    end


  fun ccx {control1 = ci1, control2 = ci2, target = ti} =
    let
      fun apply (bidx, weight) =
        let
          val bidx' =
            if BasisIdx.get bidx ci1 andalso BasisIdx.get bidx ci2 then
              BasisIdx.flip bidx ti
            else
              bidx
        in
          (bidx', weight)
        end
    in
      {touches = Seq.fromList [ci1, ci2, ti], action = NonBranching apply}
    end


  fun t qi =
    let
      fun apply (bidx, weight) =
        let
          val multiplier =
            if BasisIdx.get bidx qi then C.make (recp_sqrt_2, recp_sqrt_2)
            else C.real one
        in
          (bidx, C.* (weight, multiplier))
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun x qi =
    let
      fun apply (bidx, weight) =
        let val bidx' = BasisIdx.flip bidx qi
        in (bidx', weight)
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun cphase {control, target, rot} =
    let
      fun apply (bidx, weight) =
        let
          val rot = R.fromLarge rot
          val weight =
            if
              not (BasisIdx.get bidx control)
              orelse not (BasisIdx.get bidx target)
            then weight
            else C.* (C.rotateBy rot, weight)
        in
          (bidx, weight)
        end
    in
      {touches = Seq.fromList [control, target], action = NonBranching apply}
    end


  fun fsim {left, right, theta, phi} =
    let
      fun apply (bidx, weight) =
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

    in
      {touches = Seq.fromList [left, right], action = MaybeBranching apply}
    end


  fun rz {rot, target} =
    let
      val x = R./ (R.fromLarge rot, R.fromLarge 2.0)
      val rot0 = C.rotateBy x
      val rot1 = C.rotateBy (R.~ x)

      fun apply (bidx, weight) =
        let val mult = if BasisIdx.get bidx target then rot1 else rot0
        in (bidx, C.* (mult, weight))
        end
    in
      {touches = Seq.singleton target, action = NonBranching apply}
    end


  fun ry {rot, target} =
    let
      val rot = R./ (R.fromLarge rot, R.fromLarge 2.0)
      val s = R.Math.sin rot
      val c = R.Math.cos rot
      val xx = (R.~ s, c)
      val yy = (c, s)

      fun apply (bidx, weight) =
        let
          val bidx0 = BasisIdx.set bidx target false
          val bidx1 = BasisIdx.set bidx target true
          val (mult0, mult1) = if BasisIdx.get bidx target then xx else yy
        in
          ((bidx0, C.scale (mult0, weight)), (bidx1, C.scale (mult1, weight)))
        end

    in
      {touches = Seq.singleton target, action = Branching apply}
    end


  fun cswap {control, target1, target2} =
    let
      fun apply (bidx, weight) =
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
          (bidx', weight)
        end
    in
      { touches = Seq.fromList [control, target1, target2]
      , action = NonBranching apply
      }
    end


  fun expectBranching (gate: gate) =
    case #action gate of
      Branching _ => true
    | MaybeBranching _ => true
    | NonBranching _ => false


  fun fromGateDefn gate =
    case gate of
      GateDefn.PauliY xx => pauliy xx
    | GateDefn.PauliZ xx => pauliz xx
    | GateDefn.Hadamard xx => hadamard xx
    | GateDefn.T xx => t xx
    | GateDefn.SqrtY xx => sqrty xx
    | GateDefn.SqrtX xx => sqrtx xx
    | GateDefn.SqrtW xx => sqrtw xx
    | GateDefn.X xx => x xx
    | GateDefn.CX xx => cx xx
    | GateDefn.CCX xx => ccx xx
    | GateDefn.CPhase xx => cphase xx
    | GateDefn.FSim xx => fsim xx
    | GateDefn.RZ xx => rz xx
    | GateDefn.RY xx => ry xx
    | GateDefn.CSwap xx => cswap xx
    | GateDefn.Other xx =>
        Util.die
          ("ERROR: Gate.apply: don't know how to apply gate: " ^ #name xx)

end
