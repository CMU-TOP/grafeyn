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

  (* =========================================================================
   * pull-gates
   *)

  datatype pull_gate_action =
    PullNonBranching of BasisIdx.t -> BasisIdx.t * (weight -> weight)
  | PullBranching of
      BasisIdx.t
      -> (BasisIdx.t * (weight -> weight)) * (BasisIdx.t * (weight -> weight))

  val pushPull: gate -> pull_gate_action option

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
  val neg_recp_sqrt_2 = R.~ recp_sqrt_2

  fun pauliy qi =
    let
      val xx = C.imag (R.~ one)
      val yy = C.imag one

      fun apply (bidx, weight) =
        let
          val bidx' = BasisIdx.flip bidx qi
          val multiplier = if BasisIdx.get bidx qi then xx else yy
          val weight' = C.* (weight, multiplier)
        in
          (bidx', weight')
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun pauliz qi =
    let
      val xx = R.~ one

      fun apply (bidx, weight) =
        let
          val weight' =
            if BasisIdx.get bidx qi then C.scale (xx, weight) else weight
        in
          (bidx, weight')
        end
    in
      {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun cz {control, target} =
    let
      val xx = R.~ one
      fun apply (bidx, weight) =
        let
          val weight' =
            if BasisIdx.get bidx control andalso BasisIdx.get bidx target then
              C.scale (xx, weight)
            else
              weight
        in
          (bidx, weight')
        end
    in
      {touches = Seq.fromList [control, target], action = NonBranching apply}
    end


  fun sqrty qi =
    let
      fun apply (bidx, weight) =
        let
          val bidx1 = BasisIdx.unset bidx qi
          val bidx2 = BasisIdx.set bidx qi

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
          val bidx1 = BasisIdx.unset bidx qi
          val bidx2 = BasisIdx.set bidx qi

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
          val bidx1 = BasisIdx.unset bidx qi
          val bidx2 = BasisIdx.set bidx qi

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
            if BasisIdx.get bidx qi then neg_recp_sqrt_2 else recp_sqrt_2
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
      val mult = C.make (recp_sqrt_2, recp_sqrt_2)

      fun apply (bidx, weight) =
        let
          val weight' =
            if BasisIdx.get bidx qi then C.* (weight, mult) else weight
        in
          (bidx, weight)
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
      val rot = R.fromLarge rot
      val mult = C.rotateBy rot

      fun apply (bidx, weight) =
        let
          val weight =
            if
              not (BasisIdx.get bidx control)
              orelse not (BasisIdx.get bidx target)
            then weight
            else C.* (mult, weight)
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
          val bidx0 = BasisIdx.unset bidx target
          val bidx1 = BasisIdx.set bidx target
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
              BasisIdx.swap bidx (target1, target2)
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


  (* condMult (x, y) b w == C.* (if b then x else y, w)
   * we partially apply it for improved performance
   *)
  fun condMult (x, y) =
    let
      val (xre, xim) = C.view x
      val (yre, yim) = C.view y
    in
      if C.realIsZero xim andalso C.realIsZero yim then
        (fn b => fn w => C.scale (if b then xre else yre, w))
      else
        (fn b => fn w => C.* (if b then x else y, w))
    end


  (* | a b |
   * | c d |
   *)
  fun singleQubitUnitary {target, mat = (a, b, c, d)} =
    let
      val action =
        if C.isZero a andalso C.isZero b then
          let
            val fw = condMult (d, c)
          in
            NonBranching (fn (bidx, weight) =>
              (BasisIdx.set bidx target, fw (BasisIdx.get bidx target) weight))
          end

        else if C.isZero a andalso C.isZero d then
          let
            val fw = condMult (b, c)
          in
            NonBranching (fn (bidx, weight) =>
              (BasisIdx.flip bidx target, fw (BasisIdx.get bidx target) weight))
          end

        else if C.isZero c andalso C.isZero b then
          let
            val fw = condMult (d, a)
          in
            NonBranching (fn (bidx, weight) =>
              (bidx, fw (BasisIdx.get bidx target) weight))
          end

        else if C.isZero c andalso C.isZero d then
          let
            val fw = condMult (b, a)
          in
            NonBranching (fn (bidx, weight) =>
              (BasisIdx.unset bidx target, fw (BasisIdx.get bidx target) weight))
          end

        else
          Branching (fn (bidx, weight) =>
            let
              val bidx0 = BasisIdx.unset bidx target
              val bidx1 = BasisIdx.set bidx target

              val (mult0, mult1) =
                if BasisIdx.get bidx target then (b, d) else (a, c)
            in
              ((bidx0, C.* (mult0, weight)), (bidx1, C.* (mult1, weight)))
            end)
    in
      {touches = Seq.singleton target, action = action}
    end


  (*
    (* 1/2 [1 + e^(i theta)]               [-i e^(i lambda) (1 - e^(i theta))]
     *     [i e^(i phi) (1 - e^(i theta)]  [e^(i (phi + lambda)) (1 + e^(i theta))]
     *)
    fun u {target, theta, phi, lambda} =
      let
        val theta = R.fromLarge theta
        val phi = R.fromLarge phi
        val lambda = R.fromLarge lambda
  
        val onePlusEITheta = C.+ (C.real one, C.rotateBy theta)
        val oneMinusEITheta = C.- (C.real one, C.rotateBy theta)
  
        val a = onePlusEITheta
        val b = C.* (C.* (C.~ C.i, C.rotateBy lambda), oneMinusEITheta)
        val c = C.* (C.* (C.i, C.rotateBy phi), oneMinusEITheta)
        val d = C.* (C.rotateBy (R.+ (phi, lambda)), onePlusEITheta)
  
        val a = C.scale (half, a)
        val b = C.scale (half, b)
        val c = C.scale (half, c)
        val d = C.scale (half, d)
      in
        print
          ("u(" ^ R.toString theta ^ "," ^ R.toString phi ^ ","
           ^ R.toString lambda ^ ")\n");
        print ("  " ^ C.toString a ^ "\t" ^ C.toString b ^ "\n");
        print ("  " ^ C.toString c ^ "\t" ^ C.toString d ^ "\n");
        singleQubitUnitary {target = target, mat = (a, b, c, d)}
      end
    *)

  (*
  
  theta, phi, lam = (float(param) for param in self.params)
  cos = math.cos(theta / 2)
  sin = math.sin(theta / 2)
  return numpy.array(
      [
          [cos, -exp(1j * lam) * sin],
          [exp(1j * phi) * sin, exp(1j * (phi + lam)) * cos],
      ],
      dtype=dtype,
  )
  
  *)

  (* cos(theta/2)              -e^(i lambda) sin(theta/2)
   * e^(i phi) sin(theta/2)   e^(i(phi+lambda)) cos(theta/2)
   *)
  fun u {target, theta, phi, lambda} =
    let
      val theta = R.fromLarge theta
      val phi = R.fromLarge phi
      val lambda = R.fromLarge lambda

      val cos = C.real (R.Math.cos (R./ (theta, R.fromLarge 2.0)))
      val sin = C.real (R.Math.sin (R./ (theta, R.fromLarge 2.0)))

      (* val _ = print
        ("e^(i(phi+lambda)) = " ^ C.toString (C.rotateBy (R.+ (phi, lambda)))
         ^ "\n") *)

      val a = cos
      val b = C.~ (C.* (sin, C.rotateBy lambda))
      val c = C.* (sin, C.rotateBy phi)
      val d = C.* (cos, C.rotateBy (R.+ (phi, lambda)))
    in
      (* print
        ("u(" ^ R.toString theta ^ "," ^ R.toString phi ^ ","
         ^ R.toString lambda ^ ")\n");
      print ("  " ^ C.toString a ^ "\t" ^ C.toString b ^ "\n");
      print ("  " ^ C.toString c ^ "\t" ^ C.toString d ^ "\n"); *)
      singleQubitUnitary {target = target, mat = (a, b, c, d)}
    end


  (*
  fun u {target, theta, phi, lambda} =
    let
      val theta = R.fromLarge theta
      val phi = R.fromLarge phi
      val lambda = R.fromLarge lambda
  
      val s = C.real (R.Math.sin (R./ (theta, R.fromLarge 2.0)))
      val c = C.real (R.Math.cos (R./ (theta, R.fromLarge 2.0)))
  
      val phiPlusLambdaBy2 = R./ (R.+ (phi, lambda), R.fromLarge 2.0)
      val phiMinusLambdaBy2 = R./ (R.- (phi, lambda), R.fromLarge 2.0)
  
      val a = C.* (c, C.rotateBy (R.~ phiPlusLambdaBy2))
      val b = C.~ (C.* (s, C.rotateBy (R.~ phiMinusLambdaBy2)))
      val c = C.* (s, C.rotateBy phiMinusLambdaBy2)
      val d = C.* (c, C.rotateBy phiPlusLambdaBy2)
    in
      print
        ("u(" ^ R.toString theta ^ "," ^ R.toString phi ^ ","
         ^ R.toString lambda ^ ")\n");
      print ("  " ^ C.toString a ^ "\t" ^ C.toString b ^ "\n");
      print ("  " ^ C.toString c ^ "\t" ^ C.toString d ^ "\n");
      singleQubitUnitary {target = target, mat = (a, b, c, d)}
    end
  *)

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
    | GateDefn.CZ xx => cz xx
    | GateDefn.CCX xx => ccx xx
    | GateDefn.CPhase xx => cphase xx
    | GateDefn.FSim xx => fsim xx
    | GateDefn.RZ xx => rz xx
    | GateDefn.RY xx => ry xx
    | GateDefn.CSwap xx => cswap xx
    | GateDefn.U xx => u xx
    | GateDefn.Other xx =>
        { touches = Seq.empty ()
        , action = Branching (fn _ =>
            Util.die ("ERROR: Gate: don't know how to apply gate: " ^ #name xx))
        }


  (* ========================================================================
   * pull-gates
   *)

  datatype pull_gate_action =
    PullNonBranching of BasisIdx.t -> BasisIdx.t * (weight -> weight)
  | PullBranching of
      BasisIdx.t
      -> (BasisIdx.t * (weight -> weight)) * (BasisIdx.t * (weight -> weight))

  type pullgate = {touches: qubit_idx Seq.t, action: pull_gate_action}

  fun pushPull ({touches, action}: gate) =
    case (Seq.length touches, action) of
      (1, NonBranching apply) =>
        let
          val qi = Seq.nth touches 0
          val (b0, m0) = apply (BasisIdx.zeros, C.real one)
          val (b1, m1) = apply (BasisIdx.set BasisIdx.zeros qi, C.real one)

          val notFlipped = BasisIdx.equal (b0, BasisIdx.zeros)

          val action =
            if notFlipped then
              fn bidx =>
                ( bidx
                , fn w =>
                    if BasisIdx.get bidx qi then C.* (m1, w) else C.* (m0, w)
                )
            else
              fn bidx =>
                ( BasisIdx.flip bidx qi
                , fn w =>
                    if BasisIdx.get bidx qi then C.* (m0, w) else C.* (m1, w)
                )
        in
          SOME (PullNonBranching action)
        end

    | _ => NONE

end
