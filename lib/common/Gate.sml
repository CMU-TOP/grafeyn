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

  datatype pull_gate_action =
    PullNonBranching of BasisIdx.t -> BasisIdx.t * weight
  | PullBranching of BasisIdx.t -> (BasisIdx.t * weight) * (BasisIdx.t * weight)

  type gate =
    { touches: qubit_idx Seq.t
    , action: gate_action
    , pullAction: pull_gate_action option
    }

  type t = gate

  val fromGateDefn: GateDefn.t -> gate

  val expectBranching: gate -> bool

  val pullable: gate -> bool
  val pullAction: gate -> pull_gate_action option

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

  datatype pull_gate_action =
    PullNonBranching of BasisIdx.t -> BasisIdx.t * weight
  | PullBranching of BasisIdx.t -> (BasisIdx.t * weight) * (BasisIdx.t * weight)
  (* | PullBranchingRealModifier of
      BasisIdx.t
      -> (BasisIdx.t * C.r) * (BasisIdx.t * C.r) *)

  type gate =
    { touches: qubit_idx Seq.t
    , action: gate_action
    , pullAction: pull_gate_action option
    }

  type t = gate

  fun pullable ({touches, action, pullAction}: gate) =
    case pullAction of
      NONE => false
    | SOME _ => true

  fun pullAction ({pullAction = a, ...}: gate) = a

  val one = R.fromLarge 1.0
  val half = R.fromLarge 0.5
  val recp_sqrt_2 = R.fromLarge Constants.RECP_SQRT_2
  val neg_recp_sqrt_2 = R.~ recp_sqrt_2

  (* ========================================================================
   * convert a push-only gate into a push-pull gate
   *)

  fun makePushPull ({touches, action}) =
    let
      val pullAction =
        case (Seq.length touches, action) of
          (1, NonBranching apply) =>
            let
              val qi = Seq.nth touches 0
              val (b0, m0) = apply (BasisIdx.zeros, C.real one)
              val (b1, m1) = apply (BasisIdx.set BasisIdx.zeros qi, C.real one)

              val notFlipped = BasisIdx.equal (b0, BasisIdx.zeros)

              val action =
                if notFlipped then
                  fn bidx => (bidx, if BasisIdx.get bidx qi then m1 else m0)
                else
                  fn bidx =>
                    ( BasisIdx.flip bidx qi
                    , if BasisIdx.get bidx qi then m0 else m1
                    )
            in
              SOME (PullNonBranching action)
            end

        | (2, NonBranching apply) =>
            let
              val (qi, qj) = (Seq.nth touches 0, Seq.nth touches 1)

              (* |00⟩  ->  m00 * |b00⟩
               * |01⟩  ->  m01 * |b01⟩
               * |10⟩  ->  m10 * |b10⟩
               * |11⟩  ->  m11 * |b11⟩
               *)
              val (b00, m00) = apply (BasisIdx.zeros, C.real one)
              val (b01, m01) = apply
                (BasisIdx.set BasisIdx.zeros qj, C.real one)
              val (b10, m10) = apply
                (BasisIdx.set BasisIdx.zeros qi, C.real one)
              val (b11, m11) = apply
                (BasisIdx.set (BasisIdx.set BasisIdx.zeros qi) qj, C.real one)

              fun match left right bb =
                left = BasisIdx.get bb qi andalso right = BasisIdx.get bb qj

              fun find left right =
                if match left right b00 then (b00, m00)
                else if match left right b01 then (b01, m01)
                else if match left right b10 then (b10, m10)
                else (b11, m11)


              (* Invert the lookup.
               *
               * |b00'⟩  ->  m00' * |00⟩
               * |b01'⟩  ->  m01' * |01⟩
               * |b10'⟩  ->  m10' * |10⟩
               * |b11'⟩  ->  m11' * |11⟩
               *)
              val (b00', m00') = find false false
              val (b01', m01') = find false true
              val (b10', m10') = find true false
              val (b11', m11') = find true true


              fun alignWith bb bidx =
                let
                  val bidx = BasisIdx.setTo (BasisIdx.get bb qi) bidx qi
                  val bidx = BasisIdx.setTo (BasisIdx.get bb qj) bidx qj
                in
                  bidx
                end


              fun action bidx =
                if BasisIdx.get bidx qi then
                  (if BasisIdx.get bidx qj then (alignWith b11' bidx, m11')
                   else (alignWith b10' bidx, m10'))
                else
                  (if BasisIdx.get bidx qj then (alignWith b01' bidx, m01')
                   else (alignWith b00' bidx, m00'))
            in
              SOME (PullNonBranching action)
            end

        | (1, Branching apply) =>
            let
              val qi = Seq.nth touches 0

              (* |0⟩  ->  m00 * |b00⟩ + m01 * |b01⟩
               * |1⟩  ->  m10 * |b10⟩ + m11 * |b11⟩
               *)
              val ((b00, m00), (b01, m01)) = apply (BasisIdx.zeros, C.real one)
              val ((b10, m10), (b11, m11)) = apply
                (BasisIdx.set BasisIdx.zeros qi, C.real one)

              val ((b00, m00), (b01, m01)) =
                if BasisIdx.get b00 qi then ((b01, m01), (b00, m00))
                else ((b00, m00), (b01, m01))

              val ((b10, m10), (b11, m11)) =
                if BasisIdx.get b10 qi then ((b11, m11), (b10, m10))
                else ((b10, m10), (b11, m11))

              (* sanity check *)
              val _ =
                if
                  not (BasisIdx.get b00 qi) andalso not (BasisIdx.get b10 qi)
                  andalso BasisIdx.get b01 qi andalso BasisIdx.get b11 qi
                then
                  ()
                else
                  raise Fail
                    "Gate.pushPull: branching gate doesn't output in order |0⟩, |1⟩"

              fun action bidx =
                if BasisIdx.get bidx qi then
                  ((BasisIdx.unset bidx qi, m01), (bidx, m11))
                else
                  ((bidx, m00), (BasisIdx.set bidx qi, m10))
            in
              SOME (PullBranching action)
            end

        | _ => NONE

    in
      {touches = touches, action = action, pullAction = pullAction}
    end


  (* =========================================================================
   * push gate definitions
   *)

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
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
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
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun cz {control, target} =
    let
      val xx = R.~ one

      val Cone = C.real one
      val Cnegone = C.real xx

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


      fun pullApply bidx =
        if BasisIdx.get bidx control andalso BasisIdx.get bidx target then
          (bidx, Cnegone)
        else
          (bidx, Cone)
    in
      { touches = Seq.fromList [control, target]
      , action = NonBranching apply
      , pullAction = SOME (PullNonBranching pullApply)
      }
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
      makePushPull {touches = Seq.singleton qi, action = Branching apply}
    end


  fun sqrtx qi =
    let
      val xx = C.make (half, half)
      val yy = C.make (half, R.~ half)

      fun apply (bidx, weight) =
        let
          val bidx1 = BasisIdx.unset bidx qi
          val bidx2 = BasisIdx.set bidx qi

          val weightA = C.* (weight, xx)
          val weightB = C.* (weight, yy)
        in
          if BasisIdx.get bidx qi then ((bidx1, weightB), (bidx2, weightA))
          else ((bidx1, weightA), (bidx2, weightB))
        end


      fun pullApply bidx =
        let
          val bidx0 = BasisIdx.unset bidx qi
          val bidx1 = BasisIdx.set bidx qi
        in
          if BasisIdx.get bidx qi then ((bidx0, yy), (bidx1, xx))
          else ((bidx0, xx), (bidx1, yy))
        end

    in
      { touches = Seq.singleton qi
      , action = Branching apply
      , pullAction = SOME (PullBranching pullApply)
      }
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
      makePushPull {touches = Seq.singleton qi, action = Branching apply}
    end


  fun hadamard qi =
    let
      val Crs2 = C.real recp_sqrt_2
      val Cnrs2 = C.real neg_recp_sqrt_2

      fun apply (bidx, weight) =
        let
          val bidx0 = BasisIdx.unset bidx qi
          val bidx1 = BasisIdx.set bidx qi
        in
          if BasisIdx.get bidx qi then
            ( (bidx0, C.scale (recp_sqrt_2, weight))
            , (bidx1, C.scale (neg_recp_sqrt_2, weight))
            )
          else
            let val weight' = C.scale (recp_sqrt_2, weight)
            in ((bidx0, weight'), (bidx1, weight'))
            end
        end

      fun pullApply bidx =
        let
          val bidx0 = BasisIdx.unset bidx qi
          val bidx1 = BasisIdx.set bidx qi
        in
          if BasisIdx.get bidx qi then ((bidx0, Crs2), (bidx1, Cnrs2))
          else ((bidx0, Crs2), (bidx1, Crs2))
        end
    in
      { touches = Seq.singleton qi
      , action = Branching apply
      , pullAction = SOME (PullBranching pullApply)
      }
    end


  fun cx {control = ci, target = ti} =
    let
      val Cone = C.real one

      fun apply (bidx, weight) =
        let
          val bidx' =
            if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
        in
          (bidx', weight)
        end

      fun pullApply bidx =
        (if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx, Cone)
    in
      { touches = Seq.fromList [ci, ti]
      , action = NonBranching apply
      , pullAction = SOME (PullNonBranching pullApply)
      }
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
      makePushPull
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
          (bidx, weight')
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun x qi =
    let
      fun apply (bidx, weight) =
        let val bidx' = BasisIdx.flip bidx qi
        in (bidx', weight)
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
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
      makePushPull
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
      makePushPull
        {touches = Seq.fromList [left, right], action = MaybeBranching apply}
    end


  fun rz {rot, target} =
    let
      val x = R./ (R.fromLarge rot, R.fromLarge 2.0)
      val rot0 = C.rotateBy (R.~ x)
      val rot1 = C.rotateBy x

      fun apply (bidx, weight) =
        let val mult = if BasisIdx.get bidx target then rot1 else rot0
        in (bidx, C.* (mult, weight))
        end

      fun pullApply bidx =
        if BasisIdx.get bidx target then (bidx, rot0) else (bidx, rot1)
    in
      { touches = Seq.singleton target
      , action = NonBranching apply
      , pullAction = SOME (PullNonBranching pullApply)
      }
    end


  fun ry {rot, target} =
    let
      val rot = R./ (R.fromLarge rot, R.fromLarge 2.0)
      val s = R.Math.sin rot
      val c = R.Math.cos rot
      val ns = R.~ s
      val xx = (ns, c)
      val yy = (c, s)

      val Cs = C.real s
      val Cns = C.real ns
      val Cc = C.real c

      fun apply (bidx, weight) =
        let
          val bidx0 = BasisIdx.unset bidx target
          val bidx1 = BasisIdx.set bidx target
          val (mult0, mult1) = if BasisIdx.get bidx target then xx else yy
        in
          ((bidx0, C.scale (mult0, weight)), (bidx1, C.scale (mult1, weight)))
        end


      fun pullApply bidx =
        let
          val bidx0 = BasisIdx.unset bidx target
          val bidx1 = BasisIdx.set bidx target
        in
          if BasisIdx.get bidx target then ((bidx0, Cc), (bidx1, Cns))
          else ((bidx0, Cs), (bidx1, Cc))
        end

    in
      { touches = Seq.singleton target
      , action = Branching apply
      , pullAction = SOME (PullBranching pullApply)
      }
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
      makePushPull
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
          (*
          let
            val fw = condMult (d, c)
          in
            NonBranching (fn (bidx, weight) =>
              (BasisIdx.set bidx target, fw (BasisIdx.get bidx target) weight))
          end
          *)
          raise Fail "Gate.singleQubitUnitary: impossible?"

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
          (*
          let
            val fw = condMult (b, a)
          in
            NonBranching (fn (bidx, weight) =>
              (BasisIdx.unset bidx target, fw (BasisIdx.get bidx target) weight))
          end
          *)
          raise Fail "Gate.singleQubitUnitary: impossible?"

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


      val pullAction =
        if C.isZero a andalso C.isZero b then
          raise Fail "Gate.singleQubitUnitary: impossible?"
        else if C.isZero a andalso C.isZero d then
          PullNonBranching (fn bidx =>
            ( BasisIdx.flip bidx target
            , if BasisIdx.get bidx target then c else b
            ))
        else if C.isZero c andalso C.isZero b then
          PullNonBranching (fn bidx =>
            (bidx, if BasisIdx.get bidx target then d else a))
        else if C.isZero c andalso C.isZero d then
          raise Fail "Gate.singleQubitUnitary: impossible?"
        else
          PullBranching (fn bidx =>
            let
              val bidx0 = BasisIdx.unset bidx target
              val bidx1 = BasisIdx.set bidx target
            in
              if BasisIdx.get bidx target then ((bidx0, c), (bidx1, d))
              else ((bidx0, a), (bidx1, b))
            end)
    in
      { touches = Seq.singleton target
      , action = action
      , pullAction = SOME pullAction
      }
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


  fun rx {rot = theta, target} =
    let
      val theta = R.fromLarge theta
      val cos = C.real (R.Math.cos (R./ (theta, R.fromLarge 2.0)))
      val sin = C.real (R.Math.sin (R./ (theta, R.fromLarge 2.0)))
      val a = cos
      val b = C.* (C.~ C.i, sin)
      val c = b
      val d = a
    in
      singleQubitUnitary {target = target, mat = (a, b, c, d)}
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
    | GateDefn.CZ xx => cz xx
    | GateDefn.CCX xx => ccx xx
    | GateDefn.CPhase xx => cphase xx
    | GateDefn.FSim xx => fsim xx
    | GateDefn.RX xx => rx xx
    | GateDefn.RZ xx => rz xx
    | GateDefn.RY xx => ry xx
    | GateDefn.CSwap xx => cswap xx
    | GateDefn.U xx => u xx
    | GateDefn.Other xx =>
        { touches = Seq.empty ()
        , action = Branching (fn _ =>
            Util.die ("ERROR: Gate: don't know how to apply gate: " ^ #name xx))
        , pullAction = NONE
        }

end
