signature GATE =
sig
  structure C: COMPLEX
  structure B: BASIS_IDX

  type qubit_idx = int
  type weight = C.t
  type weighted_idx = B.t * weight

  datatype maybe_branching_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  datatype gate_action =
    NonBranching of weighted_idx -> weighted_idx
  | Branching of weighted_idx -> weighted_idx * weighted_idx
  | MaybeBranching of weighted_idx -> maybe_branching_output

  datatype pull_gate_action =
    PullNonBranching of B.t -> B.t * weight
  | PullBranching of B.t -> (B.t * weight) * (B.t * weight)

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


functor Gate (structure C: COMPLEX structure B: BASIS_IDX): GATE =
struct

  structure B = B
  structure C = C
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type qubit_idx = int
  type weight = C.t
  type r = C.r
  type weighted_idx = B.t * weight

  datatype maybe_branching_output =
    OutputOne of weighted_idx
  | OutputTwo of weighted_idx * weighted_idx

  datatype gate_action =
    NonBranching of weighted_idx -> weighted_idx
  | Branching of weighted_idx -> weighted_idx * weighted_idx
  | MaybeBranching of weighted_idx -> maybe_branching_output

  datatype pull_gate_action =
    PullNonBranching of B.t -> B.t * weight
  | PullBranching of B.t -> (B.t * weight) * (B.t * weight)

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
              val (b0, m0) = apply (B.zeros, C.real one)
              val (b1, m1) = apply (B.set B.zeros qi, C.real one)

              val notFlipped = B.equal (b0, B.zeros)

              val action =
                if notFlipped then
                  fn bidx => (bidx, if B.get bidx qi then m1 else m0)
                else
                  fn bidx => (B.flip bidx qi, if B.get bidx qi then m0 else m1)
            in
              SOME (PullNonBranching action)
            end

        | (2, NonBranching apply) =>
            let
              val (qi, qj) = (Seq.nth touches 0, Seq.nth touches 1)

              val a00 = B.zeros
              val a01 = B.set B.zeros qj
              val a10 = B.set B.zeros qi
              val a11 = B.set (B.set B.zeros qi) qj

              (* |00⟩  ->  m00 * |b00⟩
               * |01⟩  ->  m01 * |b01⟩
               * |10⟩  ->  m10 * |b10⟩
               * |11⟩  ->  m11 * |b11⟩
               *)
              val (b00, m00) = apply (a00, C.real one)
              val (b01, m01) = apply (a01, C.real one)
              val (b10, m10) = apply (a10, C.real one)
              val (b11, m11) = apply (a11, C.real one)

              fun match left right bb =
                left = B.get bb qi andalso right = B.get bb qj

              fun find left right =
                if match left right b00 then (a00, m00)
                else if match left right b01 then (a01, m01)
                else if match left right b10 then (a10, m10)
                else (a11, m11)


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
                  val bidx = B.setTo (B.get bb qi) bidx qi
                  val bidx = B.setTo (B.get bb qj) bidx qj
                in
                  bidx
                end


              fun action bidx =
                if B.get bidx qi then
                  (if B.get bidx qj then (alignWith b11' bidx, m11')
                   else (alignWith b10' bidx, m10'))
                else
                  (if B.get bidx qj then (alignWith b01' bidx, m01')
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
              val ((b00, m00), (b01, m01)) = apply (B.zeros, C.real one)
              val ((b10, m10), (b11, m11)) = apply
                (B.set B.zeros qi, C.real one)

              val ((b00, m00), (b01, m01)) =
                if B.get b00 qi then ((b01, m01), (b00, m00))
                else ((b00, m00), (b01, m01))

              val ((b10, m10), (b11, m11)) =
                if B.get b10 qi then ((b11, m11), (b10, m10))
                else ((b10, m10), (b11, m11))

              (* sanity check *)
              val _ =
                if
                  not (B.get b00 qi) andalso not (B.get b10 qi)
                  andalso B.get b01 qi andalso B.get b11 qi
                then
                  ()
                else
                  raise Fail
                    "Gate.pushPull: branching gate doesn't output in order |0⟩, |1⟩"

              fun action bidx =
                if B.get bidx qi then ((B.unset bidx qi, m01), (bidx, m11))
                else ((bidx, m00), (B.set bidx qi, m10))
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
          val bidx' = B.flip bidx qi
          val multiplier = if B.get bidx qi then xx else yy
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
        let val weight' = if B.get bidx qi then C.scale (xx, weight) else weight
        in (bidx, weight')
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
            if B.get bidx control andalso B.get bidx target then
              C.scale (xx, weight)
            else
              weight
        in
          (bidx, weight')
        end


      fun pullApply bidx =
        if B.get bidx control andalso B.get bidx target then (bidx, Cnegone)
        else (bidx, Cone)
    in
      { touches = Seq.fromList [control, target]
      , action = NonBranching apply
      , pullAction = SOME (PullNonBranching pullApply)
      }
    end


  fun sqrtx qi =
    let
      val xx = C.make (half, half)
      val yy = C.make (half, R.~ half)

      fun apply (bidx, weight) =
        let
          val bidx1 = B.unset bidx qi
          val bidx2 = B.set bidx qi

          val weightA = C.* (weight, xx)
          val weightB = C.* (weight, yy)
        in
          if B.get bidx qi then ((bidx1, weightB), (bidx2, weightA))
          else ((bidx1, weightA), (bidx2, weightB))
        end


      fun pullApply bidx =
        let
          val bidx0 = B.unset bidx qi
          val bidx1 = B.set bidx qi
        in
          if B.get bidx qi then ((bidx0, yy), (bidx1, xx))
          else ((bidx0, xx), (bidx1, yy))
        end

    in
      { touches = Seq.singleton qi
      , action = Branching apply
      , pullAction = SOME (PullBranching pullApply)
      }
    end


  fun sxdg qi =
    let
      val coeff = C.make (half, half)
      val negi = C.~ C.i

      fun apply (bidx, weight) =
        let
          val weight' = C.* (coeff, weight)
        in
          if B.get bidx qi then
            ((B.unset bidx qi, weight'), (bidx, C.* (negi, weight')))
          else
            ((bidx, C.* (negi, weight')), (B.set bidx qi, weight'))
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
          val bidx0 = B.unset bidx qi
          val bidx1 = B.set bidx qi
        in
          if B.get bidx qi then
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
          val bidx0 = B.unset bidx qi
          val bidx1 = B.set bidx qi
        in
          if B.get bidx qi then ((bidx0, Crs2), (bidx1, Cnrs2))
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
        let val bidx' = if B.get bidx ci then B.flip bidx ti else bidx
        in (bidx', weight)
        end

      fun pullApply bidx =
        (if B.get bidx ci then B.flip bidx ti else bidx, Cone)
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
            if B.get bidx ci1 andalso B.get bidx ci2 then B.flip bidx ti
            else bidx
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
        let val weight' = if B.get bidx qi then C.* (weight, mult) else weight
        in (bidx, weight')
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun tdg qi =
    let
      val mult = C.make (recp_sqrt_2, R.~ recp_sqrt_2)

      fun apply (bidx, weight) =
        let val weight' = if B.get bidx qi then C.* (weight, mult) else weight
        in (bidx, weight')
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun x qi =
    let
      fun apply (bidx, weight) =
        let val bidx' = B.flip bidx qi
        in (bidx', weight)
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun s qi =
    let
      fun apply (bidx, weight) =
        let
          val weight' =
            if not (B.get bidx qi) then weight else C.* (C.i, weight)
        in
          (bidx, weight')
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun sdg qi =
    let
      fun apply (bidx, weight) =
        let
          val weight' =
            if not (B.get bidx qi) then weight else C.* (C.~ C.i, weight)
        in
          (bidx, weight')
        end
    in
      makePushPull {touches = Seq.singleton qi, action = NonBranching apply}
    end


  fun phase {target, rot} =
    let
      val rot = R.fromLarge rot
      val mult = C.rotateBy rot

      fun apply (bidx, weight) =
        let
          val weight =
            if not (B.get bidx target) then weight else C.* (mult, weight)
        in
          (bidx, weight)
        end
    in
      makePushPull {touches = Seq.singleton target, action = NonBranching apply}
    end


  fun cphase {control, target, rot} =
    let
      val rot = R.fromLarge rot
      val mult = C.rotateBy rot

      fun apply (bidx, weight) =
        let
          val weight =
            if not (B.get bidx control) orelse not (B.get bidx target) then
              weight
            else
              C.* (mult, weight)
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
          val l = B.get bidx left
          val r = B.get bidx right

          val theta = R.fromLarge theta
          val phi = R.fromLarge phi
        in
          case (l, r) of
            (false, false) => OutputOne (bidx, weight)
          | (true, true) => OutputOne (bidx, C.* (weight, C.rotateBy (R.~ phi)))
          | _ =>
              let
                val bidx1 = bidx
                val bidx2 = B.flip (B.flip bidx left) right
                val weight1 = C.* (weight, C.real (R.Math.cos theta))
                val weight2 = C.* (weight, C.imag (R.~ (R.Math.sin theta)))
              in
                if B.get bidx left then
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
        let val mult = if B.get bidx target then rot1 else rot0
        in (bidx, C.* (mult, weight))
        end

      fun pullApply bidx =
        if B.get bidx target then (bidx, rot1) else (bidx, rot0)
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
          val bidx0 = B.unset bidx target
          val bidx1 = B.set bidx target
          val (mult0, mult1) = if B.get bidx target then xx else yy
        in
          ((bidx0, C.scale (mult0, weight)), (bidx1, C.scale (mult1, weight)))
        end


      fun pullApply bidx =
        let
          val bidx0 = B.unset bidx target
          val bidx1 = B.set bidx target
        in
          if B.get bidx target then ((bidx0, Cs), (bidx1, Cc))
          else ((bidx0, Cc), (bidx1, Cns))
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
            if B.get bidx control then B.swap bidx (target1, target2) else bidx
        in
          (bidx', weight)
        end
    in
      makePushPull
        { touches = Seq.fromList [control, target1, target2]
        , action = NonBranching apply
        }
    end


  fun swap {target1, target2} =
    let
      fun apply (bidx, weight) =
        (B.swap bidx (target1, target2), weight)
    in
      makePushPull
        {touches = Seq.fromList [target1, target2], action = NonBranching apply}
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
              (B.set bidx target, fw (B.get bidx target) weight))
          end
          *)
          raise Fail "Gate.singleQubitUnitary: impossible?"

        else if C.isZero a andalso C.isZero d then
          let
            val fw = condMult (b, c)
          in
            NonBranching (fn (bidx, weight) =>
              (B.flip bidx target, fw (B.get bidx target) weight))
          end

        else if C.isZero c andalso C.isZero b then
          let
            val fw = condMult (d, a)
          in
            NonBranching (fn (bidx, weight) =>
              (bidx, fw (B.get bidx target) weight))
          end

        else if C.isZero c andalso C.isZero d then
          (*
          let
            val fw = condMult (b, a)
          in
            NonBranching (fn (bidx, weight) =>
              (B.unset bidx target, fw (B.get bidx target) weight))
          end
          *)
          raise Fail "Gate.singleQubitUnitary: impossible?"

        else
          Branching (fn (bidx, weight) =>
            let
              val bidx0 = B.unset bidx target
              val bidx1 = B.set bidx target

              val (mult0, mult1) = if B.get bidx target then (b, d) else (a, c)
            in
              ((bidx0, C.* (mult0, weight)), (bidx1, C.* (mult1, weight)))
            end)


      val pullAction =
        if C.isZero a andalso C.isZero b then
          raise Fail "Gate.singleQubitUnitary: impossible?"
        else if C.isZero a andalso C.isZero d then
          PullNonBranching (fn bidx =>
            (B.flip bidx target, if B.get bidx target then c else b))
        else if C.isZero c andalso C.isZero b then
          PullNonBranching (fn bidx =>
            (bidx, if B.get bidx target then d else a))
        else if C.isZero c andalso C.isZero d then
          raise Fail "Gate.singleQubitUnitary: impossible?"
        else
          PullBranching (fn bidx =>
            let
              val bidx0 = B.unset bidx target
              val bidx1 = B.set bidx target
            in
              if B.get bidx target then ((bidx0, c), (bidx1, d))
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
    | GateDefn.Tdg xx => tdg xx
    | GateDefn.SqrtX xx => sqrtx xx
    | GateDefn.Sxdg xx => sxdg xx
    | GateDefn.S xx => s xx
    | GateDefn.Sdg xx => sdg xx
    | GateDefn.X xx => x xx
    | GateDefn.CX xx => cx xx
    | GateDefn.CZ xx => cz xx
    | GateDefn.CCX xx => ccx xx
    | GateDefn.Phase xx => phase xx
    | GateDefn.CPhase xx => cphase xx
    | GateDefn.FSim xx => fsim xx
    | GateDefn.RX xx => rx xx
    | GateDefn.RZ xx => rz xx
    | GateDefn.RY xx => ry xx
    | GateDefn.Swap xx => swap xx
    | GateDefn.CSwap xx => cswap xx
    | GateDefn.U xx => u xx
    | GateDefn.Other xx =>
        { touches = Seq.empty ()
        , action = Branching (fn _ =>
            Util.die ("ERROR: Gate: don't know how to apply gate: " ^ #name xx))
        , pullAction = NONE
        }

end
