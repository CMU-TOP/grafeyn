signature GATE2 =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX

  type qubit_idx = int

  type gate = { args: qubit_idx Seq.t,
                push: B.t -> B.t Seq.t,
                pull: B.t -> (B.t * C.t) Seq.t,
                maxBranchingFactor: int }

  type t = gate

  val fromGateDefn: GateDefn.t -> gate
  val maxBranchingFactor: gate -> int
  val push: gate -> B.t -> B.t Seq.t
  val pull: gate -> B.t -> (B.t * C.t) Seq.t
  val fuse: gate * gate -> gate
  val fuses: gate Seq.t -> gate
  val control: gate -> qubit_idx -> gate
end


functor Gate2
  (structure B: BASIS_IDX
   structure C: COMPLEX): GATE2 =
struct
  structure B = B
  structure C = C
  
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type qubit_idx = int

  type gate = { args: qubit_idx Seq.t,
                push: B.t -> B.t Seq.t,
                pull: B.t -> (B.t * C.t) Seq.t,
                maxBranchingFactor: int }

  type t = gate

  fun fuse (({args = args1, push = push1, pull = pull1, maxBranchingFactor = mbf1},
             {args = args2, push = push2, pull = pull2, maxBranchingFactor = mbf2}) : gate * gate) =
      let fun is_arg qi args = Seq.iterate (fn (b, i) => b orelse (i = qi)) false args
          val args3 = Seq.append (args1, Seq.filter (fn a2 => not (is_arg a2 args1)) args2)
          fun push3 b = Seq.flatten (Seq.map push2 (push1 b))
          (* TODO: need to merge duplicate basis indices in pull3! *)
          fun pull3 b = Seq.flatten (Seq.map (fn bc => let val (b', c') = bc in
                                                         Seq.map (fn bc' => let val (b'', c'') = bc' in (b'', C.* (c', c'')) end) (pull1 b') end) (pull2 b))
          val mbf3 = mbf1 * mbf2
      in
        { args = args3,
          push = push3,
          pull = pull3,
          maxBranchingFactor = mbf3 }
      end

  fun fuses (gs: gate Seq.t) = Seq.reduce fuse (Seq.nth gs 0) (Seq.drop gs 1)

  val one = R.fromLarge 1.0
  val half = R.fromLarge 0.5
  val recp_sqrt_2 = R.fromLarge Constants.RECP_SQRT_2
  val neg_recp_sqrt_2 = R.fromLarge Constants.NEG_RECP_SQRT_2
  val pos_1 = C.real one
  val neg_1 = C.real (R.~ one)
  val pos_i = C.imag one
  val neg_i = C.imag (R.~ one)
  val pos_h1 = C.real recp_sqrt_2
  val neg_h1 = C.real neg_recp_sqrt_2
  val pos_hi = C.imag recp_sqrt_2
  val neg_hi = C.imag neg_recp_sqrt_2

  fun control (g: gate) qi =
    if Seq.iterate (fn (b, i) => b orelse (i = qi)) false (#args g) then
      raise Fail "Cannot control a gate with a qubit it already uses"
    else
      { args = Seq.append (Seq.singleton qi, #args g),
        push = fn b => if B.get b qi then #push g b else Seq.singleton b,
        pull = fn b => if B.get b qi then #pull g b else Seq.singleton (b, pos_1),
        maxBranchingFactor = #maxBranchingFactor g }

  (* Given a basis idx b, return a list of tuples (b', c'), where
   *   b' = basis idx to pull from
   *   c' = scalar to multiply the weight of b' by
   *)
  fun pullFromGateDefn gd b = case gd of

    GateDefn.PauliY i =>
    [(B.flip b i, if B.get b i then neg_i else pos_i)]

  | GateDefn.PauliZ i =>
    [(b, if B.get b i then neg_1 else pos_1)]

  | GateDefn.Hadamard i =>
    [(B.unset b i, pos_h1), (B.set b i, if B.get b i then neg_h1 else pos_h1)]

  | GateDefn.SqrtX i =>
    if B.get b i then
      [(B.unset b i, C.scale (half, C.+ (pos_1, neg_i))),
       (b,           C.scale (half, C.+ (pos_1, pos_i)))]
    else
      [(b,           C.scale (half, C.+ (pos_1, pos_i))),
       (B.set b i,   C.scale (half, C.+ (pos_1, neg_i)))]

  | GateDefn.Sxdg i =>
    if B.get b i then
      [(B.unset b i, C.scale (half, C.+ (pos_1, pos_i))),
       (b,           C.scale (half, C.+ (pos_1, neg_i)))]
    else
      [(b,           C.scale (half, C.+ (pos_1, neg_i))),
       (B.set b i,   C.scale (half, C.+ (pos_1, pos_i)))]

  | GateDefn.S i =>
    [(b, if B.get b i then pos_i else pos_1)]

  | GateDefn.Sdg i =>
    [(b, if B.get b i then neg_i else pos_1)]

  | GateDefn.X i =>
    [(B.flip b i, pos_1)]

  | GateDefn.T i =>
    [(b, if B.get b i then C.+ (pos_h1, pos_hi) else pos_1)]

  | GateDefn.Tdg i => [(b, if B.get b i then C.+ (pos_h1, neg_hi) else pos_1)]

  | GateDefn.CX {control = i, target = j} =>
    [(if B.get b i then B.flip b j else b, pos_1)]

  | GateDefn.CZ {control = i, target = j} =>
    [(b, if B.get b i andalso B.get b j then neg_1 else pos_1)]

  | GateDefn.CCX {control1 = i, control2 = j, target = k} =>
    [(if B.get b i andalso B.get b j then B.flip b k else b, pos_1)]

  | GateDefn.Phase {target = i, rot = x} =>
    [(b, if B.get b i then C.rotateBy (R.fromLarge x) else pos_1)]

  | GateDefn.CPhase {control = i, target = j, rot = x} =>
    [(b, if B.get b i andalso B.get b j then C.rotateBy (R.fromLarge x) else pos_1)]

  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} =>
    raise Fail "Gate2.pushFromGateDefn FSim unimplemented"

  | GateDefn.RZ {rot = x, target = i} =>
    [(b, C.rotateBy (R.fromLarge (x * (if B.get b i then ~0.5 else 0.5))))]

  | GateDefn.RY {rot = x, target = i} =>
    if B.get b i then
      [(B.unset b i, C.real (R.Math.sin (R.* (half, R.fromLarge x)))),
       (b, C.real (R.Math.cos (R.* (half, R.fromLarge x))))]
    else
      [(b, C.real (R.Math.cos (R.* (half, R.fromLarge x)))),
       (B.set b i, C.real (R.~ (R.Math.sin (R.* (half, R.fromLarge x)))))]

  | GateDefn.RX {rot = x, target = i} =>
    if B.get b i then
      [(B.unset b i, C.imag (R.Math.sin (R.* (half, R.fromLarge x)))),
       (b, C.real (R.Math.cos (R.* (half, R.fromLarge x))))]
    else
      [(b, C.real (R.Math.cos (R.* (half, R.fromLarge x)))),
       (B.set b i, C.imag (R.~ (R.Math.sin (R.* (half, R.fromLarge x)))))]

  | GateDefn.Swap {target1 = i, target2 = j} =>
    [(B.setTo (B.get b i) (B.setTo (B.get b j) b i) j, pos_1)]

  | GateDefn.CSwap {control = i, target1 = j, target2 = k} =>
    [(if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b, pos_1)]

  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} =>
    if B.get b i then
      [(B.unset b i, C.scale (R.Math.sin (R.* (half, R.fromLarge x)), C.rotateBy (R.fromLarge y))),
       (b, C.scale (R.Math.cos (R.* (half, R.fromLarge x)), C.rotateBy (R.fromLarge (y + z))))]
    else
      [(b, C.real (R.Math.cos (R.* (half, R.fromLarge x)))),
       (B.set b i, C.~ (C.scale (R.Math.sin (R.* (half, R.fromLarge x)), C.rotateBy (R.fromLarge z))))]

  | GateDefn.Other {name = n, params = xs, args = is} =>
    raise Fail "Gate2.pushFromGateDefn Other unimplemented"

  fun pushFromGateDefn gd b = case gd of
    GateDefn.PauliY i => [B.flip b i]
  | GateDefn.PauliZ i => [b]
  | GateDefn.Hadamard i => [B.unset b i, B.set b i]
  | GateDefn.SqrtX i => [B.unset b i, B.set b i]
  | GateDefn.Sxdg i => [B.unset b i, B.set b i]
  | GateDefn.S i => [b]
  | GateDefn.Sdg i => [b]
  | GateDefn.X i => [B.flip b i]
  | GateDefn.T i => [b]
  | GateDefn.Tdg i => [b]
  | GateDefn.CX {control = i, target = j} => [if B.get b i then B.flip b j else b]
  | GateDefn.CZ {control = i, target = j} => [b]
  | GateDefn.CCX {control1 = i, control2 = j, target = k} => [if B.get b i andalso B.get b j then B.flip b k else b]
  | GateDefn.Phase {target = i, rot = x} => [b]
  | GateDefn.CPhase {control = i, target = j, rot = x} => [b]
  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} => raise Fail "Gate2.pullFromGateDefn FSim unimplemented"
  | GateDefn.RZ {rot = x, target = i} => [b]
  | GateDefn.RY {rot = x, target = i} => [B.unset b i, B.set b i]
  | GateDefn.RX {rot = x, target = i} => [B.unset b i, B.set b i]
  | GateDefn.Swap {target1 = i, target2 = j} => [B.setTo (B.get b i) (B.setTo (B.get b j) b i) j]
  | GateDefn.CSwap {control = i, target1 = j, target2 = k} => [if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b]
  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} => [B.unset b i, B.set b i]
  | GateDefn.Other {name = n, params = xs, args = is} => raise Fail "Gate2.pullFromGateDefn Other unimplemented"

  fun maxBranchingFactorFromGateDefn gd = case gd of
    GateDefn.PauliY i => 1
  | GateDefn.PauliZ i => 1
  | GateDefn.Hadamard i => 2
  | GateDefn.SqrtX i => 2
  | GateDefn.Sxdg i => 2
  | GateDefn.S i => 1
  | GateDefn.Sdg i => 1
  | GateDefn.X i => 1
  | GateDefn.T i => 1
  | GateDefn.Tdg i => 1
  | GateDefn.CX {control = i, target = j} => 1
  | GateDefn.CZ {control = i, target = j} => 1
  | GateDefn.CCX {control1 = i, control2 = j, target = k} => 1
  | GateDefn.Phase {target = i, rot = x} => 1
  | GateDefn.CPhase {control = i, target = j, rot = x} => 1
  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} => raise Fail "Gate2.pullFromGateDefn FSim unimplemented"
  | GateDefn.RZ {rot = x, target = i} => 1
  | GateDefn.RY {rot = x, target = i} => 2
  | GateDefn.RX {rot = x, target = i} => 2
  | GateDefn.Swap {target1 = i, target2 = j} => 1
  | GateDefn.CSwap {control = i, target1 = j, target2 = k} => 1
  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} => 2
  | GateDefn.Other {name = n, params = xs, args = is} => raise Fail "Gate2.pullFromGateDefn Other unimplemented"

  fun fromGateDefn gd =
    { args = Seq.fromList (GateDefn.getGateArgs gd),
      push = fn b => Seq.fromList (pushFromGateDefn gd b),
      pull = fn b => Seq.fromList (pullFromGateDefn gd b),
      maxBranchingFactor = maxBranchingFactorFromGateDefn gd
    }

  fun maxBranchingFactor (g: gate) = #maxBranchingFactor g

  fun push (g: gate) (b: B.t) = #push g b

  fun pull (g: gate) (b: B.t) = #pull g b
end
