functor Gate
  (structure B: BASIS_IDX
   structure C: COMPLEX) :>
sig

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

end =
struct
  type qubit_idx = int

  type gate = { args: qubit_idx Seq.t,
                push: B.t -> B.t Seq.t,
                pull: B.t -> (B.t * C.t) Seq.t,
                maxBranchingFactor: int }

  type t = gate

  fun fuse (g1 as {args = args1, push = push1, pull = pull1, maxBranchingFactor = mbf1},
            g2 as {args = args2, push = push2, pull = pull2, maxBranchingFactor = mbf2}) =
      let fun is_arg qi args = Seq.iterate (fn (b, i) => b orelse (i = qi)) false args
          val args3 = Seq.append (args1, Seq.filter (fn a2 => not (is_arg a2 args1)) args2)
          fun push3 b = Seq.flatten (Seq.map push2 (push1 b))
          fun pull3 b = Seq.flatten (Seq.map (fn (b', c') => let (b'', c'') = pull1 b' in (b'', C.* (c', c'')) end) (pull2 b))
          val mbf3 = mbf1 * mbf2
      in
        { args = args3,
          push = push3,
          pull = pull3,
          maxBranchingFactor = mbf3 }
      end

  fun fuses gs = Seq.reduce fuse (Seq.nth gs 0) (Seq.drop gs 1)

  fun control g qi =
    if Seq.iterate (fn (b, i) => b orelse (i = qi)) false (#args g) then
      raise Fail "Cannot control a gate with a qubit it already uses"
    else
      { args = Seq.append (Seq.singleton qi) (#args g),
        push = fn b => if B.get b qi then #push g b else Seq.singleton b,
        pull = fn b => if B.get b qi then #pull g b else Seq.singleton (b, pos_1),
        maxBranchingFactor = #maxBranchingFactor g }

  val pos_1 = one
  val neg_1 = R.~ one
  val pos_i = C.imag pos_1
  val neg_i = C.imag neg_1
  val pos_h1 = RECP_SQRT_2
  val neg_h1 = NEG_RECP_SQRT_2
  val pos_hi = C.imag pos_h1
  val neg_hi = C.imag neg_h1

  (* Given a basis idx b, return a list of tuples (b', c'), where
   *   b' = basis idx to pull from
   *   c' = scalar to multiply the weight of b' by
   *)
  fun pullFromGateDefn gd b = case gd of

    PauliY i =>
    [(B.flip b i, if B.get b i then neg_i else pos_i)]

  | PauliZ i =>
    [(b, if B.get b i then neg_1 else pos_1)]

  | Hadamard i =>
    [(B.unset b i, pos_h), (B.set b i, if B.get b i then neg_h else pos_h)]

  | SqrtX i =>
    if B.get b i then
      [(B.unset b i, C.scale (0.5, C.+ (pos_1, neg_i))),
       (b,           C.scale (0.5, C.+ (pos_1, pos_i)))]
    else
      [(b,           C.scale (0.5, C.+ (pos_1, pos_i))),
       (b.set b i,   C.scale (0.5, C.+ (pos_1, neg_i)))]

  | Sxdg i =>
    if B.get b i then
      [(B.unset b i, C.scale (0.5, C.+ (pos_1, pos_i))),
       (b,           C.scale (0.5, C.+ (pos_1, neg_i)))]
    else
      [(b,           C.scale (0.5, C.+ (pos_1, neg_i))),
       (b.set b i,   C.scale (0.5, C.+ (pos_1, pos_i)))]

  | S i =>
    [(b, if B.get b i then pos_i else pos_1)]

  | Sdg i =>
    [(b, if B.get b i then neg_i else pos_1)]

  | X i =>
    [(B.flip b i, pos_1)]

  | T i =>
    [(b, if B.get b i then C.+ (pos_h1, pos_hi) else pos_1)]

  | Tdg i => [(b, if B.get b i then C.+ (pos_h1, neg_hi) else pos_1)]

  | CX {control = i, target = j} =>
    [(if B.get b i then B.flip b j else b, pos_1)]

  | CZ {control = i, target = j} =>
    [(b, if B.get b i andalso B.get b j then neg_1 else pos_1)]

  | CCX {control1 = i, control2 = j, target = k} =>
    [(if B.get b i andalso B.get b j then B.flip b k else b, pos_1)]

  | Phase {target = i, rot = x} =>
    [(b, if B.get b i then C.rotateBy x else pos_1)]

  | CPhase {control = i, target = j, rot = x} =>
    [(b, if B.get b i andalso B.get b j then C.rotateBy x else pos_1)]

  | Fsim {left = i, right = j, theta = x, phi = y} =>
    raise Fail "Gate2.pushFromGateDefn Fsim unimplemented"

  | RZ {rot = x, target = i} =>
    [(b, C.rotateBy (R.* (x, if B.get b i then ~0.5 else 0.5)))]

  | RY {rot = x, target = i} =>
    if B.get b i then
      [(B.unset b i, R.Math.sin (R.* (0.5, x))),
       (b, R.Math.cos (R.* (0.5, x)))]
    else
      [(b, R.Math.cos (R.* (0.5, x))),
       (B.set b i, R.~ (R.Math.sin (R.* (0.5, x))))]

  | RX {rot = x, target = i} =>
    if B.get b i then
      [(B.unset b i, C.imag (R.Math.sin (R.* (0.5, x)))),
       (b, R.Math.cos (R.* (0.5, x)))]
    else
      [(b, R.Math.cos (R.* (0.5, x))),
       (B.set b i, C.imag (R.~ (R.Math.sin (R.* (0.5, x)))))]

  | Swap {target1 = i, target2 = j} =>
    [(B.setTo (B.get b i) (B.setTo (B.get b j) b i) j, pos_1)]

  | CSwap {control = i, target1 = j, target2 = k} =>
    [(if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b, pos_1)]

  | U {target = i, theta = x, phi = y, lambda = z} =>
    if B.get b i then
      [(B.unset b i, C.scale (R.Math.sin (R.* (0.5, x)), C.rotateBy y)),
       (b, C.scale (R.Math.cos (R.* (0.5, x)), C.rotateBy (R.+ (y, z))))]
    else
      [(b, R.Math.cos (R.* (0.5, x))),
       (B.set b i, C.~ (C.scale (R.Math.sin (R.* (0.5, x)), C.rotateBy z)))]

  | Other {name = n, params = xs, args = is} =>
    raise Fail "Gate2.pushFromGateDefn Other unimplemented"

  fun pushFromGateDefn gd b = case gd of
    PauliY i => [B.flip b i]
  | PauliZ i => [b]
  | Hadamard i => [B.unset b i, B.set b i]
  | SqrtX i => [B.unset b i, B.set b i]
  | Sxdg i => [B.unset b i, B.set b i]
  | S i => [b]
  | Sdg i => [b]
  | X i => [B.flip b i]
  | T i => [b]
  | Tdg i => [b]
  | CX {control = i, target = j} => [if B.get b i then B.flip b j else b]
  | CZ {control = i, target = j} => [b]
  | CCX {control1 = i, control2 = j, target = k} => [if B.get b i andalso B.get b j then B.flip b k else b]
  | Phase {target = i, rot = x} = [b]
  | CPhase {control = i, target = j, rot = x} = [b]
  | Fsim {left = i, right = j, theta = x, phi = y} = raise Fail "Gate2.pullFromGateDefn Fsim unimplemented"
  | RZ {rot = x, target = i} = [b]
  | RY {rot = x, target = i} = [B.unset b i, B.set b i]
  | RX {rot = x, target = i} = [B.unset b i, B.set b i]
  | Swap {target1 = i, target2 = j} = [B.setTo (B.get b i) (B.setTo (B.get b j) b i) j]
  | CSwap {control = i, target1 = j, target2 = k} = [if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b]
  | U {target = i, theta = x, phi = y, lambda = z} = [B.unset b i, B.set b i]
  | Other {name = n, params = xs, args = is} = raise Fail "Gate2.pullFromGateDefn Other unimplemented"

  fun maxBranchingFactorFromGateDefn gd b = case gd of
    PauliY i => 1
  | PauliZ i => 1
  | Hadamard i => 2
  | SqrtX i => 2
  | Sxdg i => 2
  | S i => 1
  | Sdg i => 1
  | X i => 1
  | T i => 1
  | Tdg i => 1
  | CX {control = i, target = j} => 1
  | CZ {control = i, target = j} => 1
  | CCX {control1 = i, control2 = j, target = k} => 1
  | Phase {target = i, rot = x} = 1
  | CPhase {control = i, target = j, rot = x} = 1
  | Fsim {left = i, right = j, theta = x, phi = y} = raise Fail "Gate2.pullFromGateDefn Fsim unimplemented"
  | RZ {rot = x, target = i} = 1
  | RY {rot = x, target = i} = 2
  | RX {rot = x, target = i} = 2
  | Swap {target1 = i, target2 = j} = 1
  | CSwap {control = i, target1 = j, target2 = k} = 1
  | U {target = i, theta = x, phi = y, lambda = z} = 2
  | Other {name = n, params = xs, args = is} = raise Fail "Gate2.pullFromGateDefn Other unimplemented"

  fun fromGateDefn gd =
    { args = GateDefn.getGateArgs gd,
      push = fn b => Seq.fromList (pushFromGateDefn gd b),
      pull = fn b => Seq.fromList (pullFromGateDefn gd b),
      maxBranchingFactor = maxBranchingFactorFromGateDefn gd
    }

  fun maxBranchingFactor g = #maxBranchingFactor g

  fun push g b = #push g b

  fun pull g b = #pull g b
