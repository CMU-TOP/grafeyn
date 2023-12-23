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

  val pos_1 = one
  val neg_1 = R.~ one
  val pos_i = C.imag pos_1
  val neg_i = C.imag neg_1
  val pos_h = RECP_SQRT_2
  val neg_h = NEG_RECP_SQRT_2

  fun pullFromGateDefn gd b = case gd of
    PauliY i => [(B.flip b i, if B.get b i then neg_i else pos_i)]
  | PauliZ i => [(b, if B.get b i then neg_1 else pos_1)]
  | Hadamard i => [(B.unset b i, pos_h), (B.set b i, if B.get b i then neg_h else pos_h)]
  | SqrtX i => TODO
  | Sxdg i => TODO
  | S i => TODO
  | Sdg i => TODO
  | X i => [(B.flip b i, pos_1)]
  | T i => TODO
  | Tdg i => TODO
  | CX {control = i, target = j} => TODO
  | CZ {control = i, target = j} => TODO
  | CCX {control1 = i, control2 = j, target = k} => TODO
  | Phase {target = i, rot = x} => TODO
  | CPhase {control = i, target = j, rot = x} => TODO
  | Fsim {left = i, right = j, theta = x, phi = y} => TODO
  | RZ {rot = x, target = i} => TODO
  | RY {rot = x, target = i} => TODO
  | RX {rot = x, target = i} => TODO
  | Swap {target1 = i, target2 = j} => TODO
  | CSwap {control = i, target1 = j, target2 = k} => TODO
  | U {target = i, theta = x, phi = y, lambda = z} => TODO
  | Other {name = n, params = xs, args = is} => TODO

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
