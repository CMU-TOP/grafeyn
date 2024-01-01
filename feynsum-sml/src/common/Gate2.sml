signature GATE2 =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX

  type qubit_idx = int

  type gate = { args: qubit_idx Seq.t,
                push: B.t -> B.t Seq.t,
                pull: B.t -> (B.t * C.t) Seq.t,
                maxBranchingFactor: int,
                numQubits: int }

  type t = gate

  val fromGateDefn: {numQubits: int} -> GateDefn.t -> gate
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
                maxBranchingFactor: int,
                numQubits: int }

  type t = gate

  structure SST = SparseStateTable2 (structure B = B structure C = C)
  fun flattenAndJoin numQubits amps =
      let val len = Seq.reduce op+ 0 (Seq.map Seq.length amps)
          val st = SST.make { capacity = len * 2, numQubits = numQubits }
          fun iter i =
              let val ampi = Seq.nth amps i in
                Seq.tabulate (fn j => SST.insertAndAdd {probes = len} st (Seq.nth ampi j))
                             (Seq.length ampi)
              end
          val _ = Seq.tabulate iter (Seq.length amps)
          val delayedSeq = SST.compact st
      in
        Seq.tabulate (DelayedSeq.nth delayedSeq) (DelayedSeq.length delayedSeq)
      end

  fun fuse (({args = args1, push = push1, pull = pull1, maxBranchingFactor = mbf1, numQubits = nq1 },
             {args = args2, push = push2, pull = pull2, maxBranchingFactor = mbf2, numQubits = nq2 }) : gate * gate) =
      let fun is_arg qi args = Seq.iterate (fn (b, i) => b orelse (i = qi)) false args
          val args3 = Seq.append (args1, Seq.filter (fn a2 => not (is_arg a2 args1)) args2)
          fun push3 b = Seq.flatten (Seq.map push2 (push1 b))
          fun pull3 b = flattenAndJoin nq1
                          (Seq.map
                             (fn bc => let val (b', c') = bc in
                                         Seq.map
                                           (fn bc' => let val (b'', c'') = bc' in
                                                        (b'', C.* (c', c'')) end)
                                           (pull1 b') end) (pull2 b))
          val mbf3 = mbf1 * mbf2
          val nq3 = if nq1 = nq2 then nq1
                    else raise Fail "Cannot fuse gates for different number of qubits"
      in
        { args = args3,
          push = push3,
          pull = pull3,
          maxBranchingFactor = mbf3,
          numQubits = nq3 }
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

  fun superpos ab = C.scale (half, C.+ ab)

  fun control (g: gate) qi =
    if Seq.iterate (fn (b, i) => b orelse (i = qi)) false (#args g) then
      raise Fail "Cannot control a gate with a qubit it already uses"
    else
      { args = Seq.append (Seq.singleton qi, #args g),
        push = fn b => if B.get b qi then #push g b else Seq.singleton b,
        pull = fn b => if B.get b qi then #pull g b else Seq.singleton (b, pos_1),
        maxBranchingFactor = #maxBranchingFactor g,
        numQubits = #numQubits g }

  (* Given a basis idx b, return a list of tuples (b', c'), where
   *   b' = basis idx to pull from
   *   c' = scalar to multiply the weight of b' by
   *)
  fun pullFromGateDefn gd = case gd of

    GateDefn.PauliY i =>
    (fn b => [(B.flip b i, if B.get b i then neg_i else pos_i)])

  | GateDefn.PauliZ i =>
    (fn b => [(b, if B.get b i then neg_1 else pos_1)])

  | GateDefn.Hadamard i =>
    (fn b => [(B.unset b i, pos_h1), (B.set b i, if B.get b i then neg_h1 else pos_h1)])

  | GateDefn.SqrtX i =>
    let val pos_1_pos_i = superpos (pos_1, pos_i)
        val pos_1_neg_i = superpos (pos_1, neg_i)
    in
      fn b => [(b, pos_1_pos_i), (B.flip b i, pos_1_neg_i)]
    end

  | GateDefn.Sxdg i =>
    let val pos_1_pos_i = superpos (pos_1, pos_i)
        val pos_1_neg_i = superpos (pos_1, neg_i)
    in
      fn b => [(b, pos_1_neg_i), (B.flip b i, pos_1_pos_i)]
    end

  | GateDefn.S i =>
    (fn b => [(b, if B.get b i then pos_i else pos_1)])

  | GateDefn.Sdg i =>
    (fn b => [(b, if B.get b i then neg_i else pos_1)])

  | GateDefn.X i =>
    (fn b => [(B.flip b i, pos_1)])

  | GateDefn.T i =>
    (fn b => [(b, if B.get b i then C.+ (pos_h1, pos_hi) else pos_1)])

  | GateDefn.Tdg i =>
    (fn b => [(b, if B.get b i then C.+ (pos_h1, neg_hi) else pos_1)])

  | GateDefn.CX {control = i, target = j} =>
    (fn b => [(if B.get b i then B.flip b j else b, pos_1)])

  | GateDefn.CZ {control = i, target = j} =>
    (fn b => [(b, if B.get b i andalso B.get b j then neg_1 else pos_1)])

  | GateDefn.CCX {control1 = i, control2 = j, target = k} =>
    (fn b => [(if B.get b i andalso B.get b j then B.flip b k else b, pos_1)])

  | GateDefn.Phase {target = i, rot = x} =>
    let val rot = C.rotateBy (R.fromLarge x) in
      fn b => [(b, if B.get b i then rot else pos_1)]
    end

  | GateDefn.CPhase {control = i, target = j, rot = x} =>
    let val rot = C.rotateBy (R.fromLarge x) in
      fn b => [(b, if B.get b i andalso B.get b j then rot else pos_1)]
    end

  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} =>
    raise Fail "Gate2.pushFromGateDefn FSim unimplemented"

  | GateDefn.RZ {rot = x, target = i} =>
    let val rot1 = C.rotateBy (R.fromLarge (x * 0.5))
        val rot2 = C.rotateBy (R.fromLarge (x * ~0.5))
    in
      fn b => [(b, if B.get b i then rot2 else rot1)]
    end

  | GateDefn.RY {rot = x, target = i} =>
    let val sin = C.real (R.Math.sin (R.* (half, R.fromLarge x)))
        val cos = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val negsin = C.~ sin
    in
      fn b => [(b, cos), (B.flip b i, if B.get b i then sin else negsin)]
    end

  | GateDefn.RX {rot = x, target = i} =>
    let val isin = C.imag (R.Math.sin (R.* (half, R.fromLarge x)))
        val rcos = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val inegsin = C.~ isin
    in
      fn b => [(b, rcos), (B.flip b i, if B.get b i then isin else inegsin)]
    end

  | GateDefn.Swap {target1 = i, target2 = j} =>
    (fn b => [(B.setTo (B.get b i) (B.setTo (B.get b j) b i) j, pos_1)])

  | GateDefn.CSwap {control = i, target1 = j, target2 = k} =>
    (fn b => [(if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b, pos_1)])

  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} =>
    let val rot1 = C.scale (R.Math.sin (R.* (half, R.fromLarge x)),
                            C.rotateBy (R.fromLarge y))
        val rot2 = C.scale (R.Math.cos (R.* (half, R.fromLarge x)),
                            C.rotateBy (R.fromLarge (y + z)))
        val rot3 = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val rot4 = C.~ (C.scale (R.Math.sin (R.* (half, R.fromLarge x)),
                                 C.rotateBy (R.fromLarge z)))
    in
      fn b => if B.get b i then [(B.unset b i, rot1), (b, rot2)]
              else [(b, rot3), (B.set b i, rot4)]
    end

  | GateDefn.Other {name = n, params = xs, args = is} =>
    raise Fail "Gate2.pushFromGateDefn Other unimplemented"

  fun pushFromGateDefn gd = case gd of
    GateDefn.PauliY i => (fn b => [B.flip b i])
  | GateDefn.PauliZ i => (fn b => [b])
  | GateDefn.Hadamard i => (fn b => [B.unset b i, B.set b i])
  | GateDefn.SqrtX i => (fn b => [B.unset b i, B.set b i])
  | GateDefn.Sxdg i => (fn b => [B.unset b i, B.set b i])
  | GateDefn.S i => (fn b => [b])
  | GateDefn.Sdg i => (fn b => [b])
  | GateDefn.X i => (fn b => [B.flip b i])
  | GateDefn.T i => (fn b => [b])
  | GateDefn.Tdg i => (fn b => [b])
  | GateDefn.CX {control = i, target = j} => (fn b => [if B.get b i then B.flip b j else b])
  | GateDefn.CZ {control = i, target = j} => (fn b => [b])
  | GateDefn.CCX {control1 = i, control2 = j, target = k} => (fn b => [if B.get b i andalso B.get b j then B.flip b k else b])
  | GateDefn.Phase {target = i, rot = x} => (fn b => [b])
  | GateDefn.CPhase {control = i, target = j, rot = x} => (fn b => [b])
  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} => raise Fail "Gate2.pullFromGateDefn FSim unimplemented"
  | GateDefn.RZ {rot = x, target = i} => (fn b => [b])
  | GateDefn.RY {rot = x, target = i} => (fn b => [B.unset b i, B.set b i])
  | GateDefn.RX {rot = x, target = i} => (fn b => [B.unset b i, B.set b i])
  | GateDefn.Swap {target1 = i, target2 = j} => (fn b => [B.setTo (B.get b i) (B.setTo (B.get b j) b i) j])
  | GateDefn.CSwap {control = i, target1 = j, target2 = k} => (fn b => [if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b])
  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} => (fn b => [B.unset b i, B.set b i])
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

  fun fromGateDefn {numQubits = numQubits} gd =
    { args = Seq.fromList (GateDefn.getGateArgs gd),
      push = (Seq.fromList o pushFromGateDefn gd),
      pull = (Seq.fromList o pullFromGateDefn gd),
      maxBranchingFactor = maxBranchingFactorFromGateDefn gd,
      numQubits = numQubits
    }

  fun maxBranchingFactor (g: gate) = #maxBranchingFactor g

  fun push (g: gate) (b: B.t) = #push g b

  fun pull (g: gate) (b: B.t) = #pull g b
end
