signature GATE =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX

  type qubit_idx = int

  type gate = { push: (B.t -> unit) -> (B.t -> unit),
                (*pull: B.t -> (B.t * C.t) DelayedSeq.t,*)
                pull: (B.t -> C.t) -> (B.t -> C.t),
                maxBranchingFactor: int,
                numQubits: int }

  type t = gate

  val composeSeq: ('a -> 'a) Seq.t -> 'a -> 'a

  val fromGateDefn: {numQubits: int} -> GateDefn.t -> gate
  val maxBranchingFactor: gate -> int
  val numQubits: gate -> int
  val push: gate -> (B.t -> unit) -> (B.t -> unit)
  val pull: gate -> (B.t -> C.t) -> (B.t -> C.t)
  val fuse: gate Seq.t -> gate
  (*val control: gate -> qubit_idx -> gate*)
end


functor Gate
  (structure B: BASIS_IDX
   structure C: COMPLEX): GATE =
struct
  structure B = B
  structure C = C
  
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type qubit_idx = int

  type gate = { push: (B.t -> unit) -> (B.t -> unit),
                pull: (B.t -> C.t) -> (B.t -> C.t),
                maxBranchingFactor: int,
                numQubits: int }

  type t = gate

  (*fun flattenAndJoinNoAmp amps =
      let val len = DelayedSeq.reduce op+ 0 (DelayedSeq.map DelayedSeq.length amps)
          val pos = ref 0
          val arr = Array.array (len, NONE)
          fun touch i b = case Array.sub (arr, i) of
                               NONE => (pos := !pos + 1; Array.update (arr, i, SOME b))
                             | SOME b' => if B.equal (b, b') then () else touch (i + 1) b
          val flattened = DelayedSeq.flatten amps
          val _ = Array.tabulate (len, touch 0 o DelayedSeq.nth flattened)
      in
        DelayedSeq.tabulate (fn i => Option.valOf (Array.sub (arr, i))) (!pos)
      end*)

  fun flattenAndJoin amps =
      if DelayedSeq.length amps <= 1 then
        DelayedSeq.flatten amps
      else
        let val len = DelayedSeq.reduce op+ 0 (DelayedSeq.map DelayedSeq.length amps)
            val pos = ref 0
            val arr = Array.array (len, NONE)
            fun ampIdx b i = case Array.sub (arr, i) of
                                 NONE => (pos := !pos + 1; (!pos - 1, C.zero))
                               | SOME (b', c') => if B.equal (b, b') then (i, c') else ampIdx b (i + 1)
            fun addAmp (b, c) = let val (i, c') = ampIdx b 0 in Array.update (arr, i, SOME (b, C.+ (c, c'))) end
            val flattened = DelayedSeq.flatten amps
            val _ = Array.tabulate (len, addAmp o DelayedSeq.nth flattened)
        in
          DelayedSeq.tabulate (fn i => Option.valOf (Array.sub (arr, i))) (!pos)
        end

  (* composeSeq [f, g, h] = h o (g o f) *)
  fun composeSeq fs =
      let val len = Seq.length fs
          fun iter i f = if i >= len then f else iter (i + 1) (Seq.nth fs i o f) in
        iter 1 (Seq.nth fs 0)
      end

  (* composeSeq' [f, g, h] = (f o g) o h *)
  fun composeSeq' fs =
      let val len = Seq.length fs
          fun iter i f = if i >= len then f else iter (i + 1) (f o Seq.nth fs i) in
        iter 1 (Seq.nth fs 0)
      end

  (*fun fuse' ((g1, g2): gate) =
      let val {numQubits = numQubits1,
               push = push1,
               pull = pull1,
               maxBranchingFactor = mbf1} = g1
          val {numQubits = numQubits2,
               push = push2,
               pull = pull2,
               maxBranchingFactor = mbf2} = g2
          val push = push1 o push2
          val pull = pull2 o pull1
      in
        { push = push,
          pull = pull,
          maxBranchingFactor = mbf1 * mbf2,
          numQubits = numQubits1 }
      end*)

  fun fuse (gs: gate Seq.t) =
      let val numQubits = #numQubits (Seq.nth gs 0)
          val numGates = Seq.length gs
          val _ = Seq.applyIdx gs (fn (_, g) => if #numQubits g = numQubits then () else raise Fail "Cannot fuse gates for circuits with different numbers of qubits")

          (*fun pushIter (f, f') b = DelayedSeq.flatten (DelayedSeq.map f' (f b))
          val push = Seq.iterate (fn (f, g) => pushIter (f, #push g))
                                 DelayedSeq.singleton gs*)
          val push = composeSeq' (Seq.map #push gs)
          val pull = composeSeq (Seq.map #pull gs)

          (*fun pullIter (f, f') b =
              DelayedSeq.flatten
                (DelayedSeq.map (fn (b', c') =>
                             DelayedSeq.map (fn (b'', c'') =>
                                         (b'', C.* (c', c'')))
                                     (f b'))
                         (f' b))
          val pull = Seq.iterate (fn (f, g) => pullIter (f, #pull g))
                                 (fn b => DelayedSeq.singleton (b, C.one)) gs*)

          val maxBranchingFactor = Seq.reduce op* 1 (Seq.map #maxBranchingFactor gs)
      in
        { push = push,
          (*pull = (fn b => flattenAndJoin (DelayedSeq.singleton (pull b))),*)
          pull = pull,
          maxBranchingFactor = maxBranchingFactor,
          numQubits = numQubits }
      end

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

  (*fun control (g: gate) qi =
      { push = fn b => if B.get b qi then #push g b else DelayedSeq.singleton b,
        pull = fn b => if B.get b qi then #pull g b else DelayedSeq.singleton (b, pos_1),
        maxBranchingFactor = #maxBranchingFactor g,
        numQubits = #numQubits g }*)


  fun idty b = b
  fun flip i b = B.flip b i
  fun set0 i b = B.unset b i
  fun set1 i b = B.set b i

  (* Given a basis idx b, return a list of tuples (b', c'), where
   *   b' = basis idx to pull from
   *   c' = scalar to multiply the weight of b' by
   *)
  fun pullFromGateDefn gd = case gd of

    GateDefn.PauliY i =>
    [fn b => (B.flip b i, if B.get b i then neg_i else pos_i)]
  | GateDefn.PauliZ i =>
    [fn b => (b, if B.get b i then neg_1 else pos_1)]

  | GateDefn.Hadamard i =>
    ([fn b => (B.unset b i, pos_h1), fn b => (B.set b i, if B.get b i then neg_h1 else pos_h1)])

  | GateDefn.SqrtX i =>
    let val pos_1_pos_i = superpos (pos_1, pos_i)
        val pos_1_neg_i = superpos (pos_1, neg_i)
    in
      [fn b => (b, pos_1_pos_i), fn b => (B.flip b i, pos_1_neg_i)]
    end

  | GateDefn.Sxdg i =>
    let val pos_1_pos_i = superpos (pos_1, pos_i)
        val pos_1_neg_i = superpos (pos_1, neg_i)
    in
      [fn b => (b, pos_1_neg_i), fn b => (B.flip b i, pos_1_pos_i)]
    end

  | GateDefn.S i =>
    [fn b => (b, if B.get b i then pos_i else pos_1)]

  | GateDefn.Sdg i =>
    [fn b => (b, if B.get b i then neg_i else pos_1)]

  | GateDefn.X i =>
    [fn b => (B.flip b i, pos_1)]

  | GateDefn.T i =>
    [fn b => (b, if B.get b i then C.+ (pos_h1, pos_hi) else pos_1)]

  | GateDefn.Tdg i =>
    [fn b => (b, if B.get b i then C.+ (pos_h1, neg_hi) else pos_1)]

  | GateDefn.CX {control = i, target = j} =>
    [fn b => (if B.get b i then B.flip b j else b, pos_1)]

  | GateDefn.CZ {control = i, target = j} =>
    [fn b => (b, if B.get b i andalso B.get b j then neg_1 else pos_1)]

  | GateDefn.CCX {control1 = i, control2 = j, target = k} =>
    [fn b => (if B.get b i andalso B.get b j then B.flip b k else b, pos_1)]

  | GateDefn.Phase {target = i, rot = x} =>
    let val rot = C.rotateBy (R.fromLarge x) in
      [fn b => (b, if B.get b i then rot else pos_1)]
    end

  | GateDefn.CPhase {control = i, target = j, rot = x} =>
    let val rot = C.rotateBy (R.fromLarge x) in
      [fn b => (b, if B.get b i andalso B.get b j then rot else pos_1)]
    end

  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} =>
    raise Fail "Gate.pullFromGateDefn FSim unimplemented"

  | GateDefn.RZ {rot = x, target = i} =>
    let val rot1 = C.rotateBy (R.fromLarge (x * 0.5))
        val rot2 = C.rotateBy (R.fromLarge (x * ~0.5))
    in
      [fn b => (b, if B.get b i then rot2 else rot1)]
    end

  | GateDefn.RY {rot = x, target = i} =>
    let val sin = C.real (R.Math.sin (R.* (half, R.fromLarge x)))
        val cos = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val negsin = C.~ sin
    in
      [fn b => (b, cos), fn b => (B.flip b i, if B.get b i then sin else negsin)]
    end

  | GateDefn.RX {rot = x, target = i} =>
    let val isin = C.imag (R.Math.sin (R.* (half, R.fromLarge x)))
        val rcos = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val inegsin = C.~ isin
    in
      [fn b => (b, rcos), fn b => (B.flip b i, if B.get b i then isin else inegsin)]
    end

  | GateDefn.Swap {target1 = i, target2 = j} =>
    [fn b => (B.setTo (B.get b i) (B.setTo (B.get b j) b i) j, pos_1)]

  | GateDefn.CSwap {control = i, target1 = j, target2 = k} =>
    [fn b => (if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b, pos_1)]

  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} =>
    let val rot1 = C.scale (R.Math.sin (R.* (half, R.fromLarge x)),
                            C.rotateBy (R.fromLarge y))
        val rot2 = C.scale (R.Math.cos (R.* (half, R.fromLarge x)),
                            C.rotateBy (R.fromLarge (y + z)))
        val rot3 = C.real (R.Math.cos (R.* (half, R.fromLarge x)))
        val rot4 = C.~ (C.scale (R.Math.sin (R.* (half, R.fromLarge x)),
                                 C.rotateBy (R.fromLarge z)))
    in
      [fn b => if B.get b i then (B.unset b i, rot1) else (b, rot3),
       fn b => if B.get b i then (b, rot2) else (B.set b i, rot4)]
    end

  | GateDefn.Other {name = n, params = xs, args = is} =>
    raise Fail "Gate.pullFromGateDefn Other unimplemented"

  fun pushFromGateDefn gd = case gd of
    GateDefn.PauliY i => [flip i]
  | GateDefn.PauliZ i => [idty]
  | GateDefn.Hadamard i => [set0 i, set1 i]
  | GateDefn.SqrtX i => [set0 i, set1 i]
  | GateDefn.Sxdg i => [set0 i, set1 i]
  | GateDefn.S i => [idty]
  | GateDefn.Sdg i => [idty]
  | GateDefn.X i => [flip i]
  | GateDefn.T i => [idty]
  | GateDefn.Tdg i => [idty]
  | GateDefn.CX {control = i, target = j} => [fn b => if B.get b i then flip j b else b]
  | GateDefn.CZ {control = i, target = j} => [idty]
  | GateDefn.CCX {control1 = i, control2 = j, target = k} => [fn b => if B.get b i andalso B.get b j then flip k b else idty b]
  | GateDefn.Phase {target = i, rot = x} => [idty]
  | GateDefn.CPhase {control = i, target = j, rot = x} => [idty]
  | GateDefn.FSim {left = i, right = j, theta = x, phi = y} => raise Fail "Gate.pushFromGateDefn FSim unimplemented"
  | GateDefn.RZ {rot = x, target = i} => [idty]
  | GateDefn.RY {rot = x, target = i} => [set0 i, set1 i]
  | GateDefn.RX {rot = x, target = i} => [set0 i, set1 i]
  | GateDefn.Swap {target1 = i, target2 = j} => [fn b => B.setTo (B.get b i) (B.setTo (B.get b j) b i) j]
  | GateDefn.CSwap {control = i, target1 = j, target2 = k} => [fn b => if B.get b i then B.setTo (B.get b j) (B.setTo (B.get b k) b j) k else b]
  | GateDefn.U {target = i, theta = x, phi = y, lambda = z} => [set0 i, set1 i]
  | GateDefn.Other {name = n, params = xs, args = is} => raise Fail "Gate.pushFromGateDefn Other unimplemented"

  fun pushFromGateDefn' gd =
      case pushFromGateDefn gd of
          [f0] => (fn (set, b) => set (f0 b))
        | [f0, f1] => (fn (set, b) => (set (f0 b); set (f1 b)))
        | [f0, f1, f2, f3] => (fn (set, b) => (set (f0 b); set (f1 b); set (f1 b); set (f2 b)))
        | fs => (fn (set, b) => List.foldr (fn (f, u) => (set (f b); u)) () fs)

  fun pullFromGateDefn' gd =
      case pullFromGateDefn gd of
          [f0] => (fn (get, b) => let val (b', c) = f0 b in C.* (c, get b') end)
        | [f0, f1] => (fn (get, b) => let val (b0, c0) = f0 b
                                          val (b1, c1) = f1 b
                                      in C.+ (C.* (c0, get b0), C.* (c1, get b1)) end)
        | [f0, f1, f2, f3] => (fn (get, b) => let val (b0, c0) = f0 b
                                                  val (b1, c1) = f1 b
                                                  val (b2, c2) = f2 b
                                                  val (b3, c3) = f3 b
                                              in C.+ (C.* (c0, get b0),
                                                      C.+ (C.* (c1, get b1),
                                                          C.+ (C.* (c2, get b2),
                                                               C.* (c3, get b3)))) end)
        | fs => (fn (get, b) => List.foldr (fn (f, a) => let val (b', c) = f b in C.+ (a, C.* (c, get b')) end) C.zero fs)

  fun fromGateDefn {numQubits = numQubits} gd =
    { (*push = (DelayedSeq.fromList o pushFromGateDefn gd),*)
      (*pull = (DelayedSeq.fromList o pullFromGateDefn gd),*)
      (* TODO: might not get inlined properly... fix *)
      push = let val p = pushFromGateDefn' gd in (fn set => fn b => p (set, b)) end,
      (*pull = (fn get => List.foldr (fn ((b, c), a) => C.+ (a, C.* (c, get b))) C.zero o pullFromGateDefn gd),*)
      pull = let val p = pullFromGateDefn' gd in (fn get => fn b => p (get, b)) end,
      maxBranchingFactor = GateDefn.maxBranchingFactor gd,
      numQubits = numQubits
    }

  fun maxBranchingFactor (g: gate) = #maxBranchingFactor g

  fun numQubits (g: gate) = #numQubits g

  fun push (g: gate) = #push g

  fun pull (g: gate) = #pull g
end

(*
bwt_n21 21.428s
multiplier_n45_jatin 1.908s
qugan_n39_transpiled (first 500 kernels) 1m38.664s


*)
