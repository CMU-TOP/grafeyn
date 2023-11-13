functor DepGraphSchedulerGreedyFinishQubit
  (val maxBranchingStride: int val disableFusion: bool):
sig
  val scheduler: DepGraphScheduler.t
  val scheduler2: DepGraphScheduler.t
  val scheduler3: DepGraphScheduler.t
  val scheduler4: DepGraphScheduler.t
  val scheduler5: DepGraphScheduler.t
  val schedulerRandom: int -> DepGraphScheduler.t
end =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  fun DFS ((new, old) : (int list * int list)) = new @ old
  fun BFS ((new, old) : (int list * int list)) = old @ new

  fun revTopologicalSort (dg: DepGraph.t) (push: (int list * int list) -> int list) =
      let val N = Seq.length (#gates dg)
          val ind = Array.tabulate (N, Seq.nth (#indegree dg))
          fun decInd i = let val d = Array.sub (ind, i) in Array.update (ind, i, d - 1); d - 1 end
          val queue = ref nil
          (*val push = case tr of BFS => (fn xs => queue := (!queue) @ xs) | DFS => (fn xs => queue := xs @ (!queue))*)
          fun pop () = case !queue of nil => NONE | x :: xs => (queue := xs; SOME x)
          val _ = queue := push (List.filter (fn i => Seq.nth (#indegree dg) i = 0) (List.tabulate (N, fn i => i)), !queue)
          fun loop L =
              case pop () of
                  NONE => L
                | SOME n =>
                  (let val ndeps = Seq.nth (#deps dg) n in
                     queue := push (List.filter (fn m => decInd m = 0) (List.tabulate (Seq.length ndeps, Seq.nth ndeps)), !queue)
                   end;
                   loop (n :: L))
      in
        loop nil
      end

  fun topologicalSort (dg: DepGraph.t) (push: (int list * int list) -> int list) =
      List.rev (revTopologicalSort dg push)

  val gateDepths: int array option ref = ref NONE

  fun computeGateDepths (dg: DepGraph.t) =
      let val N = Seq.length (#gates dg)
          val depths = Array.array (N, ~1)
          fun gdep i =
              Array.update (depths, i, 1 + Seq.reduce Int.max ~1 (Seq.map (fn j => Array.sub (depths, j)) (Seq.nth (#deps dg) i)))
              (*case Array.sub (depths, i) of
                                ~1 => 1 + Seq.reduce Int.min ~1 (Seq.map gateDepth (Seq.nth (#deps dg) i))
                              | d => d*)
      in
        List.foldl (fn (i, ()) => gdep i) () (revTopologicalSort dg DFS); depths
      end

  fun sortList (lt: 'a * 'a -> bool) (xs: 'a list) =
      let fun insert (x, xs) =
              case xs of
                  nil => x :: nil
                | x' :: xs => if lt (x, x') then x :: x' :: xs else x' :: insert (x, xs)
      in
        List.foldr insert nil xs
      end

  (* Choose in reverse topological order, sorted by easiest qubit to finish *)
  fun scheduler3 ({depGraph = dg, gateIsBranching = gib}: args) =
      let val dgt = DepGraphUtil.transpose dg
          val depths = computeGateDepths dg
          fun lt (a, b) = Array.sub (depths, a) < Array.sub (depths, b) orelse (Array.sub (depths, a) = Array.sub (depths, b) andalso not (gib a) andalso gib b)
          fun push (new, old) = DFS (sortList lt new, old)
          val xs = revTopologicalSort dgt push
          val N = Seq.length (#gates dg)
          val ord = Array.array (N, ~1)
          fun writeOrd i xs = case xs of nil => () | x :: xs' => (Array.update (ord, x, i); writeOrd (i + 1) xs')
          val _ = writeOrd 0 xs
          fun pickEarliestOrd best_idx best_ord i gates =
              if i = Seq.length gates then
                best_idx
              else let val cur_idx = Seq.nth gates i
                       val cur_ord = Array.sub (ord, cur_idx)
                   in
                     if cur_ord < best_ord then pickEarliestOrd cur_idx cur_ord (i + 1) gates else pickEarliestOrd best_idx best_ord (i + 1) gates
                   end
      in
        fn gates => let val g0 = Seq.nth gates 0 in
                      pickEarliestOrd g0 (Array.sub (ord, g0)) 1 gates
                   end
      end

  fun gateDepth i dg =
      case !gateDepths of
          NONE => let val gd = computeGateDepths dg in
                    print "recompouting gate depths";
                    gateDepths := SOME gd;
                    Array.sub (gd, i)
                  end
        | SOME gd => Array.sub (gd, i)

  fun pickLeastDepth best_idx best_depth i gates dg =
      if i = Seq.length gates then
        best_idx
      else
        let val cur_idx = Seq.nth gates i
            val cur_depth = gateDepth cur_idx dg in
          if cur_depth < best_depth then
            pickLeastDepth cur_idx cur_depth (i + 1) gates dg
          else
            pickLeastDepth best_idx best_depth (i + 1) gates dg
        end

  fun pickGreatestDepth best_idx best_depth i gates dg =
      if i = Seq.length gates then
        best_idx
      else
        let val cur_idx = Seq.nth gates i
            val cur_depth = gateDepth cur_idx dg in
          if cur_depth > best_depth then
            pickGreatestDepth cur_idx cur_depth (i + 1) gates dg
          else
            pickGreatestDepth best_idx best_depth (i + 1) gates dg
        end

  (* From a frontier, select which gate to apply next *)
  fun scheduler ({depGraph = dg, ...} : args) =
      (gateDepths := SOME (computeGateDepths dg);
       fn gates => let val g0 = Seq.nth gates 0 in
                     pickLeastDepth g0 (gateDepth g0 dg) 1 gates dg
                   end)

  (* Select gate with greatest number of descendants *)
  fun scheduler4 ({depGraph = dg, ...} : args) =
      (gateDepths := SOME (computeGateDepths dg);
       fn gates => let val g0 = Seq.nth gates 0 in
                     pickGreatestDepth g0 (gateDepth g0 dg) 1 gates dg
                   end)

  structure G = Gate (structure B = BasisIdxUnlimited
                      structure C = Complex64)

  (* Hybrid of scheduler2 (avoid branching on unbranched qubits) and also scheduler3 (choose in reverse topological order, sorted by easiest qubit to finish) *)
  fun scheduler5 ({depGraph = dg, gateIsBranching = gib} : args) =
      let val gates = Seq.map G.fromGateDefn (#gates dg)
          fun touches i = #touches (Seq.nth gates i)
          fun branches i = case #action (Seq.nth gates i) of G.NonBranching _ => 0 | G.MaybeBranching _ => 1 | G.Branching _ => 2

          val dgt = DepGraphUtil.transpose dg
          val depths = computeGateDepths dg
          fun lt (a, b) = Array.sub (depths, a) < Array.sub (depths, b) orelse (Array.sub (depths, a) = Array.sub (depths, b) andalso branches a < branches b)
          fun push (new, old) = DFS (sortList lt new, old)
          val xs = revTopologicalSort dgt push
          val N = Seq.length (#gates dg)
          val ord = Array.array (N, ~1)
          fun writeOrd i xs = case xs of nil => () | x :: xs' => (Array.update (ord, x, i); writeOrd (i + 1) xs')
          val _ = writeOrd 0 xs
          val touched = Array.array (#numQubits dg, false)
          fun touch i = Array.update (touched, i, true)
          fun touchAll gidx = let val ts = touches gidx in List.tabulate (Seq.length ts, fn i => touch (Seq.nth ts i)); () end
          fun newTouches i =
              Seq.length (Seq.filter (fn j => not (Array.sub (touched, j))) (touches i))
          fun pickLeastNewTouches best_idx best_newTouches best_ord i gates =
              if i = Seq.length gates then
                ((* print ("Picked " ^ Int.toString best_idx ^ ", new touches " ^ Int.toString best_newTouches ^ "\n"); *)
                 best_idx)
              else
                let val cur_idx = Seq.nth gates i
                    val cur_newTouches = newTouches cur_idx
                    val cur_ord = Array.sub (ord, cur_idx)
                in
                  if cur_newTouches < best_newTouches
                     orelse (cur_newTouches = best_newTouches
                             andalso cur_ord < best_ord) then
                    pickLeastNewTouches cur_idx cur_newTouches cur_ord (i + 1) gates
                  else
                    pickLeastNewTouches best_idx best_newTouches best_ord (i + 1) gates
                end
      in
        fn gates => let val g0 = Seq.nth gates 0
                        val next = pickLeastNewTouches g0 (newTouches g0) (Array.sub (ord, g0)) 1 gates
                    in
                      touchAll next; next
                    end
      end

  (* Avoids branching on unbranched qubits *)
  fun scheduler2 ({depGraph = dg, gateIsBranching = gib} : args) =
      let val touched = Array.array (#numQubits dg, false)
          val gates = Seq.map G.fromGateDefn (#gates dg)
          fun touches i = #touches (Seq.nth gates i)
          fun branches i = case #action (Seq.nth gates i) of G.NonBranching _ => 0 | G.MaybeBranching _ => 1 | G.Branching _ => 2
          fun touch i = Array.update (touched, i, true)
          fun touchAll gidx = let val ts = touches gidx in List.tabulate (Seq.length ts, fn i => touch (Seq.nth ts i)); () end
          fun newTouches i =
              Seq.length (Seq.filter (fn j => not (Array.sub (touched, j))) (touches i))
          fun pickLeastNewTouches best_idx best_newTouches i gates =
              if i = Seq.length gates then
                ((* print ("Picked " ^ Int.toString best_idx ^ ", new touches " ^ Int.toString best_newTouches ^ "\n"); *)
                 best_idx)
              else
                let val cur_idx = Seq.nth gates i
                    val cur_newTouches = newTouches cur_idx
                in
                  if cur_newTouches < best_newTouches
                     orelse (cur_newTouches = best_newTouches
                             andalso branches cur_idx < branches best_idx) then
                    pickLeastNewTouches cur_idx cur_newTouches (i + 1) gates
                  else
                    pickLeastNewTouches best_idx best_newTouches (i + 1) gates
                end
      in
        fn gates => let val g0 = Seq.nth gates 0
                        val next = pickLeastNewTouches g0 (newTouches g0) 1 gates
                    in
                      touchAll next; next
                    end
      end

  val seed = Random.rand (50, 14125)

  fun schedulerRandom seedNum ({depGraph = dg, gateIsBranching = gib} : args) =
      (*let val seed = Random.rand (seedNum, seedNum * seedNum) in*)
        fn gates => let val r = Random.randRange (0, Seq.length gates - 1) seed in
                      (print ("Randomly chose " ^ Int.toString r ^ " from range [0, " ^ Int.toString (Seq.length gates) ^ ")\n");
                        Seq.nth gates r)
                    end
      (* end *)
end
