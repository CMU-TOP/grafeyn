functor DepGraphSchedulerGreedyFinishQubit
  (val maxBranchingStride: int val disableFusion: bool):
sig
  val scheduler: DepGraphScheduler.t
  val ordered: DepGraph.t -> int list
end =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  fun intDo (n : int) (f: int -> 'b) =
      let fun next i = if i = n then () else (f i; next (i + 1))
      in
        next 0
      end

  type OrderedIntSet = {
    S: IntBinarySet.set ref,
    A: int list ref
  }

  fun popOIS ({S = S, A = A}: OrderedIntSet) =
      case !A of
          nil => NONE
        | x :: xs => (A := xs; S := IntBinarySet.delete (!S, x); SOME x)

  fun pushFrontOIS ({S = S, A = A}: OrderedIntSet) (x: int) =
      if IntBinarySet.member (!S, x) then
        ()
      else
        (A := x :: !A; S := IntBinarySet.add (!S, x))

  fun pushBackOIS ({S = S, A = A}: OrderedIntSet) (x: int) =
      if IntBinarySet.member (!S, x) then
        ()
      else
        (A := (!A) @ (x :: nil); S := IntBinarySet.add (!S, x))

  fun newOIS () = {S = ref IntBinarySet.empty, A = ref nil}

  fun emptyOIS ({S = S, A = A}: OrderedIntSet) = List.null (!A)

  datatype TraversalOrder = BFS | DFS

  fun revTopologicalSort (dg: DepGraph.t) (tr: TraversalOrder) =
      let val N = Seq.length (#gates dg)
          val L = ref nil
          val ois = newOIS ()
          (*fun outdegree i = Seq.length (Seq.nth (#deps dg) i)*)
          val ind = Array.tabulate (N, Seq.nth (#indegree dg))
          fun decInd i = let val d = Array.sub (ind, i) in Array.update (ind, i, d - 1); d end
          val push = case tr of BFS => pushBackOIS ois | DFS => pushFrontOIS ois
          val _ = intDo N (fn i => if Seq.nth (#indegree dg) i = 0 then
                                     push i else ())
          fun loop () =
              case popOIS ois of
                  NONE => ()
                | SOME n => 
                  (L := n :: !L;
                   intDo (Seq.length (Seq.nth (#deps dg) n))
                         (fn m => if decInd m = 0 then push m else ());
                   loop ())
      in
        loop (); !L
      end

  fun topologicalSort (dg: DepGraph.t) (tr: TraversalOrder) =
      List.rev (revTopologicalSort dg tr)

  fun ordered (dg: DepGraph.t) =
      topologicalSort (DepGraphUtil.redirect dg) DFS

  val gateDepths: int array option ref = ref NONE

  fun popIntBinarySet (S: IntBinarySet.set ref) = case IntBinarySet.find (fn _ => true) (!S) of
      NONE => raise Fail "popIntBinarySet called on empty set"
    | SOME elt => (S := IntBinarySet.delete (!S, elt); elt)

  fun revTopologicalSortOld (dg: DepGraph.t) =
      let val N = Seq.length (#gates dg)
          val L = ref nil
          val S = ref IntBinarySet.empty
          val A = ref nil
          val ind = Array.tabulate (N, Seq.nth (#indegree dg))
          fun decInd i = let val d = Array.sub (ind, i) in Array.update (ind, i, d - 1); d end
          val _ = intDo N (fn i => if Seq.nth (#indegree dg) i = 0 then
                                     S := IntBinarySet.add (!S, i) else ())
          fun loop () =
              if IntBinarySet.isEmpty (!S) then
                ()
              else
                let val n = popIntBinarySet S in
                  L := n :: !L;
                  intDo (Seq.length (Seq.nth (#deps dg) n))
                        (fn m => if decInd m = 0 then S := IntBinarySet.add (!S, m) else ());
                  loop ()
                end
      in
        loop (); !L
      end

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

  (* From a frontier, select which gate to apply next *)
  fun scheduler ({depGraph = dg, ...} : args) =
      (gateDepths := SOME (computeGateDepths dg);
       fn gates => let val g0 = Seq.nth gates 0 in
                     pickLeastDepth g0 (gateDepth g0 dg) 1 gates dg
                   end)
end
