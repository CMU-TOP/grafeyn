structure DepGraphUtil :>
sig

  type gate_idx = int

  (* Traversal Automaton State *)
  type state = { visited: bool array, indegree: int array }

  type dep_graph = DepGraph.t

  val visit: dep_graph -> gate_idx -> state -> unit
  val frontier: state -> gate_idx Seq.t
  val initState: dep_graph -> state

  (* Switches edge directions *)
  val transpose: dep_graph -> dep_graph

  val scheduleWithOracle: dep_graph -> (gate_idx -> bool) -> (gate_idx Seq.t -> gate_idx) -> int -> gate_idx Seq.t Seq.t

  val scheduleCost: gate_idx Seq.t Seq.t -> (gate_idx -> bool) -> real
  val chooseSchedule: gate_idx Seq.t Seq.t Seq.t -> (gate_idx -> bool) -> gate_idx Seq.t Seq.t

  (* val gateIsBranching: dep_graph -> (gate_idx -> bool) *)
end =
struct

  type gate_idx = int

  type dep_graph = DepGraph.t

  fun transpose ({gates = gs, deps = ds, indegree = is, numQubits = qs}: dep_graph) =
      let val N = Seq.length gs
          val ds2 = Array.array (N, nil)
          fun apply i = Seq.map (fn j => Array.update (ds2, j, i :: Array.sub (ds2, j))) (Seq.nth ds i)
          val _ = Seq.tabulate apply N
      in
        {gates = gs,
         deps = Seq.tabulate (fn i => Seq.rev (Seq.fromList (Array.sub (ds2, i)))) N,
         indegree = Seq.map Seq.length ds,
         numQubits = qs}
      end

  type state = { visited: bool array, indegree: int array }

  fun visit {gates = _, deps = ds, indegree = _, numQubits = _} i {visited = vis, indegree = deg} =
      (
        (* Set visited[i] = true *)
        Array.update (vis, i, true);
        (* Decrement indegree of each i dependency *)
        Seq.map (fn j => Array.update (deg, j, Array.sub (deg, j) - 1)) (Seq.nth ds i);
        ()
      )

  fun frontier {visited = vis, indegree = deg} =
      let val N = Array.length vis
          fun iter i acc =
              if i < 0 then
                acc
              else
                iter (i - 1) (if (not (Array.sub (vis, i))) andalso
                                 Array.sub (deg, i) = 0
                              then i :: acc else acc)
      in
        Seq.fromList (iter (N - 1) nil)
      end

  fun initState (graph: dep_graph) =
      let val N = Seq.length (#gates graph)
          val vis = Array.array (N, false)
          val deg = Array.tabulate (N, Seq.nth (#indegree graph))
      in
        { visited = vis, indegree = deg }
      end

  fun scheduleWithOracle (graph: dep_graph) (branching: gate_idx -> bool) (choose: gate_idx Seq.t -> gate_idx) (maxBranchingStride: int) =
      let val st = initState graph
          fun findNonBranching (i: int) (xs: gate_idx Seq.t) =
              if i = Seq.length xs then
                NONE
              else if branching (Seq.nth xs i) then
                findNonBranching (i + 1) xs
              else
                SOME (Seq.nth xs i)
          fun loadNonBranching (acc: gate_idx list) =
              case findNonBranching 0 (frontier st) of
                  NONE => acc
                | SOME i => (visit graph i st; loadNonBranching (i :: acc))
          fun loadNext numBranchingSoFar thisStep (acc: gate_idx list list) =
              let val ftr = frontier st in
                if Seq.length ftr = 0 then
                  Seq.map Seq.fromList (Seq.rev (Seq.fromList (if List.null thisStep then acc else List.rev thisStep :: acc)))
                else
                  (let val next = choose ftr in
                     visit graph next st;
                     if numBranchingSoFar + 1 >= maxBranchingStride then
                       loadNext 0 nil (List.rev (loadNonBranching (next :: thisStep)) :: acc)
                     else
                       loadNext (numBranchingSoFar + 1) (loadNonBranching (next :: thisStep)) acc
                   end)
              end
      in
        loadNext 0 (loadNonBranching nil) nil
      end

  (*fun scheduleCost2 (order: gate_idx Seq.t Seq.t) (branching: gate_idx -> bool) =
      let val gates = Seq.flatten order
          val N = Seq.length gates
          fun iter i cost branchedQubits =
              if i = N then
                cost
              else
                iter (i + 1) (1.0 + cost + (if branching (Seq.nth gates i) then cost else 0.0))
      in
        iter 0 0.0 (Vector.tabulate (N, fn _ => false))
      end*)

  
  fun scheduleCost (order: gate_idx Seq.t Seq.t) (branching: gate_idx -> bool) =
      let val gates = Seq.flatten order
          val N = Seq.length gates
          fun iter i cost =
              if i = N then
                cost
              else
                iter (i + 1) (1.0 + (if branching (Seq.nth gates i) then cost * 2.0 else cost))
      in
        iter 0 0.0
      end

  fun chooseSchedule (orders: gate_idx Seq.t Seq.t Seq.t) (branching: gate_idx -> bool) =
      let fun iter i best_i best_cost =
              if i = Seq.length orders then
                Seq.nth orders best_i
              else
                let val cost = scheduleCost (Seq.nth orders i) branching in
                  if cost < best_cost then
                    (print ("Reduced cost from " ^ Real.toString best_cost ^ " to " ^ Real.toString cost ^ "\n"); iter (i + 1) i cost)
                  else
                    (print ("Maintained cost " ^ Real.toString best_cost ^ " over " ^ Real.toString cost ^ "\n"); iter (i + 1) best_i best_cost)
                end
      in
        iter 1 0 (scheduleCost (Seq.nth orders 0) branching)
      end
end
