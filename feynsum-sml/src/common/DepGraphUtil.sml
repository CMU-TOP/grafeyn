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
end
