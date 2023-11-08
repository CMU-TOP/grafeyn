structure DepGraph:
sig

  type gate_idx = int

  type dep_graph = {
    gates: GateDefn.t Seq.t,
    deps: gate_idx Seq.t Seq.t,
    indegree: int Seq.t,
    numQubits: int
  }

  type t = dep_graph

  val fromJSON: JSON.value -> dep_graph
  val fromString: string -> dep_graph
  val fromFile: string -> dep_graph

  (*val mkGateDefn: (string * real list option * int list) -> GateDefn.t*)

end =
struct

  type gate_idx = int

  type dep_graph = {
    gates: GateDefn.t Seq.t,
    deps: gate_idx Seq.t Seq.t,
    indegree: int Seq.t,
    numQubits: int
  }

  type t = dep_graph

  fun expect opt err = case opt of
      NONE => raise Fail err
    | SOME x => x

  fun mkGateDefn ("x", NONE, [q0]) =
        GateDefn.X q0
    | mkGateDefn ("y", NONE, [q0]) =
        GateDefn.PauliY q0
    | mkGateDefn ("z", NONE, [q0]) =
        GateDefn.PauliZ q0
    | mkGateDefn ("h", NONE, [q0]) =
        GateDefn.Hadamard q0
    | mkGateDefn ("sx", NONE, [q0]) =
        GateDefn.SqrtX q0
    | mkGateDefn ("sxdg", NONE, [q0]) =
        GateDefn.Sxdg q0
    | mkGateDefn ("s", NONE, [q0]) =
        GateDefn.S q0
    | mkGateDefn ("sdg", NONE, [q0]) =
        GateDefn.S q0
    | mkGateDefn ("t", NONE, [q0]) =
        GateDefn.T q0
    | mkGateDefn ("tdg", NONE, [q0]) =
        GateDefn.Tdg q0
    | mkGateDefn ("cx", NONE, [control, target]) =
        GateDefn.CX {control = control, target = target}
    | mkGateDefn ("cz", NONE, [control, target]) =
        GateDefn.CZ {control = control, target = target}
    | mkGateDefn ("ccx", NONE, [control1, control2, target]) =
        GateDefn.CCX {control1 = control1, control2 = control2, target = target}
    | mkGateDefn ("phase", SOME [rot], [target]) =
        GateDefn.Phase {target = target, rot = rot}
    | mkGateDefn ("cp", SOME [rot], [control, target]) =
        GateDefn.CPhase {control = control, target = target, rot = rot}
    | mkGateDefn ("fsim", SOME [theta, phi], [left, right]) =
        GateDefn.FSim {left = left, right = right, theta = theta, phi = phi}
    | mkGateDefn ("rx", SOME [rot], [target]) =
        GateDefn.RX {rot = rot, target = target}
    | mkGateDefn ("ry", SOME [rot], [target]) =
        GateDefn.RY {rot = rot, target = target}
    | mkGateDefn ("rz", SOME [rot], [target]) =
        GateDefn.RZ {rot = rot, target = target}
    | mkGateDefn ("swap", NONE, [target1, target2]) =
        GateDefn.Swap {target1 = target1, target2 = target2}
    | mkGateDefn ("cswap", NONE, [control, target1, target2]) =
        GateDefn.CSwap {control = control, target1 = target1, target2 = target2}
    | mkGateDefn ("u", SOME [theta, phi, lambda], [target]) =
        GateDefn.U {target = target, theta = theta, phi = phi, lambda = lambda}
    | mkGateDefn ("u2", SOME [phi, lambda], [target]) =
        GateDefn.U {target = target, theta = Math.pi/2.0, phi = phi, lambda = lambda}
    | mkGateDefn ("u1", SOME [lambda], [target]) =
        GateDefn.U {target = target, theta = 0.0, phi = 0.0, lambda = lambda}
    | mkGateDefn (name, params, qargs) =
        raise Fail ("Unknown gate-params-qargs combination with name " ^ name)
  (*fun mkGate (g) = G.fromGateDefn (mkGateDefn g)*)

  fun arrayToSeq a = Seq.tabulate (fn i => Array.sub (a, i)) (Array.length a)

  fun getDepsInDeg (edges, N) =
      let val deps = Array.array (N, nil);
          val indeg = Array.array (N, 0);
          fun incDeg (i) = Array.update (indeg, i, 1 + Array.sub (indeg, i))
          fun go nil = ()
            | go (JSON.ARRAY [JSON.INT fm, JSON.INT to] :: edges) =
                let val fm64 = IntInf.toInt fm
                    val to64 = IntInf.toInt to
                    val () = incDeg to64
                    val () = Array.update (deps, fm64, (to64 :: Array.sub (deps, fm64)))
                in
                  go edges
                end
            | go (_ :: edges) = raise Fail "Malformed edge in JSON"
          val () = go edges;
      in
        (Seq.map Seq.fromList (arrayToSeq deps), arrayToSeq indeg)
      end

  fun fromJSON (data) =
    let fun to_gate g =
          let val name = JSONUtil.asString (expect (JSONUtil.findField g "name") "Expected field 'name' in JSON");
              val params = Option.map (JSONUtil.arrayMap JSONUtil.asNumber) (JSONUtil.findField g "params");
              val qargs = JSONUtil.arrayMap JSONUtil.asInt (expect (JSONUtil.findField g "qargs") "Expected field 'qargs' in JSON");
          in
            mkGateDefn (name, params, qargs)
          end
        val numqs = case JSONUtil.findField data "qubits" of
                        SOME (JSON.INT qs) => IntInf.toInt qs
                      | _ => raise Fail "Expected integer field 'qubits' in JSON"
        val gates = case JSONUtil.findField data "nodes" of
                        SOME (JSON.ARRAY ns) => Seq.fromList (List.map to_gate ns)
                      | _ => raise Fail "Expected array field 'nodes' in JSON"
        val edges = case JSONUtil.findField data "edges" of
                        SOME (JSON.ARRAY es) => es
                      | _ => raise Fail "Expected array field 'nodes' in JSON"
        val (deps, indegree) = getDepsInDeg (edges, Seq.length gates)
    in
      { gates = gates, deps = deps, indegree = indegree, numQubits = numqs }
      (*Seq.zipWith (fn (g, (d, i)) => {gate = g, deps = d, indegree = i}) (gates, Seq.zip (deps, indegree))*)
    end
  fun fromString (str) = fromJSON (JSONParser.parse (JSONParser.openString str))
  fun fromFile (file) = fromJSON (JSONParser.parseFile file)
end
