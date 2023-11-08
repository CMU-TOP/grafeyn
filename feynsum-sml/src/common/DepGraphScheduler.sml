structure DepGraphScheduler =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  (* From a frontier, select which gate to apply next *)
  type t = args -> (gate_idx Seq.t -> gate_idx)
end
