structure DepGraphScheduler =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  (* From a frontier, select which gate to apply next *)
  (*       args    visit gates, update frontier    break fusion      initial frontier  gate batches *)
  (*type t = args -> (gate_idx -> gate_idx Seq.t) -> (unit -> unit) -> gate_idx Seq.t -> gate_idx Seq.t Seq.t*)
  type t = args -> (gate_idx Seq.t -> gate_idx)
end
