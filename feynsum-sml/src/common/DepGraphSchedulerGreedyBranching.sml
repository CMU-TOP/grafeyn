structure DepGraphSchedulerGreedyBranching:
sig
  val scheduler: DepGraphScheduler.t
end =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  fun pickBranching i branching gates =
      if i < Seq.length gates then
        (if branching i then
           Seq.nth gates i
         else
           pickBranching (i + 1) branching gates)
      else
        Seq.nth gates 0 (* pick a non-branching gate *)

  (* From a frontier, select which gate to apply next *)
  fun scheduler ({gateIsBranching = gib, ...} : args) gates = pickBranching 0 gib gates
end
