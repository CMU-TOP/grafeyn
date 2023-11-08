functor DepGraphSchedulerGreedyNonBranching
  (val maxBranchingStride: int val disableFusion: bool):
sig
  val scheduler: DepGraphScheduler.t
end =
struct

  type gate_idx = int

  type args =
    { depGraph: DepGraph.t
    , gateIsBranching: gate_idx -> bool
    }

  fun pickNonBranching i branching gates =
      if i < Seq.length gates then
        (if branching i then
           pickNonBranching (i + 1) branching gates
         else
           Seq.nth gates i)
      else
        Seq.nth gates 0 (* pick a branching gate *)

  (* From a frontier, select which gate to apply next *)
  fun scheduler ({gateIsBranching = gib, ...} : args) gates = pickNonBranching 0 gib gates
end
