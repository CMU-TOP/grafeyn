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

  fun pickNonBranching i branching ftr =
      if i < Seq.length ftr then
        (if branching i then
           pickNonBranching (i + 1) branching ftr
         else
           Seq.nth ftr i)
      else
        Seq.nth ftr 0

  (* From a frontier, select which gate to apply next *)
  fun scheduler dg =
      let val branching = DepGraphUtil.gateIsBranching dg in
        fn gates => pickNonBranching 0 branching gates
      end
end
