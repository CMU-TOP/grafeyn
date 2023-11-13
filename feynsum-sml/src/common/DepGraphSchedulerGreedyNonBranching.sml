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
  fun scheduler ({gateIsBranching = branching, ...} : args) ftr =
      pickNonBranching 0 branching ftr
  (*fun scheduler ({gateIsBranching = branching, ...} : args) updateFrontier breakFusion initialFrontier =
      let fun updateAndBreak i = let val ftr = updateFrontier i in breakFusion (); ftr end
          fun pickNonBranching i ftr =
              if i < Seq.length gates then
                (if branching i then
                   pickNonBranching (i + 1) gates
                 else
                   Seq.nth gates i)
              else
                (breakFusion (); Seq.nth gates 0)
          fun sched ftr = if Seq.null ftr then
                            ()
                          else
                            scheduler (updateFrontier (pickNonBranching 0 gates))
      in
        sched initialFrontier
      end*)
end
