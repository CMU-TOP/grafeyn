structure QuerySimBFS:
sig
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  structure SST = SparseStateTable
  structure DS = DelayedSeq
  structure BFS = FullSimBFS(SparseStateTable)


  val queryBfsMaxBranchingStride =
    CommandLineArgs.parseInt "query-bfs-max-branching-stride" 1
  val _ = print
    ("query-bfs-max-branching-stride " ^ Int.toString queryBfsMaxBranchingStride
     ^ "\n")


  fun findNextGoal gates gatenum =
    let
      fun loop (i, branching) =
        if i >= Seq.length gates then
          (i, branching)
        else if Gate.expectBranching (Seq.nth gates i) then
          if branching >= queryBfsMaxBranchingStride then (i, branching)
          else loop (i + 1, branching + 1)
        else
          loop (i + 1, branching)
    in
      loop (gatenum, 0)
    end


  fun query circuit desired =
    let
      val finalState = BFS.run circuit

      val output =
        SeqBasis.reduce 10000 Complex.+ Complex.zero
          (0, DelayedSeq.length finalState)
          (fn i =>
             case DelayedSeq.nth finalState i of
               NONE => Complex.zero
             | SOME (bidx, weight) =>
                 if BasisIdx.equal (bidx, desired) then weight else Complex.zero)
    in
      output
    end
end
