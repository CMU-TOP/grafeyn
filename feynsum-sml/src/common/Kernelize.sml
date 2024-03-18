functor Kernelize
  (structure B: BASIS_IDX
   structure C: COMPLEX):
sig
    structure G: GATE
    structure B: BASIS_IDX
    structure C: COMPLEX
    sharing G.B = B
    sharing G.C = C

    val cost: G.t -> int (* TODO: Delete this, just wanted to check types *)
    val computeCosts: G.t Seq.t -> int Seq.t -> int -> int Seq.t (* TODO: Delete this, just wanted to check types *)
    val mediocreFusion: G.t Seq.t -> G.t Seq.t
end =
struct
    structure B = B
    structure C = C
    structure G = Gate(structure B = B
                       structure C = C)

    fun cost (g: G.t) =
        #maxBranchingFactor g

    fun printHelper seq i =
        if i >= Seq.length seq
        then
            print("\n")
        else
            let
                val _ = print(Int.toString (Seq.nth seq i) ^ " ")
            in
                printHelper seq (i+1)
            end

    fun printSeq (seq: int Seq.t) =
        printHelper seq 0

    fun minIndex (seq: int Seq.t) (start: int) =
        if start >= (Seq.length seq)-1
        then
            start
        else
            let
                val other = minIndex seq (start+1)
            in
                if Seq.nth seq start = Int.min((Seq.nth seq start), (Seq.nth seq other))
                then
                    start
                else
                    other
            end

    fun computeCosts (gates: G.t Seq.t) (costArray: int Seq.t) (i: int) =
        if i < Seq.length gates
        then
            let
                (* val costs = Seq.tabulate (fn i => (Seq.nth costArray i) + (cost (Seq.nth gates i))) i *)
                val _ = print("computeCosts, i=" ^ Int.toString(i) ^ "\n")
                val costs = Seq.tabulate (fn j =>
                                             let
                                                 val fused_gate = G.fuse(Seq.subseq gates (j, i-j+1))
                                             in
                                                 (Seq.nth costArray j) +
                                                 (cost fused_gate)
                                                 (* (cost (Seq.nth gates j)) *)
                                             end
                                         ) i
                val bestIndex = minIndex costs 0
                val _ = print(" cost array: \n  ")
                val _ = printSeq costs
                val _ = print(" Min index: " ^ Int.toString(bestIndex) ^ "\n")
                val newCostArray = Seq.append(costArray, (Seq.singleton (Seq.nth costs bestIndex)))
            in
                computeCosts gates newCostArray (i+1)
            end
        else
            costArray

    fun mediocreFusion gates = (* raise Fail "not yet implemented" *)
        let
            (* Create empty cost array *)
            val costArray = computeCosts gates (Seq.singleton(0)) 1

            val _ = print("Cost array:\n ")
            val _ = printSeq costArray

        in
            gates
        end
end
