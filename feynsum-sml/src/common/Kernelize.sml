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
    val computeCosts: G.t Seq.t -> (int * int) Seq.t -> int -> (int * int) Seq.t (* TODO: Delete this, just wanted to check types *)
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
                val tuple as (index, cost) = Seq.nth seq i
                val _ = print(Int.toString(i) ^ "(cut=" ^ Int.toString(index) ^ ", cost=" ^ Int.toString(cost) ^ ") ")
            in
                printHelper seq (i+1)
            end

    fun printSeq (seq: (int*int) Seq.t) =
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

    (* Assume costArray is pairs of (index, cost) *)
    fun computeCosts (gates: G.t Seq.t) (costArray: (int * int) Seq.t) (num_gates: int) =
        if num_gates <= Seq.length gates
        then
            let
                (* val costs = Seq.tabulate (fn i => (Seq.nth costArray i) + (cost (Seq.nth gates i))) i *)
                val _ = print("computeCosts, num_gates=" ^ Int.toString(num_gates) ^ "\n")
                val costs = Seq.tabulate (fn j =>
                                             let
                                                 val fused_gate = G.fuse(Seq.subseq gates (j, num_gates-j))
                                                 val tuple as (prior_index, prior_cost) = Seq.nth costArray j
                                             in
                                                 prior_cost + (cost fused_gate)
                                             end
                                         ) num_gates
                val bestIndex = minIndex costs 0
                val _ = print(" cost array: \n  ")
                val _ = printSeq (Seq.tabulate (fn j => (j, Seq.nth costs j)) (Seq.length costs))
                val _ = print(" Min index: " ^ Int.toString(bestIndex) ^ "\n")
                val newCostArray = Seq.append(costArray, (Seq.singleton (bestIndex, (Seq.nth costs bestIndex))))
            in
                computeCosts gates newCostArray (num_gates+1)
            end
        else
            costArray

    fun fuseGates gates costArray =
        let
            fun fuseGatesHelper gateList costArray i =
                let
                    val priorIndex = #1 (Seq.nth costArray i)
                    val _ = print("Fusing gates: " ^ Int.toString(priorIndex) ^ "-" ^ Int.toString(i) ^ "\n")
                    val fused = if i > 0
                        then
                            Seq.append((fuseGatesHelper (Seq.subseq gateList (0, priorIndex)) costArray priorIndex),
                                       Seq.singleton (G.fuse (Seq.subseq gateList (priorIndex, i-priorIndex))))
                        else
                            gateList
                    val _ = print("done\n")
                in
                    fused
                end
        in
            (* TODO: Sanity check kernels *)
            fuseGatesHelper gates costArray (Seq.length gates)
        end

    fun mediocreFusion gates = (* raise Fail "not yet implemented" *)
        let
            (* Create empty cost array *)
            val costArray = computeCosts gates (Seq.singleton((0,0))) 1

            val _ = print("Cost array:\n ")
            val _ = printSeq costArray
            fun loop gates i =
                if i < (Seq.length gates)
                then
                    let
                        (* val qstr = Seq.toString (Int.toString) (G.getGateArgs (Seq.nth gates i)) *)
                        val qstr = String.concatWith ", " (List.map (Int.toString) (G.getGateArgs (Seq.nth gates i)) )
                        val _ = print("Qubits touched: " ^ qstr ^ "\n")
                        val _ = print("Num qubits touched: " ^ Int.toString(G.numUniqueQubitsTouched (Seq.nth gates i)) ^ "\n")
                        fun innerloop defns j =
                            if j < (Seq.length defns)
                            then
                                let
                                    val defn = Seq.nth defns j
                                    val stringified: string = GateDefn.toString defn (fn j => Int.toString(j))
                                    val _ = print(stringified ^ " | ")
                                in
                                    innerloop defns (j+1)
                                end
                            else
                                ()
                        val _ = print("(")
                        val _ = innerloop (#defn (Seq.nth gates i)) 0
                        val _ = print(" maxBranchFactor: " ^ Int.toString(#maxBranchingFactor (Seq.nth gates i)))
                        val _ = print(")\n")
                    in
                        loop gates (i + 1)
                    end
                else
                    ()
            val _ = loop (fuseGates gates costArray) 0
        in
            fuseGates gates costArray
        end
end
