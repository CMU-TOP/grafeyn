structure SimulateSequential:
sig
  val run: Circuit.t -> SparseState.t
end =
struct

  structure HT = HashTable

  fun run {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      fun attemptWithTableSize capacity =
        let
          val table = HT.make
            { hash = BasisIdx.hash
            , eq = BasisIdx.equal
            , capacity = capacity
            , maxload = 0.75
            }

          fun loop (gateAppCount, insertCount) i widx =
            if i >= depth then
              ( HT.insertWith Complex.+ table widx
              ; (gateAppCount, insertCount + 1)
              )
            else
              case Gate.apply (gate i) widx of
                Gate.OutputOne widx' =>
                  loop (gateAppCount + 1, insertCount) (i + 1) widx'
              | Gate.OutputTwo (widx1, widx2) =>
                  let
                    val xx = loop (gateAppCount + 1, insertCount) (i + 1) widx1
                  in
                    loop xx (i + 1) widx2
                  end

          val initial = (BasisIdx.zeros, Complex.real 1.0)
          val (totalGateApps, totalInserts) = loop (0, 0) 0 initial
          val result = SparseState.compactFromTable table
        in
          (result, totalGateApps, totalInserts)
        end
        handle HT.Full => attemptWithTableSize (2 * capacity)

      val (result, totalGateApps, totalInserts) = attemptWithTableSize 100

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
      val _ = print
        ("duplicates and zeros "
         ^ Int.toString (totalInserts - SparseState.size result) ^ "\n")
    in
      result
    end

end
