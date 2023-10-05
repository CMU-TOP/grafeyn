structure QuerySimSequential:
sig
  (* return the amplitude for a single point *)
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  fun query {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      fun loop (gateAppCount, outputAcc) i widx =
        if i >= depth then
          let
            val (bidx, weight) = widx
          in
            if BasisIdx.equal (bidx, desired) then
              (gateAppCount, Complex.+ (outputAcc, weight))
            else
              (gateAppCount, outputAcc)
          end
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' =>
              loop (gateAppCount + 1, outputAcc) (i + 1) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              let val xx = loop (gateAppCount + 1, outputAcc) (i + 1) widx1
              in loop xx (i + 1) widx2
              end

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, result) = loop (0, Complex.zero) 0 initial
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      result
    end

end
