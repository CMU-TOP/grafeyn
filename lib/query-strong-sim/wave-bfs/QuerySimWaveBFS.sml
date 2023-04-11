structure QuerySimWaveBFS:
sig
  (* first arg is space constraint, measured in terms of number of sparse
   * elements.
   * increasing this uses more space and decreases time.
   *)
  val query: int -> Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  fun query spaceConstraint {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val numGates = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      (* Ensures that the potential increases by at most `availableSpace` *)
      fun constraint availableSpace gatenum =
        Real.ceil
          (Real.fromInt availableSpace
           / (Real.fromInt (numGates - gatenum) / 2.0 - 1.0))

      fun loopPushWave acc leftovers (gatenum, wave) =
        if gatenum >= numGates then
          (SimAccumulator.finishWave acc wave, leftovers)
        else
          let
            val sizeBefore = Wave.nonZeroSize wave

            val potential =
              (numGates - gatenum) * Wave.nonZeroSize wave
              + WaveSet.totalSizePotential {numGates = numGates} leftovers

            val available = spaceConstraint - potential
            val C = constraint available gatenum

            val _ = print
              ("num waves  " ^ Int.toString (1 + WaveSet.numWaves leftovers)
               ^ "\n")
            val _ = print ("potential  " ^ Int.toString potential ^ "\n")
            val _ = print ("available  " ^ Int.toString available ^ "\n")
            val _ = print ("constraint " ^ Int.toString C ^ "\n")

            val {numGateApps, result, leftover} =
              Wave.advanceAndSplit {constraint = C, gate = gate gatenum} wave
            val leftovers = WaveSet.insert leftovers (gatenum, leftover)
            val acc = SimAccumulator.logGateApps acc numGateApps
            val (leftovers, result) =
              WaveSet.pullMerge leftovers (gatenum + 1, result)

            val sizeAfter = Wave.nonZeroSize result
          in
            if sizeAfter <= sizeBefore then
              loopPushWave acc leftovers (gatenum + 1, result)
            else
              (acc, WaveSet.insert leftovers (gatenum + 1, result))
          end


      fun loop acc waves =
        if WaveSet.numWaves waves = 0 then
          acc
        else
          let
            val (leftovers, gatenum, wave) = WaveSet.removeOldest waves
            val (acc, waves) = loopPushWave acc leftovers (gatenum, wave)
          in
            loop acc waves
          end

      val initAcc = SimAccumulator.init {desired = desired}
      val initWaves = WaveSet.singleton
        (0, Wave.singleton (BasisIdx.zeros, Complex.real 1.0))

      val {numGateApps, weight} = SimAccumulator.view (loop initAcc initWaves)

      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
    in
      weight
    end

end
