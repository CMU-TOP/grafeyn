structure QuerySimWaveBFS:
sig
  val query: Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  structure HT = HashTable

  (* ========================================================================
   * Waves: a wave is essentially just a sparse state
   *)
  structure Wave:
  sig
    type t
    type wave = t

    val singleton: BasisIdx.t * Complex.t -> wave

    (* how many non-zeros? *)
    val nonZeroSize: wave -> int

    datatype advance_result =
      AdvanceSuccess of wave
    | AdvancePartialSuccess of {advanced: wave, leftover: wave}

    val tryAdvanceWithSpaceConstraint:
      int
      -> Gate.t
      -> wave
      -> {numGateApps: int, result: advance_result}

    val split: wave -> wave * wave

    val merge: wave * wave -> wave
  end =
  struct
    type wave = (BasisIdx.t, Complex.t) HT.t
    type t = wave

    datatype advance_result =
      AdvanceSuccess of wave
    | AdvancePartialSuccess of {advanced: wave, leftover: wave}

    fun makeNewWave cap =
      HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = cap
        , maxload = 0.75
        }


    fun singleton (bidx, weight) =
      let val wave = makeNewWave 1
      in HT.insertIfNotPresent wave (bidx, weight); wave
      end


    fun nonZeroSize wave =
      let
        val currentElems = HT.unsafeViewContents wave
      in
        SeqBasis.reduce 1000 op+ 0 (0, Seq.length currentElems) (fn i =>
          case Seq.nth currentElems i of
            NONE => 0
          | SOME (bidx, weight) => if Complex.isNonZero weight then 1 else 0)
      end


    fun tryAdvanceWithSpaceConstraint capacity gate wave =
      let
        val currentElems = HT.unsafeViewContents wave
        val newWave = makeNewWave capacity

        fun doGate widx =
          case Gate.apply gate widx of
            Gate.OutputOne widx' => HT.insertWith Complex.+ newWave widx'
          | Gate.OutputTwo (widx1, widx2) =>
              ( HT.insertWith Complex.+ newWave widx1
              ; HT.insertWith Complex.+ newWave widx2
              )

        val numGateApps =
          SeqBasis.reduce 100 op+ 0 (0, Seq.length currentElems) (fn i =>
            case Seq.nth currentElems i of
              NONE => 0
            | SOME (bidx, weight) =>
                if Complex.isNonZero weight then (doGate (bidx, weight); 1)
                else 0)
      in
        {numGateApps = numGateApps, result = AdvanceSuccess newWave}
      end


    fun split wave =
      raise Fail "QuerySimWaveBFS.Wave.split: not yet implemented"


    fun merge (wave1, wave2) =
      raise Fail "QuerySimWaveBFS.Wave.merge: not yet implemented"
  end


  (* ========================================================================
   * Wave set: mapping of gatenum -> wave
   *)
  structure WaveSet:
  sig
    type waveset
    type t = waveset

    val singleton: int * Wave.t -> waveset

    val numWaves: waveset -> int

    (* (gatenum, wave) pairs *)
    val insert: waveset -> int * Wave.t -> waveset

    (* returns (updated wave set, (gatenum, wave)) *)
    val removeBest: waveset -> waveset * (int * Wave.t)
  end =
  struct
    structure IntKey = struct open Int type ord_key = int end
    structure M = RedBlackMapFn(IntKey)
    type waveset = Wave.t M.map
    type t = waveset

    fun singleton (gatenum, wave) =
      raise Fail "QuerySimWaveBFS.WaveSet.singleton: not yet implemented"

    fun numWaves waves =
      raise Fail "QuerySimWaveBFS.WaveSet.numWaves: not yet implemented"

    fun insert waves (gatenum, wave) =
      raise Fail "QuerySimWaveBFS.WaveSet.insert: not yet implemented"

    fun removeBest waves =
      raise Fail "QuerySimWaveBFS.WaveSet.removeBest: not yet implemented"
  end


  (* ========================================================================
   * Main function
   *)


  fun query {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      fun finishWave acc wave =
        case HT.lookup wave desired of
          NONE => acc
        | SOME v => Complex.+ (v, acc)


      fun loop totalGateApps acc waves =
        if WaveSet.numWaves waves = 0 then
          (totalGateApps, acc)
        else
          let
            val (waves', (gateNum, chosenWave)) = WaveSet.removeBest waves
            val availableSpace = raise Fail "todo"
          in
            if gateNum >= depth then
              loop totalGateApps (finishWave acc chosenWave) waves'
            else
              let
                val {numGateApps, result} =
                  Wave.tryAdvanceWithSpaceConstraint availableSpace
                    (gate gateNum) chosenWave
                val advancedWaves =
                  case result of
                    Wave.AdvanceSuccess newWave =>
                      WaveSet.insert waves' (gateNum + 1, newWave)
                  | Wave.AdvancePartialSuccess {advanced, leftover} =>
                      WaveSet.insert (WaveSet.insert waves' (gateNum, leftover))
                        (gateNum + 1, advanced)
              in
                loop (totalGateApps + numGateApps) acc advancedWaves
              end
          end

      val initialWaves = WaveSet.singleton
        (0, Wave.singleton (BasisIdx.zeros, Complex.real 1.0))
      val (totalGateApps, final) = loop 0 Complex.zero initialWaves
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      final
    end
end
