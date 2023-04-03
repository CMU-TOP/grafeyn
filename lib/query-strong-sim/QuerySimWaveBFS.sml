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

    val tryAdvanceWithSpaceConstraint: int
                                       -> Gate.t
                                       -> wave
                                       -> {wave: wave, numGateApps: int} option

    val split: wave -> wave * wave

    val merge: wave * wave -> wave
  end =
  struct
    type wave = (BasisIdx.t, Complex.t) HT.t
    type t = wave

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
        SOME {wave = newWave, numGateApps = numGateApps}
      end
      handle HT.Full => NONE


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

    val removeOldest: waveset -> waveset * Wave.t
  end =
  struct
    structure IntKey = struct open Int type ord_key = int end
    structure M = RedBlackMapFn(IntKey)
    type waveset = Wave.t M.map
    type t = waveset

    fun removeOldest waves =
      raise Fail "QuerySimWaveBFS.WaveSet.removeOldest: not yet implemented"
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

      val initialWave = Wave.singleton (BasisIdx.zeros, Complex.real 1.0)
    in
      raise Fail "QuerySimWaveBFS.query: not yet implemented"
    end
end
