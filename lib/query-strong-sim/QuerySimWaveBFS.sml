structure QuerySimWaveBFS:
sig
  (* first arg is space constraint, measured in terms of number of sparse
   * elements.
   * increasing this uses more space and decreases time.
   *)
  val query: int -> Circuit.t -> BasisIdx.t -> Complex.t
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

    val capacity: wave -> int

    val lookup: wave -> BasisIdx.t -> Complex.t option

    datatype advance_result =
      AdvanceSuccess of wave
    | AdvancePartialSuccess of {advanced: wave, leftover: wave}

    val tryAdvanceWithSpaceConstraint:
      int
      -> Gate.t
      -> wave
      -> {numGateApps: int, result: advance_result}

    val merge: wave * wave -> wave
  end =
  struct
    datatype wave =
      Wave of {elems: (BasisIdx.t, Complex.t) HT.t, nonZeroSize: int}

    type t = wave

    datatype advance_result =
      AdvanceSuccess of wave
    | AdvancePartialSuccess of {advanced: wave, leftover: wave}


    fun makeNewElems cap =
      HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = cap
        , maxload = 0.75
        }


    fun singleton (bidx, weight) =
      let
        val elems = makeNewElems 1
      in
        HT.insertIfNotPresent elems (bidx, weight);

        Wave
          { elems = elems
          , nonZeroSize = if Complex.isNonZero weight then 1 else 0
          }
      end


    fun lookup (Wave {elems, ...}) desired = HT.lookup elems desired


    fun nonZeroSize (Wave {nonZeroSize = n, ...}) = n


    fun capacity (Wave {elems, ...}) = HT.capacity elems


    fun computeNonZeroSize elems =
      let
        val data = HT.unsafeViewContents elems
      in
        SeqBasis.reduce 1000 op+ 0 (0, Seq.length data) (fn i =>
          case Seq.nth data i of
            NONE => 0
          | SOME (bidx, weight) => if Complex.isNonZero weight then 1 else 0)
      end


    fun tryAdvanceWithSpaceConstraint constraint gate
      (wave as Wave {elems, ...}) =
      let
        val desiredCapacity =
          let
            val currentNonZeroSize = Wave.nonZeroSize wave
            val multiplier = if Gate.expectBranching gate then 3.0 else 1.5
          in
            Int.min (constraint, Real.ceil
              (multiplier * Real.fromInt currentNonZeroSize))
          end

        val newElems = makeNewElems desiredCapacity

        (* TODO: handle HT.Full (failed insert due to capacity)
         *
         * ideas:
         *   - If an element was successfully fully expanded (all of its
         *     outneighbors were inserted into newElems), then update it
         *     with a weight of zero. This is effectively a tombstone; the
         *     element will be ignored when we revisit this wave.
         *   - If an element wasn't successfully expanded at all (none of its
         *     outneighbors were inserted into newElems), then leave the element
         *     alone. We will try again when we revisit this wave.
         *   - But if an element was PARTIALLY expanded (branching gate where
         *     at least one outneighbor succeeded and also at least one neighbor
         *     failed)... what do we do???
         *       - Keep a separate list around of partially expanded elements
         *         for this wave?
         *       - I.e., we tombstone the original element, and then insert it
         *         into the auxiliary "partially expanded list".
         *       - When we revisit this wave, we will handle the partially
         *         exanded list specially (to continue where we left off).
         *       - THIS IS IMPORTANT FOR CORRECTNESS. OTHERWISE WE MIGHT VISIT
         *         THE SAME PATH MORE THAN ONCE, WHICH WILL MAKE THE RESULT
         *         INCORRECT.
         *)

        fun doGate widx =
          case Gate.apply gate widx of
            Gate.OutputOne widx' => HT.insertWith Complex.+ newElems widx'
          | Gate.OutputTwo (widx1, widx2) =>
              ( HT.insertWith Complex.+ newElems widx1
              ; HT.insertWith Complex.+ newElems widx2
              )

        val numGateApps =
          let
            val currentElems = HT.unsafeViewContents elems
          in
            SeqBasis.reduce 100 op+ 0 (0, Seq.length currentElems) (fn i =>
              case Seq.nth currentElems i of
                NONE => 0
              | SOME (bidx, weight) =>
                  if Complex.isNonZero weight then (doGate (bidx, weight); 1)
                  else 0)
          end

        val newWave =
          Wave {elems = newElems, nonZeroSize = computeNonZeroSize newElems}
      in
        {numGateApps = numGateApps, result = AdvanceSuccess newWave}
      end


    fun applyToElems (Wave {elems, ...}) f =
      let
        val contents = HT.unsafeViewContents elems
      in
        ForkJoin.parfor 1000 (0, Seq.length contents) (fn i =>
          case Seq.nth contents i of
            NONE => ()
          | SOME (bidx, weight) => f (bidx, weight))
      end


    fun merge (wave1, wave2) =
      let
        val totalNonZeros = nonZeroSize wave1 + nonZeroSize wave2
        val totalCapacities = capacity wave1 + capacity wave2
        val desiredCapacity = Int.min (totalCapacities, Real.ceil
          (1.5 * Real.fromInt totalNonZeros))
        val newElems = makeNewElems desiredCapacity

        fun insertNonZero (bidx, weight) =
          if Complex.isZero weight then ()
          else HT.insertWith Complex.+ newElems (bidx, weight)
      in
        applyToElems wave1 insertNonZero;
        applyToElems wave2 insertNonZero;
        Wave {elems = newElems, nonZeroSize = computeNonZeroSize newElems}
      end
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

    val totalSize: waveset -> int

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

    fun singleton (gatenum, wave) = M.singleton (gatenum, wave)

    fun numWaves waves = M.numItems waves

    fun totalSize waves =
      M.foldl (fn (wave, acc) => acc + Wave.nonZeroSize wave) 0 waves

    fun insert waves (gatenum, wave) =
      M.insertWith Wave.merge (waves, gatenum, wave)

    fun removeBest waves =
      case M.firsti waves of
        SOME (gatenum, _) =>
          let val (waves', wave) = M.remove (waves, gatenum)
          in (waves', (gatenum, wave))
          end
      | NONE => raise Fail "QuerySimWaveBFS.WaveSet.removeBest: empty"
  end


  (* ========================================================================
   * Main function
   *)


  fun query spaceConstraint {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      fun finishWave acc wave =
        case Wave.lookup wave desired of
          NONE => acc
        | SOME v => Complex.+ (v, acc)


      fun loop totalGateApps acc waves =
        if WaveSet.numWaves waves = 0 then
          (totalGateApps, acc)
        else
          let
            val availableSpace = Int.max
              (1, spaceConstraint - WaveSet.totalSize waves)

            val (waves', (gateNum, chosenWave)) = WaveSet.removeBest waves
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
