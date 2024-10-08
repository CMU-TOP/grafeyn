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
   * Waves: a wave is essentially just a sparse state... 
   *)
  structure Wave:
  sig
    type t
    type wave = t

    (* wave at position 0 *)
    val initSingleton: {numGates: int} -> BasisIdx.t * Complex.t -> wave

    (* how far has this wave advanced? *)
    val position: wave -> int

    val sizePotential: wave -> int

    (* how many non-zeros? *)
    val nonZeroSize: wave -> int

    val lookup: wave -> BasisIdx.t -> Complex.t

    val advanceAndSplit:
      {constraint: int, gate: Gate.t}
      -> wave
      -> {numGateApps: int, result: wave option, leftover: wave Seq.t}

    val merge: wave * wave -> wave
  end =
  struct

    datatype elems =
      Organized of (BasisIdx.t, Complex.t) HT.t
    | Unorganized of (BasisIdx.t * Complex.t) option Seq.t

    datatype wave =
      Wave of {elems: elems, position: int, numGates: int, nonZeroSize: int}

    type t = wave

    fun makeNewOrganizedElems cap =
      HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = cap
        , maxload = 0.75
        }


    fun initSingleton {numGates} (bidx, weight) =
      Wave
        { elems = Unorganized (Seq.singleton (SOME (bidx, weight)))
        , position = 0
        , numGates = numGates
        , nonZeroSize = if Complex.isNonZero weight then 1 else 0
        }


    fun lookup (Wave {elems, ...}) desired =
      case elems of
        Organized ht => Option.getOpt (HT.lookup ht desired, Complex.zero)
      | Unorganized s =>
          Seq.reduce Complex.+ Complex.zero
            (Seq.mapOption
               (fn NONE => NONE
                 | SOME (bidx, weight) =>
                  if BasisIdx.equal (bidx, desired) then SOME weight else NONE)
               s)


    fun sizePotential (Wave {numGates, position, nonZeroSize, ...}) =
      (numGates - position) * nonZeroSize


    fun nonZeroSize (Wave {nonZeroSize = n, ...}) = n


    fun position (Wave {position = p, ...}) = p
    fun numGates (Wave {numGates = g, ...}) = g


    fun capacity (Wave {elems, ...}) =
      case elems of
        Organized ht => HT.capacity ht
      | Unorganized s => Seq.length s


    fun viewElemsContents elems =
      case elems of
        Organized ht =>
          let val s = HT.unsafeViewContents ht
          in (Seq.length s, Seq.nth s)
          end
      | Unorganized s => (Seq.length s, Seq.nth s)


    fun computeNonZeroSize elems =
      let
        val (n, getElem) = viewElemsContents elems
      in
        SeqBasis.reduce 1000 op+ 0 (0, n) (fn i =>
          case getElem i of
            NONE => 0
          | SOME (_, weight) => if Complex.isNonZero weight then 1 else 0)
      end


    fun makeOrganizedWaveFromSeq (position, numGates, items) =
      let
        val desiredCapacity = Int.max (1, Real.ceil
          (2.0 * Real.fromInt (Seq.length items)))
        val newElems = makeNewOrganizedElems desiredCapacity
      in
        ForkJoin.parfor 1000 (0, Seq.length items) (fn i =>
          let
            val (bidx, weight) = Seq.nth items i
          in
            if Complex.isZero weight then ()
            else HT.insertWith Complex.+ newElems (bidx, weight)
          end);

        Wave
          { elems = Organized newElems
          , position = position
          , numGates = numGates
          , nonZeroSize = computeNonZeroSize (Organized newElems)
          }
      end


    fun waveDrop n (Wave {elems, position, numGates, nonZeroSize}) =


    datatype leftover =
      NextPosition of BasisIdx.t * Complex.t
    | SamePosition of BasisIdx.t * Complex.t


    fun advanceAndSplit {constraint, gate} wave =
      if nonZeroSize wave = 0 then
        {numGateApps = 0, result = NONE, leftover = Seq.empty ()}
      else
        let
          val desiredCapacity =
            let
              val currentNonZeroSize = nonZeroSize wave
              val multiplier = if Gate.expectBranching gate then 4.0 else 2.0
            in
              Int.min (constraint, Real.ceil
                (multiplier * Real.fromInt currentNonZeroSize))
            end

          val _ = print
            ("tryAdvance: desiredCapacity=" ^ Int.toString desiredCapacity
             ^ "\n")

          val newElems = makeNewOrganizedElems desiredCapacity

          fun tryPut widx =
            (HT.insertWith Complex.+ newElems widx; true)
            handle HT.Full => false

          val markSuccess = ApplyUntilFailure.Success
          val markFailed = ApplyUntilFailure.Failure

          fun doGate widx =
            case Gate.apply gate widx of
              Gate.OutputOne widx' =>
                if tryPut widx' then markSuccess
                else markFailed (NextPosition widx')
            | Gate.OutputTwo (widx1, widx2) =>
                let
                  val success1 = tryPut widx1
                  val success2 = tryPut widx2
                in
                  if success1 andalso success2 then
                    markSuccess
                  else if success1 then
                    markFailed (NextPosition widx2)
                  else if success2 then
                    markFailed (NextPosition widx1)
                  else
                    (* outside of path duplication, this is the only way we
                     * waste a gate application. *)
                    markFailed (SamePosition widx)
                end

          val numGateApps = nonZeroSize wave

          (* TODO: could be optimized... *)
          val leftover =
            let
              val Wave {elems, ...} = wave
              val (sz, getElem) = viewElemsContents elems
              val {numApplied, failed} =
                ApplyUntilFailure.doPrefix {grain = 100, acceleration = 2.0}
                  (0, sz)
                  (fn i =>
                     case getElem i of
                       NONE => markSuccess
                     | SOME (bidx, weight) =>
                         if Complex.isZero weight then markSuccess
                         else doGate (bidx, weight))

              val failedSame =
                Seq.mapOption (fn SamePosition widx => SOME widx | _ => NONE)
                  failed

              val failedNext =
                Seq.mapOption (fn NextPosition widx => SOME widx | _ => NONE)
                  failed

              val leftoverSame = waveDrop numApplied wave

              val failedSame =
                makeOrganizedWaveFromSeq
                  (position wave, numGates wave, failedSame)
              val failedNext = makeOrganizedWaveFromSeq
                (1 + position wave, numGates wave, failedNext)
            in
              Seq.fromList [leftoverSame, failedSame, failedNext]
            end

          val newWave = Wave
            { elems = Organized newElems
            , position = 1 + position wave
            , numGates = numGates wave
            , nonZeroSize = computeNonZeroSize (Organized newElems)
            }
        in
          { numGateApps = numGateApps
          , result = SOME newWave
          , leftover = leftover
          }
        end


    fun applyToElems (Wave {elems, ...}) f =
      let
        val (n, getElem) = viewElemsContents elems
      in
        ForkJoin.parfor 1000 (0, n) (fn i =>
          case getElem i of
            NONE => ()
          | SOME (bidx, weight) => f (bidx, weight))
      end


    fun merge (wave1, wave2) =
      if position wave1 <> position wave2 then
        raise Fail "QuerySimWaveBFS.Wave.merge: different positions"
      else
        let
          val pos = position wave1

          fun loopGuessCapacity desiredCapacity =
            let
              val _ = print
                ("trying desiredCapacity=" ^ Int.toString desiredCapacity ^ "\n")
              val newElems = makeNewOrganizedElems desiredCapacity

              fun insertNonZero (bidx, weight) =
                if Complex.isZero weight then ()
                else HT.insertWith Complex.+ newElems (bidx, weight)
            in
              applyToElems wave1 insertNonZero;
              applyToElems wave2 insertNonZero;
              print "merge success\n";
              Wave
                { elems = Organized newElems
                , position = pos
                , numGates = numGates wave1
                , nonZeroSize = computeNonZeroSize (Organized newElems)
                }
            end
            handle HT.Full =>
              loopGuessCapacity (Real.ceil (1.5 * Real.fromInt desiredCapacity))


          val totalNonZeros = nonZeroSize wave1 + nonZeroSize wave2
          val totalCapacities = capacity wave1 + capacity wave2
          val desiredCapacity = Int.min (totalCapacities, totalNonZeros)
          val desiredCapacity = Real.ceil (1.5 * Real.fromInt desiredCapacity)

          val _ = print
            ("merging totalNonZeros=" ^ Int.toString totalNonZeros
             ^ " totalCapacities=" ^ Int.toString totalCapacities ^ "\n")
        in
          loopGuessCapacity desiredCapacity
        end

  end


  (* ========================================================================
   * Wave set: mapping of gatenum -> wave
   *)
  structure WaveSet:
  sig
    type waveset
    type t = waveset

    val empty: waveset

    val singleton: Wave.t -> waveset

    val numWaves: waveset -> int

    val totalSizePotential: waveset -> int

    val insert: waveset -> Wave.t -> waveset

    (* returns (updated wave set, (gatenum, wave)) *)
    val removeOldest: waveset -> waveset * Wave.t
    val removeNewest: waveset -> waveset * Wave.t

    val remove: waveset -> int -> (waveset * Wave.t) option
  end =
  struct
    structure IntKey = struct open Int type ord_key = int end
    structure M = RedBlackMapFn(IntKey)
    type waveset = Wave.t M.map
    type t = waveset

    val empty = M.empty

    fun singleton wave =
      M.singleton (Wave.position wave, wave)

    fun numWaves waves = M.numItems waves

    fun totalSizePotential waves =
      M.foldl (fn (wave, acc) => acc + Wave.sizePotential wave) 0 waves

    fun insert waves wave =
      M.insertWith Wave.merge (waves, Wave.position wave, wave)

    fun removeOldest waves =
      case M.firsti waves of
        SOME (pos, _) =>
          let val (waves', wave) = M.remove (waves, pos)
          in (waves', wave)
          end
      | NONE => raise Fail "QuerySimWaveBFS.WaveSet.removeOldest: empty"

    fun removeNewest waves =
      let
        (* TODO: why does this signature not have `last`...? *)
        val lastkey = List.hd (List.rev (M.listKeys waves))
        val (waves', wave) = M.remove (waves, lastkey)
      in
        (waves', wave)
      end
      handle _ => raise Fail "QuerySimWaveBFS.WaveSet.removeNewest"

    fun remove waves gatenum =
      if M.inDomain (waves, gatenum) then SOME (M.remove (waves, gatenum))
      else NONE
  end


  (* ========================================================================
   * Main function
   *)


  fun query spaceConstraint {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val numGates = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      fun finishWave acc wave =
        Complex.+ (acc, Wave.lookup wave desired)

      fun pullMerge (waves, currentWave) =
        case WaveSet.remove waves (Wave.position currentWave) of
          NONE => (waves, currentWave)
        | SOME (waves', wave) => (waves', Wave.merge (wave, currentWave))

      fun loop totalGateApps acc (waves, currentWave) =
        if Wave.position currentWave >= numGates then
          let val acc = finishWave acc currentWave
          in loopChooseWave totalGateApps acc waves
          end
        else
          let
            val potentialSpace =
              WaveSet.totalSizePotential waves + Wave.sizePotential currentWave
            val availableSpace = Int.max (1, spaceConstraint - potentialSpace)

            val _ = print ("potential " ^ Int.toString potentialSpace ^ "\n")
            val _ = print ("available " ^ Int.toString availableSpace ^ "\n")

            val gateNum = Wave.position currentWave

            val {numGateApps, result, leftover} =
              Wave.advanceAndSplit
                {constraint = availableSpace, gate = gate gateNum} currentWave
            val totalGateApps = totalGateApps + numGateApps

            val waves =
              Seq.iterate
                (fn (waves, wave) =>
                   if Wave.nonZeroSize wave = 0 then waves
                   else WaveSet.insert waves wave) waves leftover
          in
            case result of
              NONE => loopChooseWave totalGateApps acc waves
            | SOME wave =>
                loopMaybeSwitchWaves totalGateApps acc (pullMerge (waves, wave))
                  potentialSpace
          end

      and loopMaybeSwitchWaves totalGateApps acc (waves, currentWave)
        prevPotential =
        let
          val currentPotential =
            WaveSet.totalSizePotential waves + Wave.sizePotential currentWave
        in
          if prevPotential < currentPotential then
            loop totalGateApps acc (waves, currentWave)
          else
            loopChooseWave totalGateApps acc (WaveSet.insert waves currentWave)
        end

      and loopChooseWave totalGateApps acc waves =
        if WaveSet.numWaves waves = 0 then (totalGateApps, acc)
        else loop totalGateApps acc (WaveSet.removeOldest waves)


      val initialWave =
        Wave.initSingleton {numGates = numGates}
          (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, final) =
        loop 0 Complex.zero (WaveSet.empty, initialWave)
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      final
    end
end
