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
    val initSingleton: BasisIdx.t * Complex.t -> wave

    (* how far has this wave advanced? *)
    val position: wave -> int

    (* how many non-zeros? *)
    val nonZeroSize: wave -> int

    val lookup: wave -> BasisIdx.t -> Complex.t

    val tryAdvanceWithSpaceConstraint:
      int
      -> Gate.t
      -> wave
      -> {numGateApps: int, results: wave Seq.t}

    val merge: wave * wave -> wave
  end =
  struct

    datatype elems =
      Organized of (BasisIdx.t, Complex.t) HT.t
    | Unorganized of (BasisIdx.t * Complex.t) Seq.t

    datatype wave = Wave of {elems: elems, position: int, nonZeroSize: int}

    type t = wave

    fun makeNewElems cap =
      HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = cap
        , maxload = 0.75
        }


    fun initSingleton (bidx, weight) =
      Wave
        { elems = Unorganized (Seq.singleton (bidx, weight))
        , position = 0
        , nonZeroSize = if Complex.isNonZero weight then 1 else 0
        }


    fun lookup (Wave {elems, ...}) desired =
      case elems of
        Organized ht => Option.getOpt (HT.lookup ht desired, Complex.zero)
      | Unorganized s =>
          Seq.reduce Complex.+ Complex.zero
            (Seq.mapOption
               (fn (bidx, weight) =>
                  if BasisIdx.equal (bidx, desired) then SOME weight else NONE)
               s)


    fun nonZeroSize (Wave {nonZeroSize = n, ...}) = n


    fun position (Wave {position = p, ...}) = p


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
      | Unorganized s => (Seq.length s, fn i => SOME (Seq.nth s i))


    fun computeNonZeroSize elems =
      let
        val (n, getElem) = viewElemsContents elems
      in
        SeqBasis.reduce 1000 op+ 0 (0, n) (fn i =>
          case getElem i of
            NONE => 0
          | SOME (_, weight) => if Complex.isNonZero weight then 1 else 0)
      end


    fun makeWaveFromSeq (position, items) =
      let
        val desiredCapacity = Int.max (1, Real.ceil
          (2.0 * Real.fromInt (Seq.length items)))
        val newElems = makeNewElems desiredCapacity
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
          , nonZeroSize = computeNonZeroSize (Organized newElems)
          }
      end


    datatype leftover =
      NextPosition of BasisIdx.t * Complex.t
    | SamePosition of BasisIdx.t * Complex.t


    fun tryAdvanceWithSpaceConstraint constraint gate wave =
      if nonZeroSize wave = 0 then
        {numGateApps = 0, results = Seq.empty ()}
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

          val newElems = makeNewElems desiredCapacity

          fun tryPut widx =
            (HT.insertWith Complex.+ newElems widx; true)
            handle HT.Full => false

          fun doGate widx =
            case Gate.apply gate widx of
              Gate.OutputOne widx' =>
                if tryPut widx' then NONE else SOME (NextPosition widx')
            | Gate.OutputTwo (widx1, widx2) =>
                let
                  val success1 = tryPut widx1
                  val success2 = tryPut widx2
                in
                  if success1 andalso success2 then
                    NONE
                  else if success1 then
                    SOME (NextPosition widx2)
                  else if success2 then
                    SOME (NextPosition widx1)
                  else
                    (* outside of path duplication, this is the only way we
                     * waste a gate application. *)
                    SOME (SamePosition widx)
                end

          val numGateApps = nonZeroSize wave

          (* TODO: could be optimized... *)
          val (leftoverSame, leftoverNext) =
            let
              val Wave {elems, ...} = wave
              val (sz, getElem) = viewElemsContents elems
              val leftovers =
                ArraySlice.full (SeqBasis.tabFilter 100 (0, sz) (fn i =>
                  case getElem i of
                    NONE => NONE
                  | SOME (bidx, weight) =>
                      if Complex.isZero weight then NONE
                      else doGate (bidx, weight)))
              val leftoverSame =
                Seq.mapOption (fn SamePosition widx => SOME widx | _ => NONE)
                  leftovers
              val leftoverNext =
                Seq.mapOption (fn NextPosition widx => SOME widx | _ => NONE)
                  leftovers
            in
              ( makeWaveFromSeq (position wave, leftoverSame)
              , makeWaveFromSeq (1 + position wave, leftoverNext)
              )
            end

          val newWave = Wave
            { elems = Organized newElems
            , position = 1 + position wave
            , nonZeroSize = computeNonZeroSize (Organized newElems)
            }
        in
          { numGateApps = numGateApps
          , results = Seq.fromList [newWave, leftoverSame, leftoverNext]
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
              val newElems = makeNewElems desiredCapacity

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

    val singleton: Wave.t -> waveset

    val numWaves: waveset -> int

    val totalSize: waveset -> int

    val insert: waveset -> Wave.t -> waveset

    (* returns (updated wave set, (gatenum, wave)) *)
    val removeOldest: waveset -> waveset * Wave.t
    val removeNewest: waveset -> waveset * Wave.t
  end =
  struct
    structure IntKey = struct open Int type ord_key = int end
    structure M = RedBlackMapFn(IntKey)
    type waveset = Wave.t M.map
    type t = waveset

    fun singleton wave =
      M.singleton (Wave.position wave, wave)

    fun numWaves waves = M.numItems waves

    fun totalSize waves =
      M.foldl (fn (wave, acc) => acc + Wave.nonZeroSize wave) 0 waves

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
        Complex.+ (acc, Wave.lookup wave desired)

      fun loop totalGateApps acc waves =
        if WaveSet.numWaves waves = 0 then
          (totalGateApps, acc)
        else
          let
            val currentUsage = WaveSet.totalSize waves
            val availableSpace = Int.max (1, spaceConstraint - currentUsage)

            val _ = print ("using " ^ Int.toString currentUsage ^ "\n")
            val _ = print ("available " ^ Int.toString availableSpace ^ "\n")

            val (waves', chosenWave) =
              (*WaveSet.removeOldest waves*)
              if availableSpace = 1 then WaveSet.removeNewest waves
              else WaveSet.removeOldest waves

            val gateNum = Wave.position chosenWave
          in
            if gateNum >= depth then
              loop totalGateApps (finishWave acc chosenWave) waves'
            else
              let
                val {numGateApps, results} =
                  Wave.tryAdvanceWithSpaceConstraint availableSpace
                    (gate gateNum) chosenWave
                val advancedWaves =
                  Seq.iterate
                    (fn (waves, wave) =>
                       if Wave.nonZeroSize wave = 0 then waves
                       else WaveSet.insert waves wave) waves' results
              in
                loop (totalGateApps + numGateApps) acc advancedWaves
              end
          end

      val initialWaves = WaveSet.singleton
        (Wave.initSingleton (BasisIdx.zeros, Complex.real 1.0))
      val (totalGateApps, final) = loop 0 Complex.zero initialWaves
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      final
    end
end
