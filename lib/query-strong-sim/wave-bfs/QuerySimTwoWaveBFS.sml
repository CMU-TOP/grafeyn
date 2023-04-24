structure QuerySimTwoWaveBFS:
sig
  (* first arg is space constraint, measured in terms of number of sparse
   * elements.
   * increasing this uses more space and decreases time.
   *)
  val query: int -> Circuit.t -> BasisIdx.t -> Complex.t
end =
struct


  type gatenum = int


  datatype find_local_min_result =
    FinishedCircuit
  | FailedDueToSpace
  | FoundLocalMin of gatenum * Wave.t


  datatype trend = IncreasingTrend | DecreasingTrend | UnknownTrend


  datatype find_goal_result =
    OneWave of gatenum * Wave.t
  | TwoWaves of (gatenum * Wave.t) * (gatenum * Wave.t)


  fun query spaceConstraint {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val numGates = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      (* =====================================================================
       * push-to-goal machinery
       * =====================================================================
       *)

      fun pushToGoal acc otherSize goalGate (gatenum, wave) =
        if gatenum >= goalGate then
          (acc, SOME wave)
        else
          let
            val sizeBefore = Wave.nonZeroSize wave
            val acc = SimAccumulator.logSpaceUsage acc (otherSize + sizeBefore)

            val constraint = spaceConstraint - otherSize
            val {numGateApps, result} =
              Wave.tryAdvance
                {constraint = spaceConstraint, gate = gate gatenum} wave
            val acc = SimAccumulator.logGateApps acc numGateApps
          in
            case result of
              NONE => (acc, NONE)
            | SOME wave' =>
                pushToGoal acc otherSize goalGate (gatenum + 1, wave')
          end

      (* =====================================================================
       * local minimum machinery
       *
       * pushToLocalMinimum doesn't need to be called directly
       * instead, use findLocalMinimum
       * =====================================================================
       *)

      fun pushToLocalMinimum acc leftoverSize previousTrend (gatenum, wave) =
        if gatenum >= numGates then
          (SimAccumulator.finishWave acc wave, FinishedCircuit)
        else
          let
            val sizeBefore = Wave.nonZeroSize wave
            val acc =
              SimAccumulator.logSpaceUsage acc (leftoverSize + sizeBefore)
            val constraint = spaceConstraint - leftoverSize
            val {numGateApps, result} =
              Wave.tryAdvance
                {constraint = spaceConstraint, gate = gate gatenum} wave
            val acc = SimAccumulator.logGateApps acc numGateApps
          in
            case result of
              NONE => (acc, FailedDueToSpace)
            | SOME result =>
                case
                  ( previousTrend
                  , Int.compare (Wave.nonZeroSize result, sizeBefore)
                  )
                of
                  (UnknownTrend, GREATER) =>
                    pushToLocalMinimum acc leftoverSize IncreasingTrend
                      (gatenum + 1, result)
                | (UnknownTrend, LESS) =>
                    pushToLocalMinimum acc leftoverSize DecreasingTrend
                      (gatenum + 1, result)
                | (DecreasingTrend, GREATER) =>
                    (acc, FoundLocalMin (gatenum, wave))
                | (IncreasingTrend, LESS) =>
                    pushToLocalMinimum acc leftoverSize DecreasingTrend
                      (gatenum + 1, result)
                | _ =>
                    pushToLocalMinimum acc leftoverSize previousTrend
                      (gatenum + 1, result)
          end

      fun findLocalMinimum acc leftoverSize (gatenum, wave) =
        pushToLocalMinimum acc leftoverSize UnknownTrend (gatenum, wave)

      (* =====================================================================
       * main loops
       * =====================================================================
       *)

      fun loopOneWave acc (gatenum, wave) =
        if gatenum >= numGates then
          SimAccumulator.finishWave acc wave
        else
          let
            val sizeBefore = Wave.nonZeroSize wave
            val acc = SimAccumulator.logSpaceUsage acc sizeBefore

            val {numGateApps, result} =
              Wave.tryAdvance
                {constraint = spaceConstraint, gate = gate gatenum} wave

            val acc = SimAccumulator.logGateApps acc numGateApps
          in
            case result of
              NONE =>
                ( print
                    ("[WAVES] searching for goal from " ^ Int.toString gatenum
                     ^ "\n")
                ; loopFindGoalBySplitting acc (gatenum, wave)
                    (Wave.capacity wave div 2)
                )
            | SOME wave' => loopOneWave acc (gatenum + 1, wave')
          end


      and loopFindGoalBySplitting acc (gatenum, wave) mid =
        let
          val (primary, leftover) = Wave.splitAt mid wave
        in
          case
            findLocalMinimum acc (Wave.nonZeroSize leftover) (gatenum, primary)
          of
            (acc, FailedDueToSpace) =>
              (* TODO: this doesn't necessarily guarantee that we eventually
               * succeed... it's possible that any subset generates the
               * same explosion of size.
               *)
              loopFindGoalBySplitting acc (gatenum, wave) (mid div 2)
          | (acc, FoundLocalMin (goalgate, goalwave)) =>
              ( print
                  ("[WAVES] working on " ^ Int.toString gatenum ^ " -> "
                   ^ Int.toString goalgate ^ "\n")
              ; loopFinishGoalBySplitting acc (goalgate, goalwave)
                  (gatenum, leftover) (Wave.capacity leftover)
              )
          | (acc, FinishedCircuit) =>
              ( print ("[WAVES] single wave " ^ Int.toString gatenum ^ "\n")
              ; loopOneWave acc (gatenum, leftover)
              )
        end


      and loopFinishGoalBySplitting acc (goalgate, goalwave) (gatenum, wave) mid =
        if Wave.nonZeroSize wave = 0 then
          ( print ("[WAVES] single wave " ^ Int.toString goalgate ^ "\n")
          ; loopOneWave acc (goalgate, goalwave)
          )
        else
          let
            val acc = SimAccumulator.logNumWaves acc 2
            val acc = SimAccumulator.logSpaceUsage acc
              (Wave.nonZeroSize goalwave + Wave.nonZeroSize wave)

            val (primary, leftover) = Wave.splitAt mid wave
            val (acc, result) =
              pushToGoal acc
                (Wave.nonZeroSize goalwave + Wave.nonZeroSize leftover) goalgate
                (gatenum, primary)
          in
            case result of
              NONE =>
                loopFinishGoalBySplitting acc (goalgate, goalwave)
                  (gatenum, wave) (mid div 2)
            | SOME wave' =>
                let
                  val goalwave = Wave.merge (goalwave, wave')
                in
                  loopFinishGoalBySplitting acc (goalgate, goalwave)
                    (gatenum, leftover) (Wave.capacity leftover)
                end
          end

      (* =====================================================================
       * entry
       * =====================================================================
       *)

      val initAcc = SimAccumulator.init {desired = desired}
      val initWave = Wave.singleton (BasisIdx.zeros, Complex.real 1.0)

      val _ = print "[WAVES] single wave 0\n"
      val {numGateApps, maxSpaceUsage, maxNumWaves, weight} =
        SimAccumulator.view (loopOneWave initAcc (0, initWave))

      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
      val _ = print ("max space      " ^ Int.toString maxSpaceUsage ^ "\n")
      val _ = print ("max num waves  " ^ Int.toString maxNumWaves ^ "\n")
    in
      weight
    end

end
