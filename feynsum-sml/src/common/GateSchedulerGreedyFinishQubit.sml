functor GateSchedulerGreedyFinishQubit
  (val maxBranchingStride: int val disableFusion: bool):
sig
  val scheduler: GateScheduler.t
end =
struct

  type qubit_idx = int
  type gate_idx = int


  datatype sched =
    S of
      { numQubits: int
      , numGates: int
      , gateTouches: gate_idx -> qubit_idx Seq.t
      , gateIsBranching: gate_idx -> bool
      (* each qubit keeps track of which gate is next *)
      , frontier: gate_idx array
      }


  type t = sched


  fun contains x xs =
    Util.exists (0, Seq.length xs) (fn i => Seq.nth xs i = x)


  fun nextTouch (xxx as {numGates, gateTouches}) qubit gidx =
    if gidx >= numGates then numGates
    else if contains qubit (gateTouches gidx) then gidx
    else nextTouch xxx qubit (gidx + 1)


  fun new {numQubits, numGates, gateTouches, gateIsBranching} =
    S { numQubits = numQubits
      , numGates = numGates
      , gateTouches = gateTouches
      , gateIsBranching = gateIsBranching
      , frontier = SeqBasis.tabulate 100 (0, numQubits) (fn i =>
          nextTouch {numGates = numGates, gateTouches = gateTouches} i 0)
      }


  (* It's safe to visit a gate G if, for all qubits the gate touches, the
   * qubit's next gate is G.
   *)
  fun okayToVisit
    (S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) gidx =
    if gidx >= numGates then
      false
    else
      let
        val touches = gateTouches gidx
      in
        Util.all (0, Seq.length touches) (fn i =>
          Array.sub (frontier, Seq.nth touches i) = gidx)
      end


  fun tryVisit
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier})
    gidx =
    if not (okayToVisit sched gidx) then
      false
    else
      let
        (* val _ = print
          ("GateScheduler.tryVisit " ^ Int.toString gidx ^ " numGates "
           ^ Int.toString numGates ^ " frontier "
           ^ Seq.toString Int.toString (ArraySlice.full frontier) ^ "\n") *)
        val touches = gateTouches gidx
      in
        ( Util.for (0, Seq.length touches) (fn i =>
            let
              val qi = Seq.nth touches i
              val next =
                nextTouch {numGates = numGates, gateTouches = gateTouches} qi
                  (gidx + 1)
            in
              Array.update (frontier, qi, next)
            end)

        ; true
        )
      end


  fun peekNext
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) =
    let
      fun makeProgressOnQubit qi =
        let
          val desiredGate = Array.sub (frontier, qi)
        in
          if okayToVisit sched desiredGate then
            desiredGate
          else
            (* Find which qubit is not ready to do this gate yet, and make
             * progress on that qubit (which might recursively need to
             * make progress on a different qubit, etc.)
             *)
            let
              val touches = gateTouches desiredGate
              val dependency =
                FindFirst.findFirstSerial (0, Seq.length touches) (fn i =>
                  let val qj = Seq.nth touches i
                  in Array.sub (frontier, qj) < desiredGate
                  end)
            in
              case dependency of
                SOME i => makeProgressOnQubit (Seq.nth touches i)
              | NONE =>
                  raise Fail
                    "GateSchedulerGreedyFinishQubit.peekNext.makeProgressOnQubit: error"
            end
        end

      val unfinishedQubit = FindFirst.findFirstSerial (0, numQubits) (fn qi =>
        Array.sub (frontier, qi) < numGates)
    in
      case unfinishedQubit of
        NONE => NONE
      | SOME qi => SOME (makeProgressOnQubit qi)
    end


  fun pickNextNoFusion sched =
    case peekNext sched of
      NONE => Seq.empty ()
    | SOME gidx =>
        ( if tryVisit sched gidx then
            ()
          else
            raise Fail
              "GateSchedulerGreedyFinishQubit.pickNextNoFusion: visit failed (should be impossible)"
        ; Seq.singleton gidx
        )


  fun pickNext (sched as S {gateIsBranching, ...}) =
    if disableFusion then
      pickNextNoFusion sched
    else
      let
        fun loop acc numBranchingSoFar =
          if numBranchingSoFar >= maxBranchingStride then
            acc
          else
            case peekNext sched of
              NONE => acc
            | SOME gidx =>
                let
                  val numBranchingSoFar' =
                    numBranchingSoFar + (if gateIsBranching gidx then 1 else 0)
                in
                  if tryVisit sched gidx then
                    ()
                  else
                    raise Fail
                      "GateSchedulerGreedyFinishQubit.pickNext.loop: visit failed (should be impossible)";

                  loop (gidx :: acc) numBranchingSoFar'
                end

        val acc = loop [] 0
      in
        Seq.fromRevList acc
      end


  fun scheduler args =
    let val sched = new args
    in fn () => pickNext sched
    end

end
