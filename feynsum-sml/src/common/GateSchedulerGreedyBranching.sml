structure GateSchedulerGreedyBranching:
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


  fun visitBranching
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) =
    let
      val possibles =
        Seq.filter
          (fn gi =>
             gi < numGates andalso gateIsBranching gi
             andalso okayToVisit sched gi) (ArraySlice.full frontier)
    in
      if Seq.length possibles = 0 then
        NONE
      else
        let
          val gidx = Seq.nth possibles 0
        in
          if tryVisit sched gidx then ()
          else raise Fail "GateSchedulerGreedyBranching.visitBranching: error";

          SOME gidx
        end
    end


  fun visitNonBranching
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) =
    let
      (* val _ = print
        ("visitNonBranching frontier = "
         ^ Seq.toString Int.toString (ArraySlice.full frontier) ^ "\n") *)
      val possibles =
        Seq.filter
          (fn gi =>
             gi < numGates andalso not (gateIsBranching gi)
             andalso okayToVisit sched gi) (ArraySlice.full frontier)
    in
      if Seq.length possibles = 0 then
        NONE
      else
        let
          val gidx = Seq.nth possibles 0
        in
          (* print
            ("visitNonBranching trying to visit " ^ Int.toString gidx ^ "\n"); *)

          if tryVisit sched gidx then
            ()
          else
            raise Fail "GateSchedulerGreedyBranching.visitNonBranching: error";

          SOME gidx
        end
    end


  fun pickNext sched =
    case visitBranching sched of
      SOME gidx => Seq.singleton gidx
    | NONE =>
        case visitNonBranching sched of
          SOME gidx => Seq.singleton gidx
        | NONE => Seq.empty ()


  fun scheduler args =
    let val sched = new args
    in fn () => pickNext sched
    end

end
