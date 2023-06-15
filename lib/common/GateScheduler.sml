structure GateScheduler:
sig
  type sched
  type t = sched

  type qubit_idx = int
  type gate_idx = int

  val new:
    { numQubits: int
    , numGates: int
    , gateTouches: gate_idx -> qubit_idx Seq.t
    , gateIsBranching: gate_idx -> bool
    }
    -> sched

  val tryVisit: sched -> gate_idx -> bool
  val visitMaximalNonBranchingRun: sched -> gate_idx Seq.t
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
  fun tryVisit (S {numQubits, numGates, gateTouches, gateIsBranching, frontier})
    gidx =
    let
      val touches = gateTouches gidx
      val okayToVisit = Util.all (0, Seq.length touches) (fn i =>
        Array.sub (frontier, Seq.nth touches i) = gidx)
    in
      if not okayToVisit then
        false
      else
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


  fun visitMaximalNonBranchingRun
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) =
    let
      (* visit as many non-branching gates on qubit qi as possible *)
      fun loopQubit acc qi =
        let
          val nextg = Array.sub (frontier, qi)
        in
          if
            nextg >= numGates orelse gateIsBranching nextg
            orelse not (tryVisit sched nextg)
          then acc
          else loopQubit (nextg :: acc) qi
        end

      fun loop acc =
        let
          val selection = Util.loop (0, numQubits) [] (fn (acc, qi) =>
            loopQubit acc qi)
        in
          case selection of
            [] => acc
          | _ => loop (selection @ acc)
        end
    in
      Seq.fromRevList (loop [])
    end

end
