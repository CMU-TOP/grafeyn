functor GateSchedulerSampling
  (val maxBranchingStride: int val disableFusion: bool):
sig
  type sched
  type t = sched

  type qubit_idx = int
  type gate_idx = int

  val new: GateScheduler.args -> sched

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

  fun new {numQubits, numGates, gateTouches, gateIsBranching} =
    S { numQubits = numQubits
      , numGates = numGates
      , gateTouches = gateTouches
      , gateIsBranching = gateIsBranching
      , frontier = SeqBasis.tabulate 100 (0, numQubits) (fn i =>
          nextTouch {numGates = numGates, gateTouches = gateTouches} i 0)
      }

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

  fun readyGates
    (sched as S {numQubits, numGates, gateTouches, gateIsBranching, frontier}) =
    SeqBasis.filter (okayToVisit sched)
      (SeqBasis.tabulate 100 (0, numGates) (fn i => i))

  fun sample gidx = raise Fail "not implemented"

  fun pickNext sched =
    let
      val readyGates = readyGates sched
    in
      if Seq.length readyGates = 0 then
        NONE
      else
        let
          val gidxesWithSample =
            Seq.map (fn gidx => (gidx, sample gidx)) readyGates
          val (gidx, sampled) =
            Seq.reduce (fn (a, b) => if snd a < snd b then a else b)
              gidxesWithSample
        in
          SOME gidx
        end
    end
  (*
      1. for each ready gate, sample them
      2. take minimum
      3. pick the gate
      *)
  (*
      TODO: 
      1. add a max depth parameter
      *)


  fun scheduler args =
    let val sched = new args
    in fn () => pickNext sched
    end

end
