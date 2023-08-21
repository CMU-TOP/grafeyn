signature GATE_SCHEDULER =
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

  (* destructively updates sched *)
  val pickNext: sched -> gate_idx Seq.t
end
