(* A "naive" scheduler: execute in straightline order, i.e., as the gates are
 * written in the input qasm. No fusion.
 *)
structure GateSchedulerNaive:
sig
  val scheduler: GateScheduler.t
end =
struct

  type qubit_idx = int
  type gate_idx = int

  datatype sched = S of {numGates: int, next: int ref}

  type t = sched

  fun new
    { numQubits: int
    , numGates: int
    , gateTouches: gate_idx -> qubit_idx Seq.t
    , gateIsBranching: gate_idx -> bool
    } =
    S {numGates = numGates, next = ref 0}

  fun pickNext (S {numGates, next, ...}) =
    if !next >= numGates then Seq.empty ()
    else let val gi = !next in next := gi + 1; Seq.singleton gi end

  fun scheduler args =
    let val sched = new args
    in fn () => pickNext sched
    end

end
