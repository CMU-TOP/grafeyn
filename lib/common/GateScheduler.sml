structure GateScheduler =
struct

  type qubit_idx = int
  type gate_idx = int

  type args =
    { numQubits: int
    , numGates: int
    , gateTouches: gate_idx -> qubit_idx Seq.t
    , gateIsBranching: gate_idx -> bool
    }

  type t = args -> (unit -> gate_idx Seq.t)

end
