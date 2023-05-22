structure Gate :>
sig
  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | SqrtY of qubit_idx
  | SqrtX of qubit_idx
  | SqrtW of qubit_idx
  | X of qubit_idx
  | T of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CCX of {control1: qubit_idx, control2: qubit_idx, target: qubit_idx}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }

  (** fsim(theta, phi) :=
    *   [ [ 1,   0,              0,              0          ],
    *     [ 0,   cos(theta),     -i sin(theta),  0          ],
    *     [ 0,   -i sin(theta),  cos(theta),     0          ],
    *     [ 0,   0,              0,              e^(-i phi) ] ]
    *)
  | FSim of {left: qubit_idx, right: qubit_idx, theta: real, phi: real}
  | RZ of {rot: real, target: qubit_idx}
  | RY of {rot: real, target: qubit_idx}
  | CSwap of {control: qubit_idx, target1: qubit_idx, target2: qubit_idx}

  | Other of {name: string, params: real Seq.t option, args: qubit_idx Seq.t}

  type t = gate

end =
struct

  type qubit_idx = int

  datatype gate =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | SqrtY of qubit_idx
  | SqrtX of qubit_idx
  | SqrtW of qubit_idx
  | T of qubit_idx
  | X of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CCX of {control1: qubit_idx, control2: qubit_idx, target: qubit_idx}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }
  | FSim of {left: qubit_idx, right: qubit_idx, theta: real, phi: real}
  | RZ of {rot: real, target: qubit_idx}
  | RY of {rot: real, target: qubit_idx}
  | CSwap of {control: qubit_idx, target1: qubit_idx, target2: qubit_idx}

  | Other of {name: string, params: real Seq.t option, args: qubit_idx Seq.t}

  type t = gate

end
