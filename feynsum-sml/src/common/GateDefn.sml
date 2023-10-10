structure GateDefn =
struct

  type qubit_idx = int

  datatype t =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | SqrtX of qubit_idx
  | Sxdg of qubit_idx
  | S of qubit_idx
  | Sdg of qubit_idx
  | X of qubit_idx
  | T of qubit_idx
  | Tdg of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CZ of {control: qubit_idx, target: qubit_idx}
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
  | RX of {rot: real, target: qubit_idx}
  | CSwap of {control: qubit_idx, target1: qubit_idx, target2: qubit_idx}
  | U of {target: qubit_idx, theta: real, phi: real, lambda: real}
  | Other of {name: string, params: real Seq.t, args: qubit_idx Seq.t}

  type gate = t

end
