structure Circuit:
sig
  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val toString: circuit -> string

  val numGates: circuit -> int
  val numQubits: circuit -> int
end =
struct

  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  fun numGates ({gates, ...}: circuit) = Seq.length gates
  fun numQubits ({numQubits = nq, ...}: circuit) = nq


  fun toString {numQubits, gates} =
    let
      val header = "qreg q[" ^ Int.toString numQubits ^ "];\n"

      fun qi i =
        "q[" ^ Int.toString i ^ "]"

      fun gateToString gate =
        case gate of
          Gate.PauliY i => "y " ^ qi i
        | Gate.PauliZ i => "z " ^ qi i
        | Gate.Hadamard i => "h " ^ qi i
        | Gate.T i => "t " ^ qi i
        | Gate.SqrtY i => "sqrt(y) " ^ qi i
        | Gate.X i => "x " ^ qi i
        | Gate.CX {control, target} => "cx " ^ qi control ^ ", " ^ qi target
        | Gate.CPhase {control, target, rot} =>
            "cphase(" ^ Real.toString rot ^ ") " ^ qi control ^ ", " ^ qi target
    in
      Seq.iterate op^ header (Seq.map (fn g => gateToString g ^ ";\n") gates)
    end

end
