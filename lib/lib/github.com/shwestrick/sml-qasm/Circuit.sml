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

      fun rtos r =
        if r < 0.0 then "-" ^ Real.toString (~r) else Real.toString r

      fun gateToString gate =
        case gate of
          Gate.PauliY i => "y " ^ qi i
        | Gate.PauliZ i => "z " ^ qi i
        | Gate.Hadamard i => "h " ^ qi i
        | Gate.T i => "t " ^ qi i
        | Gate.SqrtY i => "sqrt(y) " ^ qi i
        | Gate.SqrtX i => "sqrt(x) " ^ qi i
        | Gate.SqrtW i => "sqrt(w) " ^ qi i
        | Gate.X i => "x " ^ qi i
        | Gate.CX {control, target} => "cx " ^ qi control ^ ", " ^ qi target
        | Gate.CCX {control1, control2, target} =>
            "ccx " ^ qi control1 ^ ", " ^ qi control2 ^ ", " ^ qi target
        | Gate.CPhase {control, target, rot} =>
            "cphase(" ^ rtos rot ^ ") " ^ qi control ^ ", " ^ qi target
        | Gate.FSim {left, right, theta, phi} =>
            "fsim(" ^ rtos theta ^ ", " ^ rtos phi ^ ") " ^ qi left ^ ", "
            ^ qi right
        | Gate.RZ {rot, target} => "rz(" ^ rtos rot ^ ") " ^ qi target
    in
      Seq.iterate op^ header (Seq.map (fn g => gateToString g ^ ";\n") gates)
    end

end
