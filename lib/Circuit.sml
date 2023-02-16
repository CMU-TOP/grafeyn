structure Circuit:
sig
  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val toString: circuit -> string

  val numGates: circuit -> int
  val numQubits: circuit -> int

  val simulate: circuit -> SparseState.t

  val simulateSequential: circuit -> SparseState.t
end =
struct

  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val splitThreshold = CommandLineArgs.parseInt "split-threshold" 100
  val sequentialDepthAdvance =
    CommandLineArgs.parseInt "sequential-depth-advance" 10
  val expectOutputSize = CommandLineArgs.parseInt "expect-output-size" 1000

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
        | Gate.X i => "x " ^ qi i
        | Gate.CX {control, target} => "cx " ^ qi control ^ ", " ^ qi target
        | Gate.CPhase {control, target, rot} =>
            "cphase(" ^ Real.toString rot ^ ") " ^ qi control ^ ", " ^ qi target
    in
      Seq.iterate op^ header (Seq.map (fn g => gateToString g ^ ";\n") gates)
    end


  (*
    fun simulate {numQubits, gates} =
      let
        fun gate i = Seq.nth gates i
        val depth = Seq.length gates
  
        fun sequentialLoop (i, stop) (acc, widx) =
          if i >= stop then
            widx :: acc
          else
            case Gate.apply (gate i) widx of
              Gate.OutputOne widx' => sequentialLoop (i + 1, stop) (acc, widx')
            | Gate.OutputTwo (widx1, widx2) =>
                sequentialLoop (i + 1, stop)
                  (sequentialLoop (i + 1, stop) (acc, widx1), widx2)
  
  
        fun advanceState (i, stop) state =
          SparseState.fromSeq (Seq.fromList
            (Seq.iterate (sequentialLoop (i, stop)) [] (SparseState.toSeq state)))
  
  
        fun loop i state =
          if i >= depth then
            state
          else if SparseState.size state >= splitThreshold then
            let
              val state = SparseState.toSeq state
              val half = Seq.length state div 2
              val (statel, stater) =
                ForkJoin.par
                  ( fn _ => loop i (SparseState.fromSeq (Seq.take state half))
                  , fn _ => loop i (SparseState.fromSeq (Seq.drop state half))
                  )
            in
              (* SparseState.compact (SparseState.merge (statel, stater)) *)
              SparseState.fromSeq
                (Seq.append (SparseState.toSeq statel, SparseState.toSeq stater))
            end
          else
            let val stop = Int.min (depth, i + sequentialDepthAdvance)
            in loop stop (advanceState (i, stop) state)
            end
      in
        SparseState.compact (loop 0 SparseState.initial)
      end
  *)


  structure HT = HashTable


  fun simulate {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val initialCapacity = expectOutputSize * 2
      val table = HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = initialCapacity
        , maxload = 0.75
        }

      fun loop taskDepth count (i, stop) widx =
        if i >= stop then
          (HT.insertWith Complex.+ table widx; count + 1)
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' => loop taskDepth count (i + 1, stop) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              if taskDepth >= 5 then
                loop taskDepth (loop taskDepth count (i + 1, stop) widx1)
                  (i + 1, stop) widx2
              else
                let
                  val (countl, countr) =
                    ForkJoin.par
                      ( fn _ => loop (taskDepth + 1) 0 (i + 1, stop) widx1
                      , fn _ => loop (taskDepth + 1) 0 (i + 1, stop) widx2
                      )
                in
                  count + countl + countr
                end

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val totalInserts = loop 0 0 (0, depth) initial
      val result = HT.compact table

      val _ = print
        ("duplicates " ^ Int.toString (totalInserts - Seq.length result) ^ "\n")

      val resultNoZeros =
        Seq.filter (fn (bidx, w) => Complex.isNonZero w) result

      val _ = print
        ("zeros " ^ Int.toString (Seq.length result - Seq.length resultNoZeros)
         ^ "\n")
    in
      SparseState.fromSeq resultNoZeros
    end


  fun simulateSequential {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val initialCapacity = expectOutputSize * 2
      val table = HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = initialCapacity
        , maxload = 0.75
        }

      fun loop count i widx =
        if i >= depth then
          (HT.insertWith Complex.+ table widx; count + 1)
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' => loop count (i + 1) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              loop (loop count (i + 1) widx1) (i + 1) widx2

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val totalInserts = loop 0 0 initial
      val result = HT.compact table

      val _ = print
        ("duplicates " ^ Int.toString (totalInserts - Seq.length result) ^ "\n")

      val resultNoZeros =
        Seq.filter (fn (bidx, w) => Complex.isNonZero w) result

      val _ = print
        ("zeros " ^ Int.toString (Seq.length result - Seq.length resultNoZeros)
         ^ "\n")
    in
      SparseState.fromSeq resultNoZeros
    end


(*
  fun simulate {numQubits: int} circuit =
    let
      fun dump isFirst state =
        let val front = if isFirst then "" else "--------\n"
        in print (front ^ SparseState.toString {numQubits = numQubits} state)
        end

      fun doGate (state, gate) =
        let val result = SparseState.compact (Gate.apply gate state)
        in dump false result; result
        end
    in
      dump true SparseState.initial;
      Seq.iterate doGate SparseState.initial circuit
    end
*)

end
