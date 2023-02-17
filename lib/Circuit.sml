structure Circuit:
sig
  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val toString: circuit -> string

  val numGates: circuit -> int
  val numQubits: circuit -> int

  val simulate: circuit -> SparseState.t
  val simulateBreadthFirst: circuit -> SparseState.t
  val simulateSequential: circuit -> SparseState.t
end =
struct

  structure HT = HashTable

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


  fun compactSparseState s =
    let
      val s = SparseState.toSeq s

      val table = HT.make
        { hash = BasisIdx.hash
        , eq = BasisIdx.equal
        , capacity = 2 * Seq.length s
        , maxload = 0.75
        }

      val _ = ForkJoin.parfor 1000 (0, Seq.length s) (fn i =>
        HT.insertWith Complex.+ table (Seq.nth s i))
    in
      SparseState.fromSeq (HT.compact table)
    end


  fun simulateBreadthFirst {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      fun loop i countGateApp state =
        if i >= depth then
          (countGateApp, state)
        else
          let
            val countGateApp = countGateApp + SparseState.size state
          in
            loop (i + 1) countGateApp (SparseState.compact
              (Gate.applyState (gate i) state))
          end

      val (totalGateApps, state) = loop 0 0 SparseState.initial
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")
    in
      state
    end


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

      fun loop taskDepth (gateAppCount, insertCount) (i, stop) widx =
        if i >= stop then
          (HT.insertWith Complex.+ table widx; (gateAppCount, insertCount + 1))
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' =>
              loop taskDepth (gateAppCount + 1, insertCount) (i + 1, stop) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              if taskDepth >= 5 then
                let
                  val xx =
                    loop taskDepth (gateAppCount + 1, insertCount) (i + 1, stop)
                      widx1
                in
                  loop taskDepth xx (i + 1, stop) widx2
                end
              else
                let
                  val
                    ( (gateAppCountL, insertCountL)
                    , (gateAppCountR, insertCountR)
                    ) =
                    ForkJoin.par
                      ( fn _ => loop (taskDepth + 1) (0, 0) (i + 1, stop) widx1
                      , fn _ => loop (taskDepth + 1) (0, 0) (i + 1, stop) widx2
                      )
                in
                  ( gateAppCount + 1 + gateAppCountL + gateAppCountR
                  , insertCount + insertCountL + insertCountR
                  )
                end

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, totalInserts) = loop 0 (0, 0) (0, depth) initial
      val result = HT.compact table

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")

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

      fun loop (gateAppCount, insertCount) i widx =
        if i >= depth then
          (HT.insertWith Complex.+ table widx; (gateAppCount, insertCount + 1))
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' =>
              loop (gateAppCount + 1, insertCount) (i + 1) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              let val xx = loop (gateAppCount + 1, insertCount) (i + 1) widx1
              in loop xx (i + 1) widx2
              end

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val (totalGateApps, totalInserts) = loop (0, 0) 0 initial
      val result = HT.compact table

      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n")

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
