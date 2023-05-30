(* In the Google quantum supremacy experiment, they developed a chip where each
 * qubit is connected to up to four neighboring qubits. If we give each qubit
 * a coordinate (row, col), the arrangement is as shown below. Connected qubits
 * allow for two-qubit gate interactions. At any moment, each qubit may
 * participate in at most one two-qubit interaction.
 *
 *     (0,0) (0,1) (0,2) ...
 *       |  /  |  /  |
 *     (1,0) (1,1) (1,2) ...
 *       |  \  |  \  |
 *     (2,0) (2,1) (2,2) ...    
 *       |  /  |  /  | 
 *     (3,0) (3,1) (3,2) ...
 *      ...   ...   ...
 *
 * ---------------------------------------------------------------------------
 * They identify 8 patterns of two-qubit gate interactions,
 * labeled A through H.
 *
 * A
 *     (0,0) (0,1) (0,2) ...
 *
 *     (1,0) (1,1) (1,2) ...
 *       |     |     |
 *     (2,0) (2,1) (2,2) ...    
 *
 *     (3,0) (3,1) (3,2) ...
 *       |     |     |
 *     (4,0) (4,1) (4,2) ...
 *      ...   ...   ...
 *
 * B
 *     (0,0) (0,1) (0,2) ...
 *          /     /
 *     (1,0) (1,1) (1,2) ...
 *
 *     (2,0) (2,1) (2,2) ...    
 *          /     /
 *     (3,0) (3,1) (3,2) ...
 *      ...   ...   ...
 *
 * C
 *     (0,0) (0,1) (0,2) ...
 *       |     |     |
 *     (1,0) (1,1) (1,2) ...
 *
 *     (2,0) (2,1) (2,2) ...    
 *       |     |     |
 *     (3,0) (3,1) (3,2) ...
 *      ...   ...   ...
 *
 * D
 *     (0,0) (0,1) (0,2) ...
 *
 *     (1,0) (1,1) (1,2) ...
 *          \     \
 *     (2,0) (2,1) (2,2) ...    
 *
 *     (3,0) (3,1) (3,2) ...
 *          \     \
 *     (4,0) (4,1) (4,2) ...
 *      ...   ...   ...
 *
 * E
 *     (0,0) (0,1) (0,2) (0,3) ...
 *       |           |
 *     (1,0) (1,1) (1,2) (1,3) ...
 *                \
 *     (2,0) (2,1) (2,2) (2,3) ...    
 *             |           |
 *     (3,0) (3,1) (3,2) (3,3) ...
 *          \           \
 *     (4,0) (4,1) (4,2) (4,3) ...
 *       |           | 
 *      ...   ...   ...   ...
 *
 * F
 *     (0,0) (0,1) (0,2) (0,3) ...
 *             |           |
 *     (1,0) (1,1) (1,2) (1,3) ...
 *          \           \
 *     (2,0) (2,1) (2,2) (2,3) ...    
 *       |           |
 *     (3,0) (3,1) (3,2) (3,3) ...
 *                \
 *     (4,0) (4,1) (4,2) (4,3) ...
 *             |           |
 *      ...   ...   ...   ...
 *
 * G
 *     (0,0) (0,1) (0,2) (0,3) ...
 *          /           /
 *     (1,0) (1,1) (1,2) (1,3) ...
 *             |           |
 *     (2,0) (2,1) (2,2) (2,3) ...    
 *                /           /
 *     (3,0) (3,1) (3,2) (3,3) ...
 *       |           |
 *     (4,0) (4,1) (4,2) (4,3) ...
 *          /           /
 *      ...   ...   ...   ...
 *
 * H
 *     (0,0) (0,1) (0,2) (0,3) ...
 *                /           /
 *     (1,0) (1,1) (1,2) (1,3) ...
 *       |           |
 *     (2,0) (2,1) (2,2) (2,3) ...    
 *          /           /
 *     (3,0) (3,1) (3,2) (3,3) ...
 *             |           |
 *     (4,0) (4,1) (4,2) (4,3) ...
 *                /           /
 *      ...   ...   ...   ...
 *
 * ---------------------------------------------------------------------------
 * The circuits they consider consists of a sequence of cycles, where in each
 * cycle two things occur:
 *   1. first, each qubit is subjected to one of sqrt(X), sqrt(Y), sqrt(W)
 *      gates (chosen randomly, without repetitions on a single qubit)
 *   2. next, a pattern of two-qubit interactions occur. The two-qubit gate
 *      is approximately fSim(theta = pi/2, phi = pi/6).
 *
 * ---------------------------------------------------------------------------
 * So, to generate a circuit below, we need to specify:
 *   1. the number of qubits (here, in terms of number of rows and columns)
 *   2. how many cycles to perform, and
 *   3. the pattern of two-qubit interactions.
 *
 * The two-qubit patterns in their experiments are always repeating sequences.
 * So, for example, using the patterns [E,F,G,H] will repeat [E,F,G,H,E,F,G,...]
 * for the desired number of cycles.
 *
 * We also have to give the generator a seed for randomness. This is just to
 * generate the random choices of single qubit gates.
 *)
structure GenerateGoogleCircuit:
sig
  datatype two_qubit_pattern = A | B | C | D | E | F | G | H
  val twoQubitPatternFromChar: char -> two_qubit_pattern

  val generate:
    { numQubitRows: int
    , numQubitCols: int
    , numCycles: int
    , patterns: two_qubit_pattern Seq.t
    , seed: int (* just for random generation *)
    }
    -> Circuit.t
end =
struct


  datatype two_qubit_pattern = A | B | C | D | E | F | G | H

  fun twoQubitPatternFromChar c =
    case c of
      #"A" => A
    | #"B" => B
    | #"C" => C
    | #"D" => D
    | #"E" => E
    | #"F" => F
    | #"G" => G
    | #"H" => H
    | _ =>
        raise Fail
          ("GenerateGoogleCircuit.twoQubitPatternFromChar: no such pattern: "
           ^ Char.toString c)


  datatype desired_gate_type = SqrtX | SqrtY | SqrtW


  fun randomDesiredGate seed =
    let val p = Real.fromInt ((Util.hash seed) mod 1000000) / 1000000.0
    in if p < 1.0 / 3.0 then SqrtX else if p < 2.0 / 3.0 then SqrtY else SqrtW
    end


  fun randomDesiredGateExcept gate seed =
    let
      val gate' = randomDesiredGate seed
    in
      if gate' <> gate then gate'
      else randomDesiredGateExcept gate (Util.hash seed)
    end


  fun genTwoQubitPattern {numRows, numCols, pattern} =
    let
      fun coordToQubitIdx (row, col) = row * numCols + col
      fun qubitIdxToCoord qi = (qi div numCols, qi mod numCols)
      val numQubits = numRows * numCols

      fun idxInBounds qi = 0 <= qi andalso qi < numQubits
      fun rowInBounds r = 0 <= r andalso r < numRows
      fun colInBounds c = 0 <= c andalso c < numCols

      fun fsim (qi1, qi2) =
        GateDefn.FSim
          {left = qi1, right = qi2, theta = Math.pi / 2.0, phi = Math.pi / 6.0}

      (* qi1 is guaranteed okay. but there might not be a qubit at (row2, col2) *)
      fun checkEdge qi1 (row2, col2) =
        if idxInBounds qi1 andalso rowInBounds row2 andalso colInBounds col2 then
          SOME (fsim (qi1, coordToQubitIdx (row2, col2)))
        else
          NONE

      fun onlyIf b x =
        if b then x else NONE

      fun mapOptionQubits f =
        ArraySlice.full (SeqBasis.tabFilter 1000 (0, numQubits) (fn qi =>
          let val (row, col) = qubitIdxToCoord qi
          in f (qi, row, col)
          end))

      fun genA () =
        mapOptionQubits (fn (qi, row, col) =>
          if row mod 2 = 1 then checkEdge qi (row + 1, col) else NONE)

      fun genB () =
        mapOptionQubits (fn (qi, row, col) =>
          if row mod 2 = 0 then checkEdge qi (row + 1, col - 1) else NONE)

      fun genC () =
        mapOptionQubits (fn (qi, row, col) =>
          if row mod 2 = 0 then checkEdge qi (row + 1, col) else NONE)

      fun genD () =
        mapOptionQubits (fn (qi, row, col) =>
          if row mod 2 = 1 then checkEdge qi (row + 1, col + 1) else NONE)

      fun genE () =
        mapOptionQubits (fn (qi, row, col) =>
          case row mod 4 of
            0 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col))
          | 1 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col + 1))
          | 2 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col))
          | _ => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col + 1)))

      fun genF () =
        mapOptionQubits (fn (qi, row, col) =>
          case row mod 4 of
            0 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col))
          | 1 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col + 1))
          | 2 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col))
          | _ => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col + 1)))

      fun genG () =
        mapOptionQubits (fn (qi, row, col) =>
          case row mod 4 of
            0 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col - 1))
          | 1 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col))
          | 2 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col - 1))
          | _ => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col)))

      fun genH () =
        mapOptionQubits (fn (qi, row, col) =>
          case row mod 4 of
            0 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col - 1))
          | 1 => onlyIf (col mod 2 = 0) (checkEdge qi (row + 1, col))
          | 2 => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col - 1))
          | _ => onlyIf (col mod 2 = 1) (checkEdge qi (row + 1, col)))
    in
      case pattern of
        A => genA ()
      | B => genB ()
      | C => genC ()
      | D => genD ()
      | E => genE ()
      | F => genF ()
      | G => genG ()
      | H => genH ()
    end


  fun generate {numQubitRows, numQubitCols, numCycles, patterns, seed} =
    let
      val seed = Util.hash seed
      val numQubits = numQubitRows * numQubitCols

      fun pickSeed (qubitIdx, cycle) =
        seed + cycle * numQubits + qubitIdx

      fun desiredGateType (qubitIdx, cycle) =
        if cycle = 0 then
          randomDesiredGate (pickSeed (qubitIdx, cycle))
        else
          randomDesiredGateExcept (desiredGateType (qubitIdx, cycle - 1))
            (pickSeed (qubitIdx, cycle))

      fun pickGate cycle qubitIdx =
        case desiredGateType (qubitIdx, cycle) of
          SqrtX => GateDefn.SqrtX qubitIdx
        | SqrtY => GateDefn.SqrtY qubitIdx
        | SqrtW => GateDefn.SqrtW qubitIdx

      fun genCycle cycle =
        let
          val singleQubitGates =
            Seq.tabulate (fn i => pickGate cycle i) numQubits

          val pat = Seq.nth patterns (cycle mod Seq.length patterns)
          val twoQubitGates =
            genTwoQubitPattern
              {numRows = numQubitRows, numCols = numQubitCols, pattern = pat}
        in
          Seq.append (singleQubitGates, twoQubitGates)
        end

    in
      { gates = Seq.flatten (Seq.tabulate genCycle numCycles)
      , numQubits = numQubits
      }
    end

end
