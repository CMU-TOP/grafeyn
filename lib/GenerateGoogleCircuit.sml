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

  datatype desired_gate_type = SqrtX | SqrtY | SqrtW


  fun randomGate seed =
    let val p = Real.fromInt ((Util.hash seed) mod 1000000) / 1000000.0
    in if p < 1.0 / 3.0 then SqrtX else if p < 2.0 / 3.0 then SqrtY else SqrtW
    end


  fun randomGateExcept gate seed =
    let val gate' = randomGate seed
    in if gate' <> gate then gate' else randomGateExcept gate (Util.hash seed)
    end


  fun genTwoQubitPattern {numRows, numCols, pattern} =
    raise Fail "TODO: CONTINUE HERE: not yet implemented"


  fun generate {numQubitRows, numQubitCols, numCycles, patterns, seed} =
    let
      val seed = Util.hash seed
      val numQubits = numQubitRows * numQubitCols

      fun pickSeed (qubitIdx, cycle) =
        seed + cycle * numQubits + qubitIdx

      fun fsim (qi1, qi2) =
        Gate.FSim
          {left = qi1, right = qi2, theta = Math.pi / 2.0, phi = Math.pi / 6.0}

      fun desiredGateType (qubitIdx, cycle) =
        if cycle = 0 then
          randomGate (pickSeed (qubitIdx, cycle))
        else
          randomGateExcept (desiredGateType (qubitIdx, cycle - 1))
            (pickSeed (qubitIdx, cycle))

      fun pickGate cycle qubitIdx =
        case desiredGateType (qubitIdx, cycle) of
          SqrtX => Gate.SqrtX qubitIdx
        | SqrtY => Gate.SqrtY qubitIdx
        | SqrtW => Gate.SqrtW qubitIdx

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
