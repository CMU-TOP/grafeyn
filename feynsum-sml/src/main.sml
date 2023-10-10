structure CLA = CommandLineArgs

(* 32-bit or 64-bit precision for floating point arithmetic *)
val precision = CLA.parseInt "precision" 32
val _ = print ("precision " ^ Int.toString precision ^ "\n")

(* some parameters for the expand subroutine *)
val blockSize = CLA.parseInt "expand-block-size" 10000
val maxload = CLA.parseReal "expand-max-load" 0.9
val denseThreshold = CLA.parseReal "dense-thresh" 0.25
val pullThreshold = CLA.parseReal "pull-thresh" 0.8
val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")
val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")
val _ = print ("dense-thresh " ^ Real.toString denseThreshold ^ "\n")
val _ = print ("pull-thresh " ^ Real.toString pullThreshold ^ "\n")

(* should the simulator print out info about how many zeros were eliminated
 * on each frontier? (default: no)
 *)
val doMeasureZeros = CLA.parseFlag "measure-zeros"
val _ = print
  ("measure-zeros? " ^ (if doMeasureZeros then "yes" else "no") ^ "\n")

(* gate scheduler name *)
val schedulerName = CLA.parseString "scheduler" "greedy-nonbranching"
val _ = print ("scheduler " ^ schedulerName ^ "\n")

(* input circuit *)
val inputName = CLA.parseString "input" ""
val _ = print ("input " ^ inputName ^ "\n")

(* ========================================================================
 * gate scheduling
 *)


local
  val gnbDisableFusion = CLA.parseFlag "scheduler-gnb-disable-fusion"
  val gnbMaxBranchingStride =
    CLA.parseInt "scheduler-gnb-max-branching-stride" 2
in
  structure GNB =
    GateSchedulerGreedyNonBranching
      (val maxBranchingStride = gnbMaxBranchingStride
       val disableFusion = gnbDisableFusion)

  fun print_gnb_info () =
    let
      val _ = print
        ("-------------------------------------\n\
         \--- scheduler-specific args\n\
         \-------------------------------------\n")
      val _ = print
        ("scheduler-gnb-max-branching-stride "
         ^ Int.toString gnbMaxBranchingStride ^ "\n")
      val _ = print
        ("scheduler-gnb-disable-fusion? "
         ^ (if gnbDisableFusion then "yes" else "no") ^ "\n")
      val _ = print ("-------------------------------------\n")
    in
      ()
    end
end


val gateScheduler =
  case schedulerName of
    "naive" => GateSchedulerNaive.scheduler

  | "greedy-branching" => GateSchedulerGreedyBranching.scheduler
  | "gb" => GateSchedulerGreedyBranching.scheduler

  | "greedy-nonbranching" => (print_gnb_info (); GNB.scheduler)
  | "gnb" => (print_gnb_info (); GNB.scheduler)

  | "greedy-finish-qubit" => GateSchedulerGreedyFinishQubit.scheduler
  | "gfq" => GateSchedulerGreedyFinishQubit.scheduler

  | _ =>
      Util.die
        ("unknown scheduler: " ^ schedulerName
         ^
         "; valid options are: naive, greedy-branching (gb), greedy-nonbranching (gnb), greedy-finish-qubit (gfq)")

(* =========================================================================
 * parse input
 *)

val _ = print
  ("-------------------------------\n\
   \--- input-specific specs\n\
   \-------------------------------\n")

val circuit =
  case inputName of
    "" => Util.die ("missing: -input FILE.qasm")

  | _ =>
      let
        fun handleLexOrParseError exn =
          let
            val e =
              case exn of
                SMLQasmError.Error e => e
              | other => raise other
          in
            TerminalColorString.print
              (SMLQasmError.show
                 {highlighter = SOME SMLQasmSyntaxHighlighter.fuzzyHighlight} e);
            OS.Process.exit OS.Process.failure
          end

        val ast = SMLQasmParser.parseFromFile inputName
                  handle exn => handleLexOrParseError exn

        val simpleCirc = SMLQasmSimpleCircuit.fromAst ast
      in
        Circuit.fromSMLQasmSimpleCircuit simpleCirc
      end

val _ = print ("-------------------------------\n")

val _ = print ("gates  " ^ Int.toString (Circuit.numGates circuit) ^ "\n")
val _ = print ("qubits " ^ Int.toString (Circuit.numQubits circuit) ^ "\n")

val showCircuit = CLA.parseFlag "show-circuit"
val _ = print ("show-circuit? " ^ (if showCircuit then "yes" else "no") ^ "\n")
val _ =
  if not showCircuit then
    ()
  else
    print
      ("=========================================================\n"
       ^ Circuit.toString circuit
       ^ "=========================================================\n")

(* ========================================================================
 * mains: 32-bit and 64-bit
 *)

structure M_64_32 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex32
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_64_64 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex64
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_32 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex32
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_64 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex64
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

val basisIdx64Okay = Circuit.numQubits circuit <= 62

val main =
  case precision of
    32 => if basisIdx64Okay then M_64_32.main else M_U_32.main
  | 64 => if basisIdx64Okay then M_64_64.main else M_U_64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

(* ======================================================================== *)

val _ = main (inputName, circuit)
