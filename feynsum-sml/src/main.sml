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
val schedulerName = CLA.parseString "scheduler" "gfq"
val _ = print ("scheduler " ^ schedulerName ^ "\n")

(* input circuit *)
val inputName = CLA.parseString "input" ""
val _ = print ("input " ^ inputName ^ "\n")

(* =========================================================================
 * parse input
 *)

val _ = print
  ("-------------------------------\n\
   \--- input-specific specs\n\
   \-------------------------------\n")

fun parseQasm () =
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

val (circuit, depGraph) = (*(circuit, optDepGraph)*)
  case inputName of
    "" => Util.die ("missing: -input FILE.qasm")

  | _ =>
    if String.isSuffix ".qasm" inputName then
      raise Fail ".qasm no longer supported, use .json dependency graph"
      (*parseQasm (), NONE*)
    else
      let val dg = DepGraph.fromFile inputName
          val circuit = {numQubits = #numQubits dg, gates = #gates dg}
      in
        (circuit, dg)
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
 * gate scheduling
 *)


val disableFusion = CLA.parseFlag "scheduler-disable-fusion"
val maxBranchingStride = CLA.parseInt "scheduler-max-branching-stride" 2
structure GNB =
  GateSchedulerGreedyNonBranching
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

structure GFQ =
  GateSchedulerGreedyFinishQubit
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

structure DGNB =
  DepGraphSchedulerGreedyNonBranching
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

structure DGFQ =
  DepGraphSchedulerGreedyFinishQubit
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

fun print_sched_info () =
  let
    val _ = print
      ("-------------------------------------\n\
       \--- scheduler-specific args\n\
       \-------------------------------------\n")
    val _ = print
      ("scheduler-max-branching-stride " ^ Int.toString maxBranchingStride
       ^ "\n")
    val _ = print
      ("scheduler-disable-fusion? " ^ (if disableFusion then "yes" else "no")
       ^ "\n")
    val _ = print ("-------------------------------------\n")
  in
    ()
  end

type gate_idx = int
type Schedule = gate_idx Seq.t

fun dep_graph_to_schedule (dg: DepGraph.t) (sched: DepGraphScheduler.t) =
    let val choose = sched dg
        val st = DepGraphUtil.initState dg
        fun loadNext (acc: gate_idx list) =
            let val frntr = DepGraphUtil.frontier st in
              if Seq.length frntr = 0 then
                Seq.rev (Seq.fromList acc)
              else
                (let val next = choose frntr in
                   DepGraphUtil.visit dg next st;
                   loadNext (next :: acc)
                 end)
            end
    in
      GateSchedulerOrder.mkScheduler (Seq.map Seq.singleton (loadNext nil))
    end

val maxBranchingStride' = if disableFusion then 1 else maxBranchingStride

(*fun greedybranching () =
  case optDepGraph of
    NONE => GateSchedulerGreedyBranching.scheduler
  | SOME dg => GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DepGraphSchedulerGreedyBranching.scheduler { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion  maxBranchingStride')

fun greedynonbranching () =
  case optDepGraph of
    NONE => GNB.scheduler
  | SOME dg => GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGNB.scheduler { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion  maxBranchingStride')

fun greedyfinishqubit () =
  case optDepGraph of
    NONE => GFQ.scheduler
  | SOME dg =>
    GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.scheduler { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion maxBranchingStride')

fun greedyfinishqubit2 () =
  case optDepGraph of
    NONE => GFQ.scheduler
  | SOME dg =>
    (*GateSchedulerOrder.mkScheduler (Seq.fromList (DGFQ.ordered { depGraph = dg, gateIsBranching = gate_branching dg }))*)
    (* dep_graph_to_schedule { depGraph = dg, gateIsBranching = gate_branching dg } DGFQ.scheduler2 *)
    GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.scheduler2 { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion maxBranchingStride')

fun greedyfinishqubit3 () =
  case optDepGraph of
    NONE => GFQ.scheduler
  | SOME dg =>
    GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.scheduler3 { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion maxBranchingStride')

fun greedyfinishqubit4 () =
  case optDepGraph of
    NONE => GFQ.scheduler
  | SOME dg =>
    GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.scheduler4 { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion maxBranchingStride')

fun greedyfinishqubit5 () =
  case optDepGraph of
    NONE => GFQ.scheduler
  | SOME dg =>
    GateSchedulerOrder.mkScheduler (DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.scheduler5 { depGraph = dg, gateIsBranching = gate_branching dg }) disableFusion maxBranchingStride')

fun randomsched (samples: int) =
  case optDepGraph of
    NONE => raise Fail "Need dep graph for random scheduler"
  | SOME dg =>
    let val ags = { depGraph = dg, gateIsBranching = gate_branching dg }
        (*val scheds = Seq.fromList (List.tabulate (samples, fn i => DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.schedulerRandom i ags) maxBranchingStride')) *)
        val scheds = Seq.tabulate (fn i => DepGraphUtil.scheduleWithOracle dg (gate_branching dg) (DGFQ.schedulerRandom i ags) disableFusion maxBranchingStride') samples
        val chosen = DepGraphUtil.chooseSchedule scheds (gate_branching dg)
    in
      GateSchedulerOrder.mkScheduler chosen
    end*)

(*val gateScheduler =
  case schedulerName of
    "naive" => GateSchedulerNaive.scheduler

  | "greedy-branching" => (print_sched_info (); greedybranching ())
  | "gb" => (print_sched_info (); greedybranching ())

  | "greedy-nonbranching" => (print_sched_info (); greedynonbranching ())
  | "gnb" => (print_sched_info (); greedynonbranching ())

  | "greedy-finish-qubit" => (print_sched_info (); greedyfinishqubit ())
  | "gfq" => (print_sched_info (); greedyfinishqubit ())

  | "greedy-finish-qubit2" => (print_sched_info (); greedyfinishqubit2 ())
  | "gfq2" => (print_sched_info (); greedyfinishqubit2 ())

  | "greedy-finish-qubit3" => (print_sched_info (); greedyfinishqubit3 ())
  | "gfq3" => (print_sched_info (); greedyfinishqubit3 ())

  | "greedy-finish-qubit4" => (print_sched_info (); greedyfinishqubit4 ())
  | "gfq4" => (print_sched_info (); greedyfinishqubit4 ())

  | "greedy-finish-qubit5" => (print_sched_info (); greedyfinishqubit5 ())
  | "gfq5" => (print_sched_info (); greedyfinishqubit5 ())

  | "random" => randomsched 50

  | _ =>
      Util.die
        ("unknown scheduler: " ^ schedulerName
         ^
         "; valid options are: naive, greedy-branching (gb), greedy-nonbranching (gnb), greedy-finish-qubit (gfq)")*)

(* ========================================================================
 * mains: 32-bit and 64-bit
 *)

structure M_64_32 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex32
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_64_64 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex64
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_32 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex32
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_64 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex64
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
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

val _ = main (inputName, depGraph)
