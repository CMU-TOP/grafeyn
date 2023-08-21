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

  | _ =>
      Util.die
        ("unknown scheduler: " ^ schedulerName
         ^
         "; valid options are: naive, greedy-branching (gb), greedy-nonbranching (gnb)")

(* ========================================================================
 * mains: 32-bit and 64-bit
 *)

structure M32 =
  MkMain
    (structure C = Complex32
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M64 =
  MkMain
    (structure C = Complex64
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = gateScheduler
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

val main =
  case precision of
    32 => M32.main
  | 64 => M64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

val _ = main ()
