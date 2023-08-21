structure CLA = CommandLineArgs

val precision = CLA.parseInt "precision" 32
val blockSize = CLA.parseInt "expand-block-size" 10000
val maxload = CLA.parseReal "expand-max-load" 0.9
val maxBranchingStride = CLA.parseInt "max-branching-stride" 2
val disableFusion = CLA.parseFlag "disable-fusion"
val naiveSchedule = CLA.parseFlag "naive-schedule"
val doMeasureZeros = CLA.parseFlag "measure-zeros"
val denseThreshold = CLA.parseReal "dense-thresh" 0.25
val pullThreshold = CLA.parseReal "pull-thresh" 0.8

val _ = print ("precision " ^ Int.toString precision ^ "\n")
val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")
val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")
val _ = print ("max-branching-stride " ^ Int.toString maxBranchingStride ^ "\n")
val _ = print
  ("measure-zeros? " ^ (if doMeasureZeros then "yes" else "no") ^ "\n")
val _ = print
  ("disable-fusion? " ^ (if disableFusion then "yes" else "no") ^ "\n")
val _ = print
  ("naive-schedule? " ^ (if naiveSchedule then "yes" else "no") ^ "\n")
val _ = print ("dense-thresh " ^ Real.toString denseThreshold ^ "\n")
val _ = print ("pull-thresh " ^ Real.toString pullThreshold ^ "\n")


(* ========================================================================
 * gate scheduling
 *)

structure GSN = GateSchedulerNaive

structure GSENB =
  GateSchedulerEagerNonBranching
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

val gateScheduler = if naiveSchedule then GSN.scheduler else GSENB.scheduler

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
