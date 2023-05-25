structure CLA = CommandLineArgs

val precision = CLA.parseInt "precision" 32
val blockSize = CLA.parseInt "expand-block-size" 10000
val maxload = CLA.parseReal "expand-max-load" 0.9
val maxBranchingStride = CLA.parseInt "max-branching-stride" 1
val doMeasureZeros = CLA.parseFlag "measure-zeros"
val denseThreshold = CLA.parseReal "dense-thresh" 0.3

val _ = print ("precision " ^ Int.toString precision ^ "\n")
val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")
val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")
val _ = print ("max-branching-stride " ^ Int.toString maxBranchingStride ^ "\n")
val _ = print
  ("measure-zeros " ^ (if doMeasureZeros then "true" else "false") ^ "\n")
val _ = print ("dense-thresh " ^ Real.toString denseThreshold ^ "\n")

structure M32 =
  MkMain
    (structure C = Complex32
     val blockSize = blockSize
     val maxload = maxload
     val maxBranchingStride = maxBranchingStride
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold)

structure M64 =
  MkMain
    (structure C = Complex64
     val blockSize = blockSize
     val maxload = maxload
     val maxBranchingStride = maxBranchingStride
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold)

val main =
  case precision of
    32 => M32.main
  | 64 => M64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

val _ = main ()
