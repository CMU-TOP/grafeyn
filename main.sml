structure CLA = CommandLineArgs

val precision = CommandLineArgs.parseInt "precision" 32
val _ = print ("precision " ^ Int.toString precision ^ "\n")
val blockSize = CommandLineArgs.parseInt "expand-block-size" 10000
val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")
val maxload = CommandLineArgs.parseReal "expand-max-load" 0.9
val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")
val maxBranchingStride = CommandLineArgs.parseInt "max-branching-stride" 1
val _ = print ("max-branching-stride " ^ Int.toString maxBranchingStride ^ "\n")
val doMeasureZeros = CommandLineArgs.parseFlag "measure-zeros"
val _ = print
  ("measure-zeros " ^ (if doMeasureZeros then "true" else "false") ^ "\n")

structure M32 =
  MkMain
    (structure C = Complex32
     val blockSize = blockSize
     val maxload = maxload
     val maxBranchingStride = maxBranchingStride
     val doMeasureZeros = doMeasureZeros)

structure M64 =
  MkMain
    (structure C = Complex64
     val blockSize = blockSize
     val maxload = maxload
     val maxBranchingStride = maxBranchingStride
     val doMeasureZeros = doMeasureZeros)

val main =
  case precision of
    32 => M32.main
  | 64 => M64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

val _ = main ()
