structure CLA = CommandLineArgs
structure M32 = MkMain(Complex32)
structure M64 = MkMain(Complex64)

val precision = CommandLineArgs.parseInt "precision" 32
val _ = print ("precision " ^ Int.toString precision ^ "\n")

val main =
  case precision of
    32 => M32.main
  | 64 => M64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

val _ = main ()
