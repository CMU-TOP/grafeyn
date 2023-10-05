(* ========================================================================
 * A little boilerplate, but hiding this behind an interface seems nice
 *)
structure SimAccumulator :>
sig
  type t
  val init: {desired: BasisIdx.t} -> t

  val logSpaceUsage: t -> int -> t

  val logGateApps: t -> int -> t

  val logNumWaves: t -> int -> t

  val finishWave: t -> Wave.t -> t

  val view:
    t
    -> { weight: Complex.t
       , numGateApps: int
       , maxSpaceUsage: int
       , maxNumWaves: int
       }
end =
struct
  type t =
    { desired: BasisIdx.t
    , weight: Complex.t
    , numGateApps: int
    , maxSpaceUsage: int
    , maxNumWaves: int
    }

  fun init {desired} =
    { desired = desired
    , weight = Complex.zero
    , numGateApps = 0
    , maxSpaceUsage = 1
    , maxNumWaves = 1
    }

  fun logGateApps {desired, weight, numGateApps, maxSpaceUsage, maxNumWaves} n =
    { desired = desired
    , weight = weight
    , numGateApps = numGateApps + n
    , maxSpaceUsage = maxSpaceUsage
    , maxNumWaves = maxNumWaves
    }

  fun logSpaceUsage {desired, weight, numGateApps, maxSpaceUsage, maxNumWaves} n =
    ( if n <= maxSpaceUsage then ()
      else print ("[ACC] space usage increase: " ^ Int.toString n ^ "\n")
    ; { desired = desired
      , weight = weight
      , numGateApps = numGateApps
      , maxSpaceUsage = Int.max (maxSpaceUsage, n)
      , maxNumWaves = maxNumWaves
      }
    )

  fun logNumWaves {desired, weight, numGateApps, maxSpaceUsage, maxNumWaves} n =
    { desired = desired
    , weight = weight
    , numGateApps = numGateApps
    , maxSpaceUsage = maxSpaceUsage
    , maxNumWaves = Int.max (n, maxNumWaves)
    }

  fun finishWave {desired, weight, numGateApps, maxSpaceUsage, maxNumWaves} wave =
    { desired = desired
    , weight = Complex.+ (weight, Wave.lookup wave desired)
    , numGateApps = numGateApps
    , maxSpaceUsage = maxSpaceUsage
    , maxNumWaves = maxNumWaves
    }

  fun view {desired, weight, numGateApps, maxSpaceUsage, maxNumWaves} =
    { weight = weight
    , numGateApps = numGateApps
    , maxSpaceUsage = maxSpaceUsage
    , maxNumWaves = maxNumWaves
    }
end
