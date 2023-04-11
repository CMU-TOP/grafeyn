(* ========================================================================
 * A little boilerplate, but hiding this behind an interface seems nice
 *)
structure SimAccumulator:
sig
  type t
  val init: {desired: BasisIdx.t} -> t
  val logGateApps: t -> int -> t
  val finishWave: t -> Wave.t -> t
  val view: t -> {weight: Complex.t, numGateApps: int}
end =
struct
  type t = {desired: BasisIdx.t, weight: Complex.t, numGateApps: int}
  fun init {desired} =
    {desired = desired, weight = Complex.zero, numGateApps = 0}
  fun logGateApps {desired, weight, numGateApps} n =
    {desired = desired, weight = weight, numGateApps = numGateApps + n}
  fun finishWave {desired, weight, numGateApps} wave =
    { desired = desired
    , weight = Complex.+ (weight, Wave.lookup wave desired)
    , numGateApps = numGateApps
    }
  fun view {desired, weight, numGateApps} =
    {weight = weight, numGateApps = numGateApps}
end
