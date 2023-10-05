import "c64"
import "basis_idx"
import "constants"

module gate: {

  type qubit_idx = i64
  type weight = c64.t
  type weighted_idx = (basis_idx.t, weight)

  type gate =
    #Hadamard qubit_idx
  | #PauliY qubit_idx

  type gate_output =
    #OutputOne weighted_idx
  | #OutputTwo weighted_idx weighted_idx

  val apply: gate -> weighted_idx -> gate_output

} = {

  type qubit_idx = i64
  type weight = c64.t
  type weighted_idx = (basis_idx.t, weight)

  type gate =
    #Hadamard qubit_idx
  | #PauliY qubit_idx

  type gate_output =
    #OutputOne weighted_idx
  | #OutputTwo weighted_idx weighted_idx

  def hadamard qi (bidx, weight) : gate_output =
    let bidx1 = bidx
    let bidx2 = basis_idx.flip bidx qi
    let multiplier1 =
      if basis_idx.get bidx qi then
        c64.neg (c64.mk_re constants.recp_sqrt_2)
      else
        c64.mk_re constants.recp_sqrt_2
    let multiplier2 = c64.mk_re constants.recp_sqrt_2
    let weight1 = c64.(weight * multiplier1)
    let weight2 = c64.(weight * multiplier2)
    in #OutputTwo (bidx1, weight1) (bidx2, weight2)

  def pauliy qi (bidx, weight) : gate_output =
    let bidx' = basis_idx.flip bidx qi
    let multiplier =
      if basis_idx.get bidx qi then c64.mk_im (-1f64) else c64.mk_im 1f64
    let weight' = c64.(weight * multiplier)
    in #OutputOne (bidx', weight')

  def apply (gate: gate) widx : gate_output =
    match gate
      case #Hadamard qi -> hadamard qi widx
      case #PauliY qi -> pauliy qi widx
}