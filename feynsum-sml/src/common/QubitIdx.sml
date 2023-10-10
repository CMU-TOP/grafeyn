(* This is a workaround for SML's broken "sharing" constraints.
 * 
 * Here's the gist. The BASIS_IDX signature would like to define a type alias
 * for qubit indices (integers) to make the types more self-documenting:
 *
 *     signature BASIS_IDX =
 *     sig
 *       type t
 *       type qubit_idx = int
 *       ...
 *       val get: t -> qubit_idx -> bool
 *       ...
 *     end
 *
 * However, we then use `sharing` constraints for structures ascribing to
 * BASIS_IDX, and SML unfortunately doesn't allow for `sharing` for a defined
 * type such as `type qubit_idx = ...`
 * (See e.g.: https://stackoverflow.com/questions/28900279/how-is-structure-sharing-broken-in-standard-ml )
 *
 * So, instead we define a structure `QubitIdx` for a workaround:
 *
 *     signature BASIS_IDX =
 *     sig
 *       type t
 *       ...
 *       val get: t -> QubitIdx.t -> bool
 *       ...
 *     end
 *)
structure QubitIdx =
struct type t = int end
