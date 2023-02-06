structure SparseState :>
sig
  (* sparse state is abstractly a set of (weight, BasisIdx.t) pairs:
   *   { (a0, |000>), (a1, |101>), ... }
   *)
  type t
  type state = t
  type weight = Complex.t
  type qubit_idx = int

  val toString: {numQubits: int} -> state -> string

  val empty: state
  val initial: state

  val reportActive: state -> (BasisIdx.t * weight) Seq.t

  (* combine duplicates, eliminate zeros *)
  val compact: state -> state
  val merge: state * state -> state

  val pauliy: state -> qubit_idx -> state
  val pauliz: state -> qubit_idx -> state
  val hadamard: state -> qubit_idx -> state

  val cx: state -> {control: qubit_idx, target: qubit_idx} -> state
end =
struct

  (* Signature mostly comes from ORD_MAP, from SML/NJ lib. I'm making some
   * small changes, so putting the full signature (with changes) here for
   * reference.
   *)
  structure BasisIdxMap:
  sig
    type 'a map
    type 'a t = 'a map
    val empty: 'a map
    val isEmpty: 'a map -> bool
    val singleton: (BasisIdx.t * 'a) -> 'a map
    val insert: 'a map * BasisIdx.t * 'a -> 'a map
    val insert': ((BasisIdx.t * 'a) * 'a map) -> 'a map
    val insertWith: ('a * 'a -> 'a) -> 'a map * BasisIdx.t * 'a -> 'a map
    val insertWithi: (BasisIdx.t * 'a * 'a -> 'a)
                     -> 'a map * BasisIdx.t * 'a
                     -> 'a map
    val find: 'a map * BasisIdx.t -> 'a option
    val lookup: 'a map * BasisIdx.t -> 'a
    val inDomain: ('a map * BasisIdx.t) -> bool
    val remove: 'a map * BasisIdx.t -> 'a map * 'a
    val first: 'a map -> 'a option
    val firsti: 'a map -> (BasisIdx.t * 'a) option
    val numItems: 'a map -> int
    val listItems: 'a map -> 'a list
    val listItemsi: 'a map -> (BasisIdx.t * 'a) list
    val listKeys: 'a map -> BasisIdx.t list
    val collate: ('a * 'a -> order) -> ('a map * 'a map) -> order
    val unionWith: ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi: (BasisIdx.t * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val intersectWith: ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi: (BasisIdx.t * 'a * 'b -> 'c)
                        -> ('a map * 'b map)
                        -> 'c map
    val mergeWith: ('a option * 'b option -> 'c option)
                   -> ('a map * 'b map)
                   -> 'c map
    val mergeWithi: (BasisIdx.t * 'a option * 'b option -> 'c option)
                    -> ('a map * 'b map)
                    -> 'c map
    val app: ('a -> unit) -> 'a map -> unit
    val appi: ((BasisIdx.t * 'a) -> unit) -> 'a map -> unit
    val map: ('a -> 'b) -> 'a map -> 'b map
    val mapi: (BasisIdx.t * 'a -> 'b) -> 'a map -> 'b map
    val foldl: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli: (BasisIdx.t * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldr: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri: (BasisIdx.t * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val filter: ('a -> bool) -> 'a map -> 'a map
    val filteri: (BasisIdx.t * 'a -> bool) -> 'a map -> 'a map
    val mapPartial: ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali: (BasisIdx.t * 'a -> 'b option) -> 'a map -> 'b map
    val exists: ('a -> bool) -> 'a map -> bool
    val existsi: (BasisIdx.t * 'a -> bool) -> 'a map -> bool
    val all: ('a -> bool) -> 'a map -> bool
    val alli: (BasisIdx.t * 'a -> bool) -> 'a map -> bool
  end =
  struct
    structure BasisIdxKey = struct open BasisIdx type ord_key = t end
    structure M = RedBlackMapFn(BasisIdxKey)
    open M
    type 'a t = 'a map
  end

  (* ====================================================================== *)

  type weight = Complex.t

  type state = (BasisIdx.t * weight) Seq.t

  type t = state

  type qubit_idx = int

  (* ====================================================================== *)

  val empty = Seq.empty ()

  val initial = Seq.singleton (BasisIdx.zeros, Complex.real 1.0)

  fun toString {numQubits} state =
    let
      fun element (bidx, weight) =
        BasisIdx.toString {numQubits = numQubits} bidx ^ " "
        ^ Complex.toString weight ^ "\n"
    in
      Seq.iterate op^ "" (Seq.map element state)
    end

  (* ====================================================================== *)

  fun toMap state =
    Seq.reduce (BasisIdxMap.unionWith Complex.+) BasisIdxMap.empty
      (Seq.map BasisIdxMap.singleton state)

  fun fromMap statemap =
    Seq.fromList (BasisIdxMap.listItemsi statemap)

  fun merge (state1, state2) =
    fromMap (BasisIdxMap.unionWith Complex.+ (toMap state1, toMap state2))

  fun compact state =
    Seq.filter (fn (_, w) => Complex.isNonZero w) (fromMap (toMap state))

  fun reportActive state = compact state

  (* ====================================================================== *)

  fun pauliy state qi =
    let
      fun f (bidx, weight) =
        let
          val bidx' = BasisIdx.flip bidx qi
          val multiplier =
            if BasisIdx.get bidx qi then Complex.imag ~1.0 else Complex.imag 1.0
          val weight' = Complex.* (weight, multiplier)
        in
          (bidx', weight')
        end
    in
      Seq.map f state
    end


  fun pauliz state qi =
    let
      fun f (bidx, weight) =
        let
          val multiplier =
            if BasisIdx.get bidx qi then Complex.real ~1.0 else Complex.real 1.0
          val weight' = Complex.* (weight, multiplier)
        in
          (bidx, weight')
        end
    in
      Seq.map f state
    end


  fun hadamard state qi =
    let
      fun f (bidx, weight) =
        let
          val bidx1 = bidx
          val bidx2 = BasisIdx.flip bidx qi

          val multiplier1 =
            if BasisIdx.get bidx qi then
              Complex.~ (Complex.real Constants.RECP_SQRT_2)
            else
              Complex.real Constants.RECP_SQRT_2

          val multiplier2 = Complex.real Constants.RECP_SQRT_2

          val weight1 = Complex.* (weight, multiplier1)
          val weight2 = Complex.* (weight, multiplier2)
        in
          Seq.fromList [(bidx1, weight1), (bidx2, weight2)]
        end
    in
      Seq.flatten (Seq.map f state)
    end


  fun cx state {control = ci, target = ti} =
    let
      fun f (bidx, weight) =
        let
          val bidx' =
            if BasisIdx.get bidx ci then BasisIdx.flip bidx ti else bidx
        in
          (bidx', weight)
        end
    in
      Seq.map f state
    end

end
