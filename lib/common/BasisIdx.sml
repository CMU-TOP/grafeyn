(* mapping of qubit index to {0,1} *)
structure BasisIdx :>
sig
  type qubit_idx = int
  type t

  val toString: {numQubits: int} -> t -> string

  val zeros: t

  val set: t -> qubit_idx -> bool -> t
  val flip: t -> qubit_idx -> t
  val get: t -> qubit_idx -> bool

  (* not a very meaningful comparison; this is just for storing it in
   * ordered maps and whatever.
   *)
  val compare: t * t -> order
  val equal: t * t -> bool
  val hash: t -> int
end =
struct
  type qubit_idx = int

  type t = Word64.word (* allows up to 64 qubits *)

  val zeros: t = 0w0

  fun selector qi =
    Word64.<< (0w1, Word.fromInt qi)

  fun set bidx qi b =
    let val cleared = Word64.andb (bidx, Word64.notb (selector qi))
    in if b then Word64.orb (cleared, selector qi) else cleared
    end

  fun flip bidx qi =
    Word64.xorb (bidx, selector qi)

  fun get bidx qi =
    0w0 <> (Word64.andb (bidx, selector qi))

  fun toString {numQubits} bidx =
    "|"
    ^
    CharVector.tabulate (numQubits, fn i =>
      if get bidx (numQubits - i - 1) then #"1" else #"0") ^ "⟩"

  fun compare (bidx1: t, bidx2: t) = Word64.compare (bidx1, bidx2)

  fun equal (b1, b2) = b1 = b2

  fun hash bidx =
    Word64.toIntX (Util.hash64_2 bidx)
end
