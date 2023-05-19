structure SparseStateTable :>
sig
  type t
  type table = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int, maxload: real, emptykey: BasisIdx.t} -> table

  val size: table -> int
  val nonZeroSize: table -> int
  val capacity: table -> int

  (* val insertIfNotPresent: table -> BasisIdx.t * Complex.t -> bool *)

  val insertAddWeights: table -> BasisIdx.t * Complex.t -> unit
  val forceInsertUnique: table -> BasisIdx.t * Complex.t -> unit

  (* not safe for concurrency with insertions *)
  val lookup: table -> BasisIdx.t -> Complex.t option

  val compact: table -> (BasisIdx.t * Complex.t) DelayedSeq.t

  val increaseCapacityByFactor: real -> table -> table

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: table -> (BasisIdx.t * Complex.t) option DelayedSeq.t
end =
struct

  datatype t =
    T of
      { keys: BasisIdx.t array
      , emptykey: BasisIdx.t
      , packedWeights: real array (* 2x capacity, for manual unboxing *)
      , maxload: real
      }

  exception Full
  exception DuplicateKey

  type table = t

  fun make {capacity, maxload, emptykey} =
    if capacity = 0 then
      raise Fail "SparseStateTable.make: capacity 0"
    else
      let
        val keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey)
        val packedWeights =
          SeqBasis.tabulate 5000 (0, 2 * capacity) (fn _ => 0.0)
      in
        T { keys = keys
          , maxload = maxload
          , emptykey = emptykey
          , packedWeights = packedWeights
          }
      end


  fun capacity (T {keys, ...}) = Array.length keys


  fun maxload (T {maxload = m, ...}) = m


  fun size (T {keys, emptykey, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length keys) (fn i =>
      if BasisIdx.equal (Array.sub (keys, i), emptykey) then 0 else 1)


  fun unsafeViewContents (T {keys, packedWeights, emptykey, ...}) =
    let
      fun makeWeight i =
        Complex.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun elem i =
        let val k = Array.sub (keys, i)
        in if BasisIdx.equal (k, emptykey) then NONE else SOME (k, makeWeight i)
        end
    in
      DelayedSeq.tabulate elem (Array.length keys)
    end


  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))


  fun atomicAdd (arr: real array) i x =
    let
      val old = Array.sub (arr, i)
      val new = old + x
    in
      if bcas (arr, i, old, new) then () else atomicAdd arr i x
    end


  fun nonAtomicAdd (arr: real array) i x =
    Array.update (arr, i, x + Array.sub (arr, i))


  fun insertAddWeights (input as T {keys, packedWeights, emptykey, maxload})
    (x, v) =
    let
      val n = Array.length keys
      val tolerance = 100 * Real.ceil (1.0 / (1.0 - maxload))

      fun claimSlotAt i = bcas (keys, i, emptykey, x)

      fun putValueAt i =
        let
          val (re, im) = Complex.view v
        in
          atomicAdd packedWeights (2 * i) re;
          atomicAdd packedWeights (2 * i + 1) im
        end

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val k = Array.sub (keys, i)
          in
            if BasisIdx.equal (k, emptykey) then
              if claimSlotAt i then putValueAt i else loop i probes
            else if BasisIdx.equal (k, x) then
              putValueAt i
            else
              loop (i + 1) (probes + 1)
          end

      val start = (BasisIdx.hash x) mod (Array.length keys)
    in
      loop start 0
    end


  fun forceInsertUnique (T {keys, packedWeights, emptykey, maxload}) (x, v) =
    let
      val n = Array.length keys
      val start = (BasisIdx.hash x) mod n

      fun claimSlotAt i = bcas (keys, i, emptykey, x)

      fun putValueAt i =
        let
          val (re, im) = Complex.view v
        in
          nonAtomicAdd packedWeights (2 * i) re;
          nonAtomicAdd packedWeights (2 * i + 1) im
        end

      fun loop i =
        if i >= n then
          loop 0
        else
          let
            val k = Array.sub (keys, i)
          in
            if BasisIdx.equal (k, emptykey) then
              if claimSlotAt i then putValueAt i else loop i
            else if BasisIdx.equal (k, x) then
              raise DuplicateKey
            else
              loopNext (i + 1)
          end

      and loopNext i =
        if i = start then raise Full else loop i
    in
      loop start
    end


  fun lookup (T {keys, packedWeights, emptykey, ...}) x =
    let
      val n = Array.length keys
      val start = (BasisIdx.hash x) mod n

      fun makeWeight i =
        Complex.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun loop i =
        let
          val k = Array.sub (keys, i)
        in
          if BasisIdx.equal (k, emptykey) then NONE
          else if BasisIdx.equal (k, x) then SOME (makeWeight i)
          else loopCheck (i + 1)
        end

      and loopCheck i =
        if i >= n then loopCheck 0 else if i = start then NONE else loop i
    in
      if n = 0 then NONE else loop start
    end


  fun nonZeroSize state =
    let
      val currentElems = unsafeViewContents state
    in
      SeqBasis.reduce 1000 op+ 0 (0, DelayedSeq.length currentElems) (fn i =>
        case DelayedSeq.nth currentElems i of
          NONE => 0
        | SOME (bidx, weight) => if Complex.isNonZero weight then 1 else 0)
    end


  fun compact (T {keys, emptykey, packedWeights, ...}) =
    let
      fun makeWeight i =
        Complex.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun makeElem i =
        ( Array.sub (keys, i)
        , Array.sub (packedWeights, 2 * i)
        , Array.sub (packedWeights, 2 * i + 1)
        )

      fun keepElem i =
        not (BasisIdx.equal (Array.sub (keys, i), emptykey))
        andalso Complex.isNonZero (makeWeight i)

      val data = SeqBasis.filter 10000 (0, Array.length keys) makeElem keepElem
    in
      DelayedSeq.tabulate
        (fn i => let val (b, re, im) = Array.sub (data, i)
                 in (b, Complex.make (re, im))
                 end) (Array.length data)
    end


  fun increaseCapacityByFactor alpha (table as T {maxload, emptykey, ...}) =
    let
      val newCap = Real.ceil (alpha * Real.fromInt (capacity table))
      val newTable =
        make {capacity = newCap, maxload = maxload, emptykey = emptykey}

      val elems = compact table
    in
      ForkJoin.parfor 1000 (0, DelayedSeq.length elems) (fn i =>
        forceInsertUnique newTable (DelayedSeq.nth elems i));

      newTable
    end

end
