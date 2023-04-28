structure SparseStateTable :>
sig
  type t
  type table = t

  exception Full

  val make: {capacity: int, maxload: real, emptykey: BasisIdx.t} -> table

  val size: table -> int
  val capacity: table -> int

  (* val insertIfNotPresent: table -> BasisIdx.t * Complex.t -> bool *)

  val insertAddWeights: table -> BasisIdx.t * Complex.t -> unit

  (* not safe for concurrency with insertions *)
  val lookup: table -> BasisIdx.t -> Complex.t option

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


  fun insertAddWeights (input as T {keys, packedWeights, emptykey, maxload})
    (x, v) =
    let
      val n = Array.length keys
      val tolerance = 20 * Real.ceil (1.0 / (1.0 - maxload))

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
            val goodSlot =
              (BasisIdx.equal (k, emptykey) andalso claimSlotAt i)
              orelse BasisIdx.equal (k, x)
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

end
