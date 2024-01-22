functor SparseStateTable (structure B: BASIS_IDX
                           structure C: COMPLEX):
  SPARSE_STATE_TABLE =
struct
  structure B = B
  structure C = C
  structure R = struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type t = { keys: B.t array,
             amps: C.r array,
             emptykey: B.t }

  exception Full
  exception DuplicateKey

  (*val fromLarge = *)

  type table = t

  fun make' {capacity, emptykey} =
      let val zero = R.fromLarge 0.0 in
        { keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey),
          amps = SeqBasis.tabulate 5000 (0, 2 * capacity) (fn _ => zero),
          emptykey = emptykey }
      end


  fun checkSpaceForEmptyKey numQubits =
    numQubits >= 0
    andalso
    (case B.maxNumQubits of
       NONE => true
     | SOME limit => numQubits <= limit - 1)


  fun make {capacity, numQubits} =
    if capacity = 0 then
      raise Fail "SparseStateTable.make: capacity 0"
    else if not (checkSpaceForEmptyKey numQubits) then
      raise Fail "SparseStateTable.make: cannot construct empty key"
    else
      make' { capacity = capacity,
              emptykey = B.set B.zeros numQubits }

  fun capacity {keys = keys, ...} = Array.length keys

  fun keyIsEmpty {emptykey, ...} k = B.equal (k, emptykey)
  fun keyIdxIsEmpty {keys, emptykey, ...} i = B.equal (Array.sub (keys, i), emptykey)
  val ampIsZero = C.isZero
  fun ampIdxIsZero {amps, ...} i =
      C.realIsZero (Array.sub (amps, 2 * i)) andalso
      C.realIsZero (Array.sub (amps, 2 * i + 1))
  fun slotEmpty table i = keyIdxIsEmpty table i orelse ampIdxIsZero table i

  fun keyAt {keys, ...} i = Array.sub (keys, i)
  fun ampAt {amps, ...} i = C.make (Array.sub (amps, 2 * i), Array.sub (amps, 2 * i + 1))
  fun kvAt table i = (keyAt table i, ampAt table i)

  (*fun size table =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity table)
        (fn i => if keyIdxIsEmpty table i then 0 else 1)*)

  fun size table =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity table)
        (fn i => if slotEmpty table i then 0 else 1)

  fun sizeInclZeroAmp table =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity table)
        (fn i => if keyIdxIsEmpty table i then 0 else 1)

  fun unsafeViewContents table =
    DelayedSeq.tabulate
      (fn i => if slotEmpty table i then NONE else SOME (kvAt table i))
      (capacity table)

  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun atomicAdd (arr: C.r array) i x =
    let val old = Array.sub (arr, i)
        val new = R.+ (old, x) in
      if bcas (arr, i, old, new) then () else atomicAdd arr i x
    end

  fun insert' {probes = tolerance, forceUnique, add} table (x, y) =
    let val n = capacity table
        val amps = #amps table
        val keys = #keys table

        fun insertAmp i =
            let val (yre, yim) = C.view y in
              if add then
                (atomicAdd amps (2 * i) yre;
                 atomicAdd amps (2 * i + 1) yim)
              else
                (Array.update (amps, 2 * i, yre);
                 Array.update (amps, 2 * i + 1, yim))
            end
        
        fun loop i probes =
          if probes >= tolerance then
            raise Full
          else if i >= n then
            loop 0 probes
          else
            let val k = keyAt table i in
              if keyIsEmpty table k then
                if bcas (keys, i, k, x) then insertAmp i else loop i probes
              else if B.equal (k, x) then
                if forceUnique then raise DuplicateKey else insertAmp i
              else
                loop (i + 1) (probes + 1)
            end
      val start = (B.hash x) mod n
    in
      loop start 0
    end

  fun insert table x =
    insert' {probes = capacity table, forceUnique = false, add = false} table x

  fun insertForceUnique table x =
    insert' {probes = capacity table, forceUnique = true, add = false} table x

  fun insertLimitProbes {probes = tolerance} table x =
    insert' {probes = tolerance, forceUnique = false, add = false} table x

  fun insertAndAdd table x =
    insert' {probes = capacity table, forceUnique = false, add = true} table x

  fun lookup table x =
    let val n = capacity table
        val start = (B.hash x) mod n

        fun loop i =
          let val k = keyAt table i in
            if keyIsEmpty table k then NONE
            else if B.equal (k, x) then SOME (ampAt table i)
            else loopCheck (i + 1)
          end

        and loopCheck i =
          if i = start then NONE else if i >= n then loopCheck 0 else loop i
    in
      if n = 0 then NONE else loop start
    end

  fun sub (table: t) (i: int) =
      if slotEmpty table i then NONE else SOME (kvAt table i)

  fun compact table =
    let val keepers = SeqBasis.filter 5000 (0, capacity table)
                                      (fn i => i) (not o slotEmpty table) in
      DelayedSeq.tabulate
        (fn i => kvAt table (Array.sub (keepers, i)))
        (Array.length keepers)
    end

  fun compactKeys table =
    let val keepers = SeqBasis.filter 5000 (0, capacity table)
                                      (fn i => i) (not o slotEmpty table) in
      DelayedSeq.tabulate
        (fn i => keyAt table (Array.sub (keepers, i)))
        (Array.length keepers)
    end


  fun increaseCapacityByFactor alpha table =
    let val newCap = Real.ceil (alpha * Real.fromInt (capacity table))
        val newTable = make' {capacity = newCap,
                              emptykey = #emptykey table}
    in
      ForkJoin.parfor 1000 (0, capacity table)
                      (fn i => if slotEmpty table i then ()
                               else insertForceUnique newTable (kvAt table i));
      newTable
    end

  val GRAIN = 1000

  structure SSS = SparseStateSet (structure B = B)

  fun fromSet (set: SSS.t) (amp: B.t -> C.t) =
    let val keys = #keys set
        val emptykey = #emptykey set
        val cap = SSS.capacity set
        val zero = R.fromLarge 0.0
        val arr = SeqBasis.tabulate GRAIN (0, 2 * cap) (fn _ => zero)
        fun put i x y = (Array.update (arr, 2 * i, x);
                         Array.update (arr, 2 * i + 1, y))
        fun pararr i =
            let val k = Array.sub (keys, i) in
              if B.equal (k, emptykey) then
                ()
              else
                let val (re, im) = C.view (amp k) in
                  put i re im
                end
            end
        val _ = ForkJoin.parfor GRAIN (0, cap) pararr

        val table = {
          keys = keys,
          amps = arr,
          emptykey = emptykey
        }
    in
      table
    end

  fun fromSetWith (set: SSS.t) (amp: B.t -> C.t * 'a) (join: 'a * 'a -> 'a) (base: 'a) =
    let val keys = #keys set
        val emptykey = #emptykey set
        val cap = SSS.capacity set
        val zero = R.fromLarge 0.0
        (* TODO: use ForkJoin.alloc for amps, then put zero zero below if emptykey *)
        val amps = ForkJoin.alloc (2 * cap)
        fun put i a x y = (Array.update (amps, 2 * i, x);
                           Array.update (amps, 2 * i + 1, y);
                           a)
        fun f i =
            let val k = Array.sub (keys, i) in
              if B.equal (k, emptykey) then
                put i base zero zero
              else
                let val (c, a) = amp k
                    val (re, im) = C.view c in
                  put i a re im
                end
            end
        val a = SeqBasis.reduce GRAIN join base (0, cap) f

        val table = {
          keys = keys,
          amps = amps,
          emptykey = emptykey
        }
    in
      (table, a)
    end

  fun toSet {keys, amps, emptykey} = { keys = keys, emptykey = emptykey }

  fun singleton {numQubits} x =
    let val table = make {capacity = 1, numQubits = numQubits} in
      insertForceUnique table x;
      table
    end

end
