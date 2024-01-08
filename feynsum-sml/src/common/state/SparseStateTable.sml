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
    { keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey),
      amps = SeqBasis.tabulate 5000 (0, 2 * capacity) (fn _ => R.fromLarge 0.0),
      emptykey = emptykey }


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

  fun unsafeViewContents table =
    DelayedSeq.tabulate
      (fn i => if slotEmpty table i then NONE else SOME (kvAt table i))
      (capacity table)

  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun insert' {probes = tolerance, forceUnique} table (x, y) =
    let val n = capacity table

        fun insertAmp i =
            let val (yre, yim) = C.view y in
              Array.update (#amps table, 2 * i, yre);
              Array.update (#amps table, 2 * i + 1, yim)
            end
        
        fun loop i probes =
          if probes >= tolerance then
            raise Full
          else if i >= n then
            loop 0 probes
          else
            let val k = keyAt table i in
              if keyIsEmpty table k then
                if bcas (#keys table, i, k, x) then insertAmp i else loop i probes
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
    insert' {probes = capacity table, forceUnique = false} table x

  fun insertForceUnique table x =
    insert' {probes = capacity table, forceUnique = true} table x

  fun insertLimitProbes {probes = tolerance} table x =
    insert' {probes = tolerance, forceUnique = false} table x

  (*fun insertAndAdd {probes = tolerance} table x =
    insert' {probes = tolerance, join = C.+} table x*)


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

  fun compact table =
    let val keepers = SeqBasis.filter 5000 (0, capacity table)
                                      (fn i => i) (not o slotEmpty table) in
      DelayedSeq.tabulate
        (fn i => kvAt table (Array.sub (keepers, i)))
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

  (* Converts an array A_c of complex numbers into an
   * array A_r of reals with twice its length, such
   * that A_c[i] = C.make (A_r[2*i], A_r[2*i+1]) *)
  fun arrayComplexToReals a_c =
      SeqBasis.tabulate 1000 (0, 2 * Array.length a_c)
                        (fn i => let val (re, im) = C.view (Array.sub (a_c, i div 2)) in
                                   if i mod 2 = 0 then re else im
                                 end)

  structure SSS = SparseStateSet (structure B = B)
  fun fromKeys (set: SSS.t) (amp: B.t -> C.t) =
      let val cap = SSS.capacity set
          val amps = SeqBasis.tabulate
                      1000 (0, cap)
                      (fn i => let val k = Array.sub (#keys set, i) in
                                 if B.equal (k, #emptykey set) then C.zero else amp k end)
      in
        { keys = #keys set,
          amps = arrayComplexToReals amps,
          emptykey = #emptykey set }
      end

  fun fromKeysWith (set: SSS.t) (amp: B.t -> C.t * 'a) =
    let val keys = #keys set
        val emptykey = #emptykey set
        val cap = SSS.capacity set
        val arr = SeqBasis.tabulate
                      1000 (0, cap)
                      (fn i => let val k = Array.sub (#keys set, i) in
                                 if B.equal (k, #emptykey set) then NONE else SOME (amp k) end)
        val seq = Seq.mapOption (Option.map (fn (c, a) => a))
                                (Seq.tabulate (fn i => Array.sub (arr, i)) cap)
        val amps = SeqBasis.tabulate
                     1000 (0, cap)
                     (fn i => case Array.sub (arr, i) of NONE => C.zero | SOME (c, _) => c)
        val table = {
          keys = keys,
          amps = arrayComplexToReals amps,
          emptykey = emptykey
        }
    in
      (table, seq)
    end

  fun singleton {numQubits} x =
    let val table = make {capacity = 1, numQubits = numQubits} in
      insertForceUnique table x;
      table
    end

end
