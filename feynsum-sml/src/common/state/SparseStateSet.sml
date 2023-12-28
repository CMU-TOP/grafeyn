functor SparseStateSet (structure B: BASIS_IDX):
  SPARSE_STATE_SET =
struct
  structure B = B
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type t = { keys: B.t array,
             emptykey: B.t }

  exception Full
  exception DuplicateKey

  type set = t

  fun make' {capacity, emptykey} =
    { keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey),
      emptykey = emptykey }


  fun checkSpaceForEmptyKey numQubits =
    numQubits >= 0
    andalso
    (case B.maxNumQubits of
       NONE => true
     | SOME limit => numQubits <= limit - 1)


  fun make {capacity, numQubits} =
    if capacity = 0 then
      raise Fail "SparseStateSet.make: capacity 0"
    else if not (checkSpaceForEmptyKey numQubits) then
      raise Fail "SparseStateSet.make: cannot construct empty key"
    else
      make' { capacity = capacity,
              emptykey = B.set B.zeros numQubits
            }

  fun capacity {keys = keys, ...} = Array.length keys

  fun keyIsEmpty {emptykey, ...} k = B.equal (k, emptykey)
  fun keyIdxIsEmpty {keys, emptykey, ...} i = B.equal (Array.sub (keys, i), emptykey)

  fun keyAt {keys, ...} i = Array.sub (keys, i)

  fun size set =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity set)
        (fn i => if keyIdxIsEmpty set i then 0 else 1)


  fun unsafeViewContents set =
    DelayedSeq.tabulate
      (fn i => let val k = keyAt set i in
                 if keyIsEmpty set k then NONE else SOME k end)
      (capacity set)


  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun insert' {probes = tolerance, forceUnique = forceUnique} set x =
    let val n = capacity set
        val keys = #keys set
        fun claimSlotAt (k, i) = bcas (keys, i, k, x)
        fun loop i probes =
          if probes >= tolerance then
            raise Full
          else if i >= n then
            loop 0 probes
          else
            let val k = keyAt set i in
              if keyIsEmpty set k then
                if claimSlotAt (k, i) then () else loop i probes
              else if B.equal (k, x) then
                if forceUnique then raise DuplicateKey else ()
              else
                loop (i + 1) (probes + 1)
            end
      val start = (B.hash x) mod n
    in
      loop start 0
    end

  fun insert set x =
    insert' {probes = capacity set, forceUnique = false} set x

  fun insertForceUnique set x =
    insert' {probes = capacity set, forceUnique = true} set x


  fun contains set x =
    let val n = capacity set
        val start = (B.hash x) mod n

        fun loop i =
          let val k = keyAt set i in
            if keyIsEmpty set k then false
            else if B.equal (k, x) then true
            else loopCheck (i + 1)
          end

        and loopCheck i =
          if i >= n then loopCheck 0 else if i = start then false else loop i
    in
      if n = 0 then false else loop start
    end

  fun compact set =
    let val keepers = SeqBasis.filter 5000 (0, capacity set)
                                      (fn i => i) (not o keyIdxIsEmpty set) in
      DelayedSeq.tabulate
        (fn i => keyAt set (Array.sub (keepers, i)))
        (Array.length keepers)
    end


  fun increaseCapacityByFactor alpha set =
    let val newCap = Real.ceil (alpha * Real.fromInt (capacity set))
        val newSet = make' {capacity = newCap,
                            emptykey = #emptykey set}
    in
      ForkJoin.parfor 1000 (0, capacity set) (fn i =>
        let val key = keyAt set i in
          if keyIsEmpty key then () else insertForceUnique newSet key
        end);
      newSet
    end


  fun singleton {numQubits} x =
    let val set = make {capacity = 1, numQubits = numQubits} in
      insertForceUnique set x;
      set
    end

end
