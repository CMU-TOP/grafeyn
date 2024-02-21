signature HASH_TABLE =
sig
  type K
  val emptykey: K
  val eqkey: K * K -> bool
  val hash: K -> int
  type V
  type VArray
  val emptyval: V
  val eqval: V * V -> bool
  val valloc: int -> VArray
  val vget: (VArray * int) -> V
  val vset: (VArray * int * V) -> unit
  val vmod: (VArray * int * V) -> unit

  type t
  type table = t

  exception Full
  exception DuplicateKey (* only raised by forceInsertUnique *)

  val make: {capacity: int} -> table
  val defaultPadding: int -> int
  val singleton: (K * V) -> table

  val size: table -> int
  val capacity: table -> int
  val nonEmptyKeys: table -> int

  val insert: table -> (K * V) -> unit
  val insertLimitProbes: {probes: int} -> table -> (K * V) -> unit
  val insertForceUnique: table -> (K * V) -> unit
  val update: table -> (K * V) -> unit

  val contains: (table * K) -> bool

  (* not safe for concurrency with insertions *)
  val lookup: (table * K) -> V option
  val lookupElse: (table * K * V) -> V

  val sub: (table * int) -> (K * V) option

  val compact: table -> (K * V) DelayedSeq.t
  val compactKeys: table -> K DelayedSeq.t

  val increaseCapacityByFactor: table -> real -> table
  val resize: table -> int -> table

  val fromKeys: (K array * (K -> V)) -> table
  val fromKeysWith: (K array * (K -> V * 'a)) -> ('a * ('a * 'a -> 'a)) -> table * 'a
  val toKeys: table -> K array

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: table -> (K * V) option DelayedSeq.t

  val reset: table -> unit
  
  val estimateSize : {subsection: int } -> table -> int
  val estimateSize': {subsection: real, minsection: int} -> table -> int
end

functor HashTable (type K
                   val emptykey: K
                   val eqkey: K * K -> bool
                   val hash: K -> int
                   type V
                   type VArray
                   val emptyval: V
                   val eqval: V * V -> bool
                   val valloc: int -> VArray
                   val vget: (VArray * int) -> V
                   val vset: (VArray * int * V) -> unit
                   val vmod: (VArray * int * V) -> unit):
  HASH_TABLE =
struct
  type K = K
  val emptykey = emptykey
  val eqkey = eqkey
  val hash = hash
  type V = V
  type VArray = VArray
  val emptyval = emptyval
  val eqval = eqval
  val valloc = valloc
  val vget = vget
  val vset = vset
  val vmod = vmod

  (*structure R = struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end*)

  type t = { keys: K array, vals: VArray }
  type table = t

  exception Full
  exception DuplicateKey

  fun make {capacity} =
    if capacity = 0 then
      raise Fail "SparseStateTable.make: capacity 0"
    else
      { keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey),
        vals = valloc capacity }

  fun defaultPadding x = (5 * x) div 4

  fun capacity {keys = keys, ...} = Array.length keys

  fun kempty k = eqkey (k, emptykey)
  fun vempty v = eqval (v, emptyval)

  fun ksub {keys, vals} i = Array.sub (keys, i)
  fun vsub {keys, vals} i = vget (vals, i)
  fun kvsub table i = (ksub table i, vsub table i)

  fun kxempty table i = kempty (ksub table i)
  fun vxempty table i = vempty (vsub table i)
  fun slotEmpty table i = kxempty table i orelse vxempty table i

  fun size table =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity table)
        (fn i => if slotEmpty table i then 0 else 1)

  fun nonEmptyKeys table =
      SeqBasis.reduce
        10000 op+ 0 (0, capacity table)
        (fn i => if kxempty table i then 0 else 1)

  fun unsafeViewContents table =
    DelayedSeq.tabulate
      (fn i => if slotEmpty table i then NONE else SOME (kvsub table i))
      (capacity table)

  fun insert' {probes = tolerance, forceUnique} vins (table as {keys, vals}) (x, y) =
    let val n = capacity table
        
        fun loop i probes =
          if probes >= tolerance then
            raise Full
          else if i >= n then
            loop 0 probes
          else
            let val k = ksub table i in
              if kempty k then
                if Helpers.bcas (keys, i, k, x) then vset (vals, i, y) else loop i probes
              else if eqkey (k, x) then
                if forceUnique then raise DuplicateKey else vins (vals, i, y)
              else
                loop (i + 1) (probes + 1)
            end

      val start = (hash x) mod n
    in
      loop start 0
    end

  fun insert table kv = insert' {probes = capacity table, forceUnique = false} vset table kv

  fun insertForceUnique table kv  = insert' {probes = capacity table, forceUnique = true} vset table kv

  fun insertLimitProbes {probes = tolerance} =
      insert' {probes = tolerance, forceUnique = false} vset

  fun singleton (x, y) =
    let val table = make {capacity = 1} in
      insertForceUnique table (x, y);
      table
    end

  fun update table kv =
      insert' {probes = capacity table, forceUnique = false} vmod table kv

  fun lookup (table, x) =
    let val n = capacity table
        val start = (hash x) mod n

        fun loop i =
          let val k = ksub table i in
            if kempty k then NONE
            else if eqkey (k, x) then SOME (vsub table i)
            else loopCheck (i + 1)
          end

        and loopCheck i =
          if i = start then NONE else if i >= n then loopCheck 0 else loop i
    in
      if n = 0 then NONE else loop start
    end

  fun lookupElse (table, x, default) =
      case lookup (table, x) of
          NONE => default
        | SOME y => y

  fun contains x =
      case lookup x of
          NONE => false
        | SOME _ => true


  fun sub (table, i) =
      if slotEmpty table i then NONE else SOME (kvsub table i)

  fun compact table =
    let val keepers = SeqBasis.filter 5000 (0, capacity table)
                                      (fn i => i) (not o slotEmpty table) in
      DelayedSeq.tabulate
        (fn i => kvsub table (Array.sub (keepers, i)))
        (Array.length keepers)
    end

  fun compactKeys table =
    let val keepers = SeqBasis.filter 5000 (0, capacity table)
                                      (fn i => i) (not o slotEmpty table) in
      DelayedSeq.tabulate
        (fn i => ksub table (Array.sub (keepers, i)))
        (Array.length keepers)
    end

  fun resize table newCap =
      let val newTable = make {capacity = newCap} in
        ForkJoin.parfor 1000 (0, capacity table)
                        (fn i => if slotEmpty table i then ()
                                 else insertForceUnique newTable (kvsub table i));
        newTable
      end

  fun increaseCapacityByFactor table alpha =
      resize table (Real.ceil (alpha * Real.fromInt (capacity table)))

  fun fromKeys (keys, f) =
    let val cap = Array.length keys
        val varr = valloc (defaultPadding cap)
        fun pararr i =
            let val k = Array.sub (keys, i) in
              if kempty k then
                ()
              else
                vset (varr, i, f k)
            end
        val _ = ForkJoin.parfor 1000 (0, cap) pararr
    in
      { keys = keys, vals = varr }
    end

  fun fromKeysWith (keys, f) (base, join) =
    let val cap = Array.length keys
        val varr = valloc (defaultPadding cap)
        fun pararr i =
            let val k = Array.sub (keys, i) in
              if kempty k then
                base
              else
                let val (v, a) = f k in
                  (vset (varr, i, v); a)
                end
            end
        val a = SeqBasis.reduce 1000 join base (0, cap) pararr
    in
      ({ keys = keys, vals = varr }, a)
    end

  fun toKeys {keys, vals} = keys

  fun reset {keys, vals} = Helpers.resetArray (keys, emptykey)

  fun estimateSize' {subsection, minsection} table =
      let val cap = capacity table
          val rcap = Real.fromInt cap
          val subs = Int.max (minsection, Real.round (subsection * rcap)) in
        if subs < cap then
          Real.round
            (Real.fromInt (SeqBasis.reduce
                             5000 op+ 0 (0, subs)
                             (fn i => if slotEmpty table i then 0 else 1))
             * rcap / Real.fromInt subs)
        else
          size table
      end

  fun estimateSize {subsection} table =
      estimateSize' {subsection = 0.0, minsection = subsection} table
end

functor SparseStateSet (structure B: BASIS_IDX
                        val numQubits: int) =
  HashTable
    (type K = B.t
     val emptykey = B.set B.zeros numQubits
     val eqkey = B.equal
     val hash = B.hash
     type V = unit
     type VArray = unit
     val emptyval = ()
     val eqval = fn _ => false
     val valloc = fn _ => ()
     val vget = fn _ => ()
     val vset = fn _ => ()
     val vmod = fn _ => ())

functor SparseStateTable (structure B: BASIS_IDX
                          structure C: COMPLEX
                          val numQubits: int) =
  HashTable
    (type K = B.t
     val emptykey = B.set B.zeros numQubits
     val eqkey = B.equal
     val hash = B.hash
     type V = C.t
     type VArray = C.r array
     val emptyval = C.zero
     val eqval = C.isZero o C.-
     val valloc = fn i => ForkJoin.alloc (2 * i)
     val vget = (fn (arr, i) => C.make (Array.sub (arr, 2 * i), Array.sub (arr, 2 * i + 1)))
     val vset = (fn (arr, i, v) =>
                   let val (re, im) = C.view v in
                     Array.update (arr, 2 * i, re);
                     Array.update (arr, 2 * i + 1, im)
                   end)
    val vmod = fn (arr, i, v) =>
                  let val (re, im) = C.view v in
                    Helpers.atomicUpdate (arr, 2 * i, re, C.R.+);
                    Helpers.atomicUpdate (arr, 2 * i + 1, im, C.R.+)
                  end)

(* Counts the number of times a basis idx is touched *)
functor SparseStateCounter (structure B: BASIS_IDX
                            val numQubits: int) =
  HashTable
    (type K = B.t
     val emptykey = B.set B.zeros numQubits
     val eqkey = B.equal
     val hash = B.hash
     type V = int
     type VArray = int array
     val emptyval = 0
     val eqval = op=
     val valloc = ForkJoin.alloc
     val vget = Array.sub
     val vset = Array.update
     val vmod = fn x => (Helpers.fetchAndAdd x; ())) (* TODO: use fetch and add from sml basis library *)
