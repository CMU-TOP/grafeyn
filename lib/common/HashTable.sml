structure HashTable :>
sig
  type ('a, 'b) t
  type ('a, 'b) table = ('a, 'b) t

  exception Full

  val make: {hash: 'a -> int, eq: 'a * 'a -> bool, capacity: int, maxload: real}
            -> ('a, 'b) table

  val size: ('a, 'b) table -> int
  val capacity: ('a, 'b) table -> int
  val resize: ('a, 'b) table -> ('a, 'b) table
  val insertIfNotPresent: ('a, 'b) table -> 'a * 'b -> bool
  val insertWith: ('b * 'b -> 'b) -> ('a, 'b) table -> 'a * 'b -> unit
  val compact: ('a, 'b) table -> ('a * 'b) Seq.t

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *)
  val unsafeViewContents: ('a, 'b) table -> ('a * 'b) option Seq.t
end =
struct

  datatype ('a, 'b) t =
    T of
      { data: ('a * 'b) option array
      , hash: 'a -> int
      , eq: 'a * 'a -> bool
      , maxload: real
      }

  exception Full

  type ('a, 'b) table = ('a, 'b) t

  fun make {hash, eq, capacity, maxload} =
    let val data = SeqBasis.tabulate 5000 (0, capacity) (fn _ => NONE)
    in T {data = data, hash = hash, eq = eq, maxload = maxload}
    end

  fun unsafeViewContents (T {data, ...}) = ArraySlice.full data

  fun bcas (arr, i) (old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun size (T {data, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length data) (fn i =>
      if Option.isSome (Array.sub (data, i)) then 1 else 0)

  fun capacity (T {data, ...}) = Array.length data


  fun insertWith combine (input as T {data, hash, eq, maxload}) (x, v) =
    let
      val n = Array.length data
      val start = (hash x) mod (Array.length data)

      val tolerance = 20 * Real.ceil (1.0 / (1.0 - maxload))

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val current = Array.sub (data, i)
          in
            case current of
              SOME (y, v') =>
                if not (eq (x, y)) then
                  loop (i + 1) (probes + 1)
                else if bcas (data, i) (current, SOME (x, combine (v, v'))) then
                  ()
                else
                  (* just try again *)
                  loop i probes
            | NONE =>
                if bcas (data, i) (current, SOME (x, v)) then ()
                else loop i probes
          end

      val start = (hash x) mod (Array.length data)
    in
      loop start 0
    end


  fun insertIfNotPresent' (input as T {data, hash, eq, maxload}) (x, v) force =
    let
      val n = Array.length data
      val start = (hash x) mod (Array.length data)

      val tolerance = 2 * Real.ceil (1.0 / (1.0 - maxload))

      fun loop i probes =
        if not force andalso probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val current = Array.sub (data, i)
          in
            case current of
              SOME (y, _) =>
                if eq (x, y) then false else loop (i + 1) (probes + 1)
            | NONE =>
                if bcas (data, i) (current, SOME (x, v)) then
                  (* (Concurrency.faa sz 1; true) *)
                  true
                else
                  loop i probes
          end

      val start = (hash x) mod (Array.length data)
    in
      loop start 0
    end


  fun insertIfNotPresent s x =
    insertIfNotPresent' s x false


  fun resize (input as T {data, hash, eq, maxload}) =
    let
      val newcap = 2 * capacity input
      val new = make
        {hash = hash, eq = eq, capacity = newcap, maxload = maxload}
    in
      ForkJoin.parfor 1000 (0, Array.length data) (fn i =>
        case Array.sub (data, i) of
          NONE => ()
        | SOME x => (insertIfNotPresent' new x true; ()));

      new
    end


  fun compact (T {data, ...}) =
    ArraySlice.full (SeqBasis.tabFilter 2000 (0, Array.length data) (fn i =>
      Array.sub (data, i)))

end
