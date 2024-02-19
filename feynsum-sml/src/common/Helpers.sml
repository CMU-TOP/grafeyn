structure Helpers:
sig
  val riMult: real -> IntInf.int -> IntInf.int
  val log2: real -> real
  val exp2: int -> int
  val divPow2: real -> int -> real
  val forRange: int * int -> (int -> 'a) -> unit
  val bcas: ('a array * int * 'a * 'a) -> bool
  val atomicUpdate: ('a array * int * 'a * ('a * 'a -> 'a)) -> unit
  val fetchAndAdd: (int array * int * int) -> int
  val atomicInsert: ('a array * int * 'a * ('a * 'a -> 'a) option) -> unit
  val sampleArray: Random.rand -> 'a array -> int -> 'a array
  val resetArray: 'a array * 'a -> unit
  val eqArray: 'a array * 'a array -> ('a * 'a -> bool) -> bool
  val copyArray: 'a array -> 'a array
end =
struct

  (* r / 2^n *)
  fun divPow2 r n = if n <= 0 then r else divPow2 (r / 2.0) (n - 1)
  fun exp2 n = let fun h x n = if n <= 0 then x else h (x + x) (n - 1) in h 1 n end

  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun atomicUpdate (arr, i, a, join) =
    let val old = Array.sub (arr, i)
        val new = join (old, a) in
      if bcas (arr, i, old, new) then () else atomicUpdate (arr, i, a, join)
    end

  fun fetchAndAdd (arr, i, x) =
    let val old = Array.sub (arr, i)
        val new = old + x in
      if bcas (arr, i, old, new) then new else fetchAndAdd (arr, i, x)
    end

  fun atomicInsert (arr, i, a, join) =
      case join of
          NONE => Array.update (arr, i, a)
        | SOME f => atomicUpdate (arr, i, a, f)

  (* 0 < r < 1
   *
   * I wish this wasn't so difficult
   *
   * The problem is that I can't always convert the IntInf into a real,
   * because it might be too large.
   *)
  fun riMult (r: real) (i: IntInf.int) : IntInf.int =
    if IntInf.abs i <= 1000000000000 then
      Real.toLargeInt IEEEReal.TO_NEAREST
        (r * Real.fromLargeInt (IntInf.toLarge i))
    else
      let
        val digits = Real.fmt StringCvt.EXACT r
        val digits =
          if String.isPrefix "0." digits then String.extract (digits, 2, NONE)
          else raise Fail "riMult: uh oh"

        fun loop acc depth =
          if depth >= String.size digits then
            acc
          else
            let
              val d = Char.ord (String.sub (digits, depth)) - Char.ord #"0"
              val _ =
                if 0 <= d andalso d <= 9 then ()
                else raise Fail ("riMult: bad digit " ^ digits ^ ", " ^ Real.toString r ^ ", " ^ IntInf.toString i)
              val acc =
                acc + (i * IntInf.fromInt d) div (IntInf.pow (10, depth + 1))
            in
              loop acc (depth + 1)
            end
      in
        loop 0 0
      end


  fun log2 x = Math.log10 x / Math.log10 2.0

  fun forRange (range: int * int) (f: int -> 'a) =
      let val (s, e) = range
          fun iter i = if i < e then (f i; iter (i + 1)) else () in
        iter s
      end

  (* Selects a random k-size subset of an n-length array in O(k).
   * This modifies the array in-place. *)
  fun sampleArray (seed: Random.rand) (xs: 'a array) (k: int) =
      if k >= Array.length xs then
        xs
      else
        let val len = Array.length xs
            (*val seed' = Util.hash seed*)
            fun swapAndRet i =
                let (*val j = i + (Util.hash (i + seed') mod (len - 1 - i))*)
                    val j = Random.randRange (i, len - 1) seed
                    val xi = Array.sub (xs, i)
                    val xj = Array.sub (xs, j) in
                  Array.update (xs, j, xi); xj
                end
        in
          Array.tabulate (k, swapAndRet)
        end

  (*fun sampleArray (xs: 'a array) (k: int) =
      Array.tabulate (Int.min (k, Array.length xs), fn i => Array.sub (xs, i))*)
  (*fun sampleArray (xs: 'a array) (k: int) =
      let val len = Array.length xs in
        if k >= len then
          xs
        else
          SeqBasis.tabulate 1000 (0, k)
                            (fn i => Array.sub (xs, Random.randRange (0, len - 1) seed))
      end*)

  (* Randomly selects k distinct elements from array xs *)
  (*fun sampleArray (xs: 'a array) (k: int) =
      if k >= Array.length xs then
        xs
      else
        let val len = Array.length xs
            fun swapAndRet i j =
                let val xi = Array.sub (xs, i)
                    val xj = Array.sub (xs, j) in
                  Array.update (xs, i, xj);
                  Array.update (xs, j, xi);
                  xj
                end
        in
          Array.tabulate (k, fn i => swapAndRet i (Random.randRange (i, len - 1) seed))
        end*)

  fun resetArray (arr, x) =
      ForkJoin.parfor
        5000
        (0, Array.length arr)
        (fn i => Array.update (arr, i, x))

  fun eqArray (arr1, arr2) eq =
      Array.length arr1 = Array.length arr2 andalso
      SeqBasis.reduce
        5000
        (fn (a, b) => a andalso b)
        true
        (0, Array.length arr1)
        (fn i => eq (Array.sub (arr1, i), Array.sub (arr2, i)))

  fun copyArray arr =
      SeqBasis.tabulate
        5000
        (0, Array.length arr)
        (fn i => Array.sub (arr, i))

end
