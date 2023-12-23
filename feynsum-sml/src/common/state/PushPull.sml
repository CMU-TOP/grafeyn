functor PushPull
  (structure HS: HYBRID_STATE
   structure G: GATE
   sharing HS.B = G.B
   sharing HS.C = G.C
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real) :>
sig
  structure B = HS.B
  structure C = HS.C
  val push: G.t Seq.t * (B, C) HashTable.t -> B HashSet.t
  val pull: G.t Seq.t * (B, C) HashTable.t -> B HashSet.t -> (B, C) HashTable.t
end =
struct

  structure B = HS.B
  structure C = HS.C

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
