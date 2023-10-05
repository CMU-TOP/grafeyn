structure ApplyUntilFailure:
sig
  datatype 'a element_result = Success | Failure of 'a

  val doPrefix: {grain: int, acceleration: real}
                -> int * int
                -> (int -> 'a element_result)
                -> {numApplied: int, failed: 'a Seq.t}
end =
struct

  datatype 'a element_result = Success | Failure of 'a

  fun doPrefix {grain, acceleration} (start, stop) doElem =
    let
      fun loop numApplied (lo, hi) =
        if lo >= hi then
          {numApplied = numApplied, failed = Seq.empty ()}
        else
          let
            val resultHere = SeqBasis.tabFilter grain (lo, hi) (fn i =>
              case doElem i of
                Success => NONE
              | Failure x => SOME x)

            val widthHere = hi - lo
            val numApplied' = numApplied + widthHere

            val desiredWidth = Real.ceil (acceleration * Real.fromInt widthHere)
            val lo' = hi
            val hi' = Int.min (hi + desiredWidth, stop)
          in
            if Array.length resultHere = 0 then loop numApplied' (lo', hi')
            else {numApplied = numApplied', failed = ArraySlice.full resultHere}
          end
    in
      loop 0 (start, stop)
    end

end
