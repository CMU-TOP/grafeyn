structure ExpandState :>
sig

  type state = (BasisIdx.t * Complex.t) DelayedSeq.t

  val expand: {gates: Gate.t Seq.t, state: state, expected: int}
              -> SparseStateTable.table

end =
struct

  structure SST = SparseStateTable
  structure DS = DelayedSeq
  type state = (BasisIdx.t * Complex.t) DelayedSeq.t


  val blockSize = CommandLineArgs.parseInt "expand-block-size" 10000
  val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")


  val maxload = CommandLineArgs.parseReal "expand-max-load" 0.9
  val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")


  fun log2 x = Math.log10 x / Math.log10 2.0

  fun tryPut table widx =
    let
      val n = SST.capacity table
      val probablyLongestProbe = Int.max (10, Real.ceil
        (log2 (Real.fromInt n) / (maxload - 1.0 - log2 maxload)))
      val tolerance = 2 * probablyLongestProbe
      val tolerance = Int.min (tolerance, n)
    in
      SST.insertAddWeightsLimitProbes {probes = tolerance} table widx;
      true
    end
    handle SST.Full => false


  datatype successors_result =
    AllSucceeded
  | SomeFailed of {widx: BasisIdx.t * Complex.t, gatenum: int} list


  fun expand {gates, state, expected} =
    let
      val numGates = Seq.length gates
      fun gate i = Seq.nth gates i

      (* number of initial elements *)
      val n = DelayedSeq.length state

      (* block size configuration *)
      val numBlocks = Util.ceilDiv n blockSize
      fun blockStart b = blockSize * b
      fun blockStop b =
        Int.min (n, blockSize + blockStart b)

      (* each block keeps track of how many elements have been completed, and
       * any pending insertions (from a previous failure that triggered a resizing)
       *)
      val blockRemainingStarts =
        SeqBasis.tabulate 5000 (0, numBlocks) blockStart
      fun blockHasRemaining b =
        Array.sub (blockRemainingStarts, b) < blockStop b

      val blockPending = SeqBasis.tabulate 5000 (0, numBlocks) (fn _ => [])

      fun blockHasPending b =
        not (List.null (Array.sub (blockPending, b)))

      (* fun blockNumPending b =
        List.length (Array.sub (blockPending, b)) *)

      fun blockPopPending b =
        case Array.sub (blockPending, b) of
          x :: rest => (Array.update (blockPending, b, rest); SOME x)
        | _ => NONE

      fun blockPushPending b pending =
        case Array.sub (blockPending, b) of
          [] => Array.update (blockPending, b, pending)
        | xx => Array.update (blockPending, b, pending @ xx)


      (* remainingBlocks: list of block ids that aren't finished yet
       * table: place to put results; this will fill up and need resizing
       *)
      fun loop remainingBlocks table =
        let
          val full = ref (0w0 : Word8.word)
          fun notFull () =
            (!full = 0w0)
          fun isFull () =
            (!full = 0w1)
          fun markFull () = (full := 0w1)


          (* try insert all successors of `(widx, gatenum)` into `table` *)
          fun doGates (widx, gatenum) : successors_result =
            if Complex.isZero (#2 widx) then
              AllSucceeded
            else if isFull () then
              SomeFailed [{widx = widx, gatenum = gatenum}]
            else if gatenum >= numGates then
              if notFull () andalso tryPut table widx then
                AllSucceeded
              else
                ( if notFull () then markFull () else ()
                ; SomeFailed [{widx = widx, gatenum = gatenum}]
                )
            else
              case Gate.apply (gate gatenum) widx of
                Gate.OutputOne widx' => doGates (widx', gatenum + 1)
              | Gate.OutputTwo (widx1, widx2) =>
                  case doGates (widx1, gatenum + 1) of
                    AllSucceeded => doGates (widx2, gatenum + 1)
                  | SomeFailed failures =>
                      SomeFailed
                        ({widx = widx2, gatenum = gatenum + 1} :: failures)


          fun workOnBlock b =
            let
              val start = Array.sub (blockRemainingStarts, b)
              val stop = blockStop b

              fun clearPending () =
                case blockPopPending b of
                  NONE => true
                | SOME {widx, gatenum} =>
                    case doGates (widx, gatenum) of
                      AllSucceeded => clearPending ()
                    | SomeFailed failures =>
                        (blockPushPending b failures; false)

              fun loop i =
                if i >= stop then
                  Array.update (blockRemainingStarts, b, stop)
                else
                  case doGates (DelayedSeq.nth state i, 0) of
                    AllSucceeded => loop (i + 1)
                  | SomeFailed failures =>
                      ( Array.update (blockRemainingStarts, b, i + 1)
                      ; blockPushPending b failures
                      )
            in
              if clearPending () then loop start else ()
            end


          (* push through blocks *)
          val _ = ForkJoin.parfor 1 (0, Seq.length remainingBlocks) (fn bi =>
            let val b = Seq.nth remainingBlocks bi
            in workOnBlock b
            end)

          val remainingBlocks' =
            Seq.filter (fn b => blockHasPending b orelse blockHasRemaining b)
              remainingBlocks
        in
          if Seq.length remainingBlocks' = 0 then
            table
          else
            ( print
                ("growing to "
                 ^
                 Int.toString (Real.ceil
                   (1.5 * Real.fromInt (SST.capacity table))) ^ "\n")
            ; loop remainingBlocks' (SST.increaseCapacityByFactor 1.5 table)
            )
        end


      val impossibleBasisIdx = BasisIdx.flip BasisIdx.zeros 63
      val initialTable =
        SST.make {capacity = expected, emptykey = impossibleBasisIdx}
      val initialBlocks = Seq.tabulate (fn b => b) numBlocks
    in
      ( print ("expand state; guess = " ^ Int.toString expected ^ "\n")
      ; loop initialBlocks initialTable
      )
    end

end
