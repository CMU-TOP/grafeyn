functor ExpandState(SST: SPARSE_STATE_TABLE) :>
sig

  type state = (BasisIdx.t * Complex.t) option DelayedSeq.t

  val expand: {gates: Gate.t Seq.t, numQubits: int, state: state, expected: int}
              -> SST.t

end =
struct

  structure DS = DelayedSeq
  type state = (BasisIdx.t * Complex.t) option DS.t


  val blockSize = CommandLineArgs.parseInt "expand-block-size" 10000
  val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")


  val maxload = CommandLineArgs.parseReal "expand-max-load" 0.9
  val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")


  fun log2 x = Math.log10 x / Math.log10 2.0


  (* tryPut will fail (and return false) if the table is full, or almost full.
   *
   * We use here a trick to avoid needing to scan the whole table to figure out
   * the load factor. The idea is: if a single insertion takes a long time,
   * then we can assume the load factor is high.
   *
   * At load factor `alpha`, the longest probe sequence is probably going to
   * be approximately 
   *   log(n)/(alpha - 1 - log(alpha))
   * Analysis in this paper:
   *   Linear Probing: The Probable Largest Search Time Grows Logarithmically
   *   with the Number of Records
   *   Author: B. Pittel
   *
   * So, if number of probes witnessed by a single insertion is nearby this
   * amount, then the table is probably nearby `alpha` load factor, so it's
   * time to back off and resize the table.
   *
   * (This seems to work decently, but it's possible we could do better. Need
   * to analyze the probability of the load factor being high, given the
   * observation of a single long probe sequence.)
   *)
  fun tryPut table widx =
    let
      val n = SST.capacity table
      val probablyLongestProbe = Real.ceil
        (log2 (Real.fromInt n) / (maxload - 1.0 - log2 maxload))
      val tolerance = 2 * Int.max (10, probablyLongestProbe)
      val tolerance = Int.min (tolerance, n)
    in
      SST.insertAddWeightsLimitProbes {probes = tolerance} table widx;
      true
    end
    handle SST.Full => false


  datatype successors_result =
    AllSucceeded
  | SomeFailed of {widx: BasisIdx.t * Complex.t, gatenum: int} list


  fun expand {gates, numQubits, state, expected} =
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
      fun blockPopPending b =
        case Array.sub (blockPending, b) of
          [] => NONE
        | x :: rest => (Array.update (blockPending, b, rest); SOME x)
      fun blockPushPending b pending =
        case Array.sub (blockPending, b) of
          [] => Array.update (blockPending, b, pending)
        | xx => Array.update (blockPending, b, pending @ xx)

      (* fun blockHasPending b = false
      fun blockPopPending b = NONE
      fun blockPushPending b _ = () *)


      (* val apply =
        if numGates = 1 then
          let val f = Gate.apply (gate 0)
          in fn (_, widx) => f widx
          end
        else
          (fn (gatenum, widx) => Gate.apply (gate gatenum) widx) *)

      val apply = (fn (gatenum, widx) => Gate.apply (gate gatenum) widx)

      (* remainingBlocks: list of block ids that aren't finished yet
       * table: place to put results; this will fill up and need resizing
       *)
      fun loop remainingBlocks table =
        let
          val full = ref (0w0 : Word8.word)
          fun notFull () =
            (!full = 0w0)
          fun markFull () = (full := 0w1)

          (* fun notFull () = true
          fun markFull () = () *)

          (* fun doGates (widx, gatenum) =
            if Complex.isZero (#2 widx) then
              AllSucceeded
            else if gatenum >= numGates then
              if tryPut table widx then AllSucceeded else Util.die ("uh oh!")
            else
              case apply (gatenum, widx) of
                Gate.OutputOne widx' => doGates (widx', gatenum + 1)
              | Gate.OutputTwo (widx1, widx2) =>
                  (doGates (widx1, gatenum + 1); doGates (widx2, gatenum + 1)) *)

          (* try insert all successors of `(widx, gatenum)` into `table` *)
          fun doGates (widx, gatenum) : successors_result =
            if Complex.isZero (#2 widx) then
              AllSucceeded
            else if gatenum >= numGates then
              if notFull () andalso tryPut table widx then
                AllSucceeded
              else
                ( if notFull () then markFull () else ()
                ; SomeFailed [{widx = widx, gatenum = gatenum}]
                )
            else
              case apply (gatenum, widx) of
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
                  case DelayedSeq.nth state i of
                    NONE => loop (i + 1)
                  | SOME elem =>
                      case doGates (elem, 0) of
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


      val initialTable = SST.make {capacity = expected, numQubits = numQubits}
      val initialBlocks = Seq.tabulate (fn b => b) numBlocks
    in
      ( print ("expand state; guess = " ^ Int.toString expected ^ "\n")
      ; loop initialBlocks initialTable
      )
    end

end
