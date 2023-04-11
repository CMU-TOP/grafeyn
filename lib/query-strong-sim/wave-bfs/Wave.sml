structure Wave:
sig
  type wave
  type t = wave

  val empty: unit -> wave
  val singleton: BasisIdx.t * Complex.t -> wave

  val capacity: wave -> int
  val size: wave -> int
  val nonZeroSize: wave -> int

  val lookup: wave -> BasisIdx.t -> Complex.t

  val applyAll: {grain: int} -> wave -> (BasisIdx.t * Complex.t -> unit) -> unit

  val applyNonZero: {grain: int}
                    -> wave
                    -> (BasisIdx.t * Complex.t -> unit)
                    -> unit

  (* Use constraint to control the size of the result *)
  val advanceAndSplit: {constraint: int, gate: Gate.t}
                       -> wave
                       -> {numGateApps: int, result: wave, leftover: wave}

  val merge: wave * wave -> wave
end =
struct

  structure HT = HashTable

  fun makeTable cap =
    HT.make
      { hash = BasisIdx.hash
      , eq = BasisIdx.equal
      , capacity = cap
      , maxload = 0.75
      }

  (* ======================================================================= *)

  type t = (BasisIdx.t * Complex.t) option Seq.t
  type wave = t


  fun empty () = Seq.empty ()


  fun singleton widx =
    Seq.singleton (SOME widx)


  fun size wave =
    SeqBasis.reduce 2000 op+ 0 (0, Seq.length wave) (fn i =>
      if Option.isSome (Seq.nth wave i) then 1 else 0)


  fun nonZeroSize wave =
    SeqBasis.reduce 2000 op+ 0 (0, Seq.length wave) (fn i =>
      case Seq.nth wave i of
        NONE => 0
      | SOME (bidx, weight) => if Complex.isZero weight then 0 else 1)


  fun capacity wave = Seq.length wave


  fun lookup wave bidx =
    let
      fun choose (x, y) =
        case (x, y) of
          (SOME _, _) => x
        | (_, SOME _) => y
        | _ => NONE

      val result =
        SeqBasis.reduce 2000 choose NONE (0, Seq.length wave) (fn i =>
          case Seq.nth wave i of
            NONE => NONE
          | SOME (b, w) => if BasisIdx.equal (b, bidx) then SOME w else NONE)
    in
      case result of
        NONE => Complex.zero
      | SOME w => w
    end


  fun applyAll {grain} wave f =
    ForkJoin.parfor grain (0, Seq.length wave) (fn i =>
      case Seq.nth wave i of
        NONE => ()
      | SOME widx => f widx)


  fun applyNonZero {grain} wave f =
    ForkJoin.parfor grain (0, Seq.length wave) (fn i =>
      case Seq.nth wave i of
        NONE => ()
      | SOME (bidx, weight) =>
          if Complex.isZero weight then () else f (bidx, weight))


  fun merge (wave1, wave2) =
    let
      fun loopGuessCapacity desiredCapacity =
        let
          val _ = print
            ("trying desiredCapacity=" ^ Int.toString desiredCapacity ^ "\n")
          val table = makeTable desiredCapacity

          fun put widx =
            HT.insertWith Complex.+ table widx
        in
          applyNonZero {grain = 100} wave1 put;
          applyNonZero {grain = 100} wave2 put;
          print "merge success\n";
          HT.unsafeViewContents table
        end
        handle HT.Full =>
          loopGuessCapacity (Real.ceil (1.5 * Real.fromInt desiredCapacity))

      val totalSize = size wave1 + size wave2
      val totalCapacities = capacity wave1 + capacity wave2

      val _ = print
        ("merging totalSize=" ^ Int.toString totalSize ^ " totalCapacities="
         ^ Int.toString totalCapacities ^ "\n")

      val desiredCapacity = Int.min (totalCapacities, Real.ceil
        (1.5 * Real.fromInt totalSize))
    in
      loopGuessCapacity desiredCapacity
    end


  fun advanceAndSplit {constraint, gate} wave =
    let
      fun upCapacity cap =
        Int.min (constraint, Real.ceil (1.5 * Real.fromInt cap))

      fun upPrefixSize prefixSize =
        Int.min (2 * prefixSize, Seq.length wave)

      fun loop previousResult prefixSize cap =
        let
          val _ = print
            ("advanceAndSplit |wave|=" ^ Int.toString (Seq.length wave)
             ^ " prefixSize=" ^ Int.toString prefixSize ^ " capacity="
             ^ Int.toString cap ^ "\n")

          val prefix = Seq.take wave prefixSize
          val table = makeTable cap

          fun doGate widx =
            case Gate.apply gate widx of
              Gate.OutputOne widx' => HT.insertWith Complex.+ table widx'
            | Gate.OutputTwo (widx1, widx2) =>
                ( HT.insertWith Complex.+ table widx1
                ; HT.insertWith Complex.+ table widx2
                )

          val _ = applyNonZero {grain = 100} prefix doGate
          val result = SOME (prefix, HT.unsafeViewContents table)
        in
          if prefixSize >= Seq.length wave then result
          else loop result (upPrefixSize prefixSize) cap
        end
        handle HT.Full =>
          if cap >= constraint then previousResult
          else loop previousResult prefixSize (upCapacity cap)

      val startingSize = Int.min
        (Seq.length wave, Int.max (1, constraint div 2))

      val multiplier = if Gate.expectBranching gate then 4.0 else 2.0
      val startingCapacity = Int.min (constraint, Real.ceil
        (multiplier * Real.fromInt startingSize))
    in
      case loop NONE startingSize startingCapacity of
        SOME (prefix, result) =>
          { numGateApps = nonZeroSize prefix
          , result = result
          , leftover = Seq.drop wave (Seq.length prefix)
          }

      | NONE => Util.die "Wave.advanceAndSplit: whoops"
    end

end
