functor Fingerprint (structure B: BASIS_IDX structure C: COMPLEX):
sig
  type sparse_state_vector = (B.t * C.t) option DelayedSeq.t

  type fingerprint = (B.t * C.t) Seq.t
  type t = fingerprint

  val fingerprint: sparse_state_vector -> fingerprint
end =
struct

  type sparse_state_vector = (B.t * C.t) option DelayedSeq.t
  type fingerprint = (B.t * C.t) Seq.t
  type t = fingerprint

  val fingerprintSize = 10

  structure BCKey =
  struct
    type ord_key = (B.t * C.t)
    fun compare ((b1, c1), (b2, c2)) =
      let
        val m1 = C.magnitude c1
        val m2 = C.magnitude c2
      in
        if C.R.< (C.R.abs (C.R.- (m1, m2)), C.zeroThreshold) then
          (* reverse the comparison here so that the output fingerprint is
           * sorted like normal binary numbers, for readability
           *)
          B.compare (b2, b1)
        else if C.R.< (m1, m2) then
          LESS
        else
          GREATER
      end
  end

  structure BCSet = RedBlackSetFn(BCKey)


  fun insert_keep_heavy (fp: BCSet.set, (b, c)) =
    let
      val fp = BCSet.add (fp, (b, c))
    in
      if BCSet.numItems fp > fingerprintSize then
        BCSet.delete (fp, BCSet.minItem fp)
      else
        fp
    end

  fun insert_keep_heavy_opt (fp: BCSet.set, x) =
    case x of
      NONE => fp
    | SOME bc => insert_keep_heavy (fp, bc)


  fun merge_fingerprints (fp1, fp2) =
    BCSet.foldl (fn (elem, acc) => insert_keep_heavy (acc, elem)) fp1 fp2


  fun fingerprint state =
    let
      val n = DelayedSeq.length state
      val block_size = 10000
      val num_blocks = Util.ceilDiv n block_size
      fun doBlock b =
        let
          val lo = b * block_size
          val hi = Int.min (lo + block_size, n)
          val block = DelayedSeq.subseq state (lo, hi - lo)
        in
          DelayedSeq.iterate insert_keep_heavy_opt BCSet.empty block
        end

      val fp =
        SeqBasis.reduce 1 merge_fingerprints BCSet.empty (0, num_blocks) doBlock
    in
      Seq.filter (fn (b, c) => not (C.isZero c))
        (Seq.fromRevList (BCSet.toList fp))
    end

end
