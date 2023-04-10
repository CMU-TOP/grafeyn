(* mapping of gatenum -> wave *)
structure WaveSet:
sig
  type waveset
  type t = waveset

  val empty: waveset
  val singleton: int * Wave.t -> waveset
  val numWaves: waveset -> int

  val totalSizePotential: {numGates: int} -> waveset -> int

  val insert: waveset -> int * Wave.t -> waveset

  (* returns (updated wave set, (gatenum, wave)) *)
  val removeOldest: waveset -> waveset * (int * Wave.t)
  val removeNewest: waveset -> waveset * (int * Wave.t)

  val remove: waveset -> int -> (waveset * (int * Wave.t)) option
end =
struct
  structure IntKey = struct open Int type ord_key = int end
  structure M = RedBlackMapFn(IntKey)
  type waveset = Wave.t M.map
  type t = waveset

  val empty = M.empty

  fun singleton wave =
    M.singleton (Wave.position wave, wave)

  fun numWaves waves = M.numItems waves

  fun totalSizePotential {numGates} waves =
    let
      fun next (gatenum, wave, acc) =
        acc + (numGates - gatenum) * Wave.nonZeroSize wave
    in
      M.foldli next 0 waves
    end

  fun insert waves (gatenum, wave) =
    M.insertWith Wave.merge (waves, gatenum, wave)

  fun removeOldest waves =
    case M.firsti waves of
      SOME (pos, _) =>
        let val (waves', wave) = M.remove (waves, pos)
        in (waves', wave)
        end
    | NONE => raise Fail "QuerySimWaveBFS.WaveSet.removeOldest: empty"

  fun removeNewest waves =
    let
      (* TODO: why does this signature not have `last`...? *)
      val lastkey = List.hd (List.rev (M.listKeys waves))
      val (waves', wave) = M.remove (waves, lastkey)
    in
      (waves', wave)
    end
    handle _ => raise Fail "QuerySimWaveBFS.WaveSet.removeNewest"

  fun remove waves gatenum =
    if M.inDomain (waves, gatenum) then SOME (M.remove (waves, gatenum))
    else NONE
end
