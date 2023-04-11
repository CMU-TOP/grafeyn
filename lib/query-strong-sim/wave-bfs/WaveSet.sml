(* mapping of gatenum -> wave *)
structure WaveSet:
sig
  type waveset
  type t = waveset

  val empty: waveset
  val singleton: int * Wave.t -> waveset
  val numWaves: waveset -> int

  val totalSize: waveset -> int
  val totalSizePotential: {numGates: int} -> waveset -> int

  val insert: waveset -> int * Wave.t -> waveset

  (* returns (updated wave set, (gatenum, wave)) *)
  val removeOldest: waveset -> waveset * int * Wave.t
  val removeNewest: waveset -> waveset * int * Wave.t

  val remove: waveset -> int -> (waveset * Wave.t) option

  val pullMerge: waveset -> int * Wave.t -> waveset * Wave.t
end =
struct
  structure IntKey = struct open Int type ord_key = int end
  structure M = RedBlackMapFn(IntKey)
  type waveset = Wave.t M.map
  type t = waveset

  val empty = M.empty

  fun singleton (gatenum, wave) = M.singleton (gatenum, wave)

  fun numWaves waves = M.numItems waves

  fun totalSize waves =
    let
      fun next (wave, acc) = acc + Wave.nonZeroSize wave
    in
      M.foldl next 0 waves
    end

  fun totalSizePotential {numGates} waves =
    let
      fun next (gatenum, wave, acc) =
        acc + (numGates - gatenum) * Wave.nonZeroSize wave
    in
      M.foldli next 0 waves
    end

  fun insert waves (gatenum, wave) =
    if Wave.capacity wave = 0 then waves
    else M.insertWith Wave.merge (waves, gatenum, wave)

  fun removeOldest waves =
    case M.firsti waves of
      SOME (gatenum, _) =>
        let val (waves', wave) = M.remove (waves, gatenum)
        in (waves', gatenum, wave)
        end
    | NONE => raise Fail "WaveSet.removeOldest: empty"

  fun removeNewest waves =
    let
      (* TODO: why does this signature not have `last`...? *)
      val gatenum = List.hd (List.rev (M.listKeys waves))
      val (waves', wave) = M.remove (waves, gatenum)
    in
      (waves', gatenum, wave)
    end
    handle _ => raise Fail "WaveSet.removeNewest"

  fun remove waves gatenum =
    if M.inDomain (waves, gatenum) then SOME (M.remove (waves, gatenum))
    else NONE

  fun pullMerge waves (gatenum, currentWave) =
    case remove waves gatenum of
      NONE => (waves, currentWave)
    | SOME (waves', wave) => (waves', Wave.merge (wave, currentWave))
end
