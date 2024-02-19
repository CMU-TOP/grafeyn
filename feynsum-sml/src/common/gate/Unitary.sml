structure Unitary (structure B: BASIS_IDX
                   structure C: COMPLEX):
sig
  type unitary
  type qubit_idx
  val make: C.t list list -> qubit_idx list -> unitary
  val compose: unitary * unitary -> unitary
end =
struct
  type qubit_idx = int
  type unitary = { data: C.r array, qargs: qubit_idx array, size: int }


  (*
   * a b c d
   * e f g h  ->  a_re a_im b_re b_im c_re c_im d_re d_im e_re e_im...
   * i j k l
   * m n o p
   *)
  fun sub' (u as {data, qargs, size}: unitary) ((i1, i2): int * int) =
      let val x = i1 * size + i2 in
        (Array.sub (data, 2 * x),
         Array.sub (data, 2 * x + 1))
      end

  fun qarg_idx (u as {data, qargs, size}: unitary) (qidx: qubit_idx) =
      case Array.findi (fn (i, qidx') -> qidx' = qidx) qargs of
          NONE => raise Error "Looked up missing qubit in unitary"
        | SOME (i, _) => i

  fun b2i (u as {data, qargs, size}: unitary) (b: B.t) =
      #2 (Array.foldr (fn (qarg, (exponent, i)) =>
                          (exponent * 2, if B.get b qarg then exponent + i else i))
                      (1, 0) qargs)
  
  fun sub (u: unitary) ((b1, b2): B.t * B.t) =
      sub' u (b2i u b1, b2i u b2)
  
  fun make (u: C.t list list) (qis: qubit_idx list) =
      let val size = Helpers.exp2 (List.length qis)

          (* Check u is a size * size matrix *)
          fun shapeError l = if List.length u = size
                             then () else raise Error "Unitary.make: misshaped input"
          val _ = shapeError u
          val _ = List.foldr (shapeError o #1) () u

          val qargs = Array.fromList qis
          val data = Array.array (size * size * 2)
          fun storeAmp (a, row, col) =
              let val (re, im) = C.view a
                  val i = (row * size + col) in
                Array.update (data, 2 * i, re);
                Array.update (data, 2 * i + 1, im)
              end
          fun storeVec (v, row, col) =
              case v of
                  nil => ()
                | a :: v' => (storeAmp (a, row, col);
                              storeVec (v', row, col + 1))
          fun storeMtx (m, row) =
              case m of
                  nil => ()
                | v :: m' => (storeVec (v, row, 0);
                              storeMtx (m, row + 1))
          val _ = storeMtx' (u, 0)
      in
        { data = data, qargs = qargs, size = size }
      end

  type intmap = (int * int) array
  fun intmapMake (keys: int array) (map: int -> int) =
      Array.tabulate (Array.length keys,
                      fn i => let val ki = Array.sub (keys, i) in (ki, map ki) end)
  fun intmapMakei (keys: int array) (map: int * int -> int) =
      Array.tabulate (Array.length keys,
                      fn i => let val ki = Array.sub (keys, i) in (ki, map (i, ki)) end)

  fun intmapLookup (map: intmap) (i: int) =
      let fun iter (j: int) =
              if j >= Array.length intmap then
                NONE
              else
                let val (i', x) = Array.sub (map, j) = i in
                  if i = i' then SOME x else iter (j + 1)
                end
      in
        iter 0
      end

  fun reorder_qargs (u as { data, qargs, size }: unitary) (qargs': qubit_idx array) =
      if Helpers.eqArray (qargs, qargs') op= then
        u
      else
        let val data' = copyArray data
            val maxQidx = Array.foldr Int.max ~1
            val qmax1 = maxQidx qargs1
            val qmax2 = maxQidx qargs2
            val qmax = Int.max (qmax1, qmax 2)

            (* Map from a qidx to its original placement in qargs/qargs' *)
            val old_idx = intmapMakei (qargs, #1)
            val new_idx = intmapMakei (qargs', #1)
            val new2old_idx = intmapMake (qargs', intmapLookup old_idx)
            val reordered = TODO
        in
          TODO
        end

  fun multiplyAligned ((u1, u2): unitary * unitary) =
      let val { data = data1, qargs = qargs1, size = size1 } = u1
          val { data = data2, qargs = qargs2, size = size2 } = u2 in
        if not (Helpers.eqArray (qargs1, qargs2)) then
          raise Error "multiplyAligned: unitaries are not aligned!"
        else
          TODO
      end

  fun compose ((u1, u2): unitary * unitary) =
      TODO
end
