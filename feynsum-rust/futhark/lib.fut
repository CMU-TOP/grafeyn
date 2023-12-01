type complex = [2]f32

def add (a: complex) (b: complex) : complex =
[ a[0] + b[0], a[1] + b[1] ]

def mul (a: complex) (b: complex) : complex =
 [ a[0] * b[0] - a[1] * b[1], a[0] * b[1] + a[1] * b[0] ]

def zero = [ 0f32, 0f32 ]

entry matmul [n][m][p]
           (A: [n][m]complex) (B: [m][p]complex) : [n][p]complex =
  map (\A_row ->
         map (\B_col ->
                reduce add (copy zero) (map2 mul A_row B_col))
             (transpose B))
      A


def get_G_index [q] (i: i64) (qubit_indices: [q]i64) : i64 =
       reduce (\acc -> \j -> acc + (((i >> (qubit_indices[j])) & 1) << j))
              0i64
              (indices qubit_indices)

def get_S_index [q] (i: i64) (k: i64) (qubit_indices: [q]i64) : i64 =
       reduce (\acc -> 
                     \j -> 
                     if bool.i64 (k & (1 << j)) then acc | (1 << qubit_indices[j]) else acc & !(1 << qubit_indices[j]))
              i
              (indices qubit_indices)


entry apply_vec [n][q][qsq]
          (S: [n]complex) (G : [qsq][qsq]complex) (qubit_indices: [q]i64): [n]complex =
  map (\i -> 
       (let G_index = get_G_index i qubit_indices in
              (reduce 
              add
              (copy zero)
              (map (\k -> 
                     mul S[get_S_index i k qubit_indices] G[G_index][k])
                     (indices G)
              )
              )))
(indices S)
