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

