structure Complex:
sig
  type t

  val toString: t -> string

  val make: (real * real) -> t

  val real: real -> t
  val imag: real -> t
  val rotateBy: real -> t (* rotateBy x = e^(ix) *)

  val isZero: t -> bool
  val isNonZero: t -> bool

  val zero: t
  val i: t

  val ~ : t -> t
  val + : t * t -> t
  val * : t * t -> t
end =
struct
  datatype t = C of {re: real, im: real}

  val rtos = Real.fmt (StringCvt.FIX (SOME 4))

  fun toString (C {re, im}) =
    rtos re ^ " + " ^ rtos im ^ "i"

  fun make (re, im) = C {re=re, im=im}

  fun closeEnough (x, y) =
    Real.abs (x - y) <= 0.000000001

  fun isZero (C {re, im}) =
    closeEnough (re, 0.0) andalso closeEnough (im, 0.0)

  fun isNonZero c =
    not (isZero c)

  fun rotateBy r =
    C {re = Math.cos r, im = Math.sin r}

  fun real r = C {re = r, im = 0.0}
  fun imag i = C {re = 0.0, im = i}

  val zero = C {re = 0.0, im = 0.0}
  val i = C {re = 0.0, im = 1.0}

  fun neg (C {re, im}) =
    C {re = ~re, im = ~im}

  fun add (C x, C y) =
    C {re = #re x + #re y, im = #im x + #im y}

  fun mul (C {re = a, im = b}, C {re = c, im = d}) =
    C {re = a * c + b * d, im = a * d + b * c}

  val ~ = neg
  val op+ = add
  val op* = mul
end
