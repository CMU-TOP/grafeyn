(** Copyright (c) 2020-2021 Sam Westrick
  * See the file LICENSE for details.
  *
  * This structure implements a lot of the "simple" parser functions, to
  * avoid cluttering the main Parser implementation.
  *)

structure ParseSimple:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker
  type tokens = Token.t Seq.t

  val identifier: tokens -> (int, Token.t) parser
  val reserved: tokens -> Token.reserved -> (int, Token.t) parser
  val maybeReserved: tokens -> Token.reserved -> (int, Token.t option) parser
end =
struct

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker
  type tokens = Token.t Seq.t


  fun check toks f i =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
    in
      i < numToks andalso f (tok i)
    end


  fun isReserved toks rc i =
    check toks (fn t => Token.Reserved rc = Token.getClass t) i


  fun identifier toks i =
    if check toks Token.isIdentifier i then
      (i + 1, Seq.nth toks i)
    else
      ParserUtils.tokError toks
        { pos = i
        , what = "Unexpected token. Expected to see an identifier."
        , explain = NONE
        }


  fun reserved toks rc i =
    if isReserved toks rc i then
      (i + 1, Seq.nth toks i)
    else
      ParserUtils.tokError toks
        { pos = i
        , what =
            "Unexpected token. Expected to see " ^ "'"
            ^ Token.reservedToString rc ^ "'"
        , explain = NONE
        }


  fun maybeReserved toks rc i =
    if isReserved toks rc i then (i + 1, SOME (Seq.nth toks i)) else (i, NONE)

end
