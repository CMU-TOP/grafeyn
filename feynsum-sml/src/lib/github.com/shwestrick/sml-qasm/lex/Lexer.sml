(** Copyright (c) 2020-2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Lexer:
sig
  (** Get the next token in the given source. If there isn't one, returns NONE.
    * raises Error if there's a problem.
    *)
  val next: Source.t -> Token.Pretoken.t option

  (** Get all the tokens in the given source.
    * raises Error if there's a problem.
    *)
  val tokens: Source.t -> Token.t Seq.t
end =
struct

  fun success tok = SOME tok

  fun error {pos, what, explain} =
    raise Error.Error
      (Error.lineError
         {header = "SYNTAX ERROR", pos = pos, what = what, explain = explain})


  fun next (src: Source.t) : Token.Pretoken.t option =
    let
      val startOffset = Source.absoluteStartOffset src
      val src = Source.wholeFile src

      (** Some helpers for making source slices and tokens. *)
      fun slice (i, j) =
        Source.slice src (i, j - i)
      fun mk x (i, j) =
        Token.Pretoken.make (slice (i, j)) x
      fun mkr x (i, j) =
        Token.Pretoken.reserved (slice (i, j)) x

      fun get i = Source.nth src i

      fun isEndOfFileAt s = s >= Source.length src

      (** This silliness lets you write almost-English like this:
        *   if is #"x" at i          then ...
        *   if check isSymbolic at i then ...
        *)
      infix 5 at
      fun f at i = f i
      fun check f i =
        i < Source.length src andalso f (get i)
      fun is c =
        check (fn c' => c = c')


      fun isPrint c =
        let val i = Char.ord c
        in 32 <= i andalso i <= 126
        end

      fun isMaybeUnicode c =
        let val i = Char.ord c
        in (128 <= i andalso i <= 253) (* ?? *)
        end

      (* Valid unicode chars according to OpenQASM are
       *   [\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]
       *
       * This is regex syntax. Here's what they mean, according to 
       * https://www.regular-expressions.info/unicode.html:
       *   \p{Lu} - an uppercase letter that has a lowercase variant
       *   \p{Ll} - a lowercase letter that has an uppercase variant
       *   \p{Lt} - a letter that appears at the start of a word when only the
       *            first letter of the word is capitalized
       *   \p{Lm} - a special character that is used like a letter
       *   \p{Lo} - a letter or ideograph that does not have lowercase and
       *            uppercase variants
       *   \p{Nl} - a number that looks like a letter, such as a Roman numeral
       *
       * Leaving a TODO for now to come back to this...
       *)

      fun tryAdvancePastFirstCharacterOfIdentifier s =
        if is #"_" at s then
          SOME (s + 1)
        else if check LexUtils.isLetter at s then
          SOME (s + 1)
        else if check isMaybeUnicode at s then
          error
            { pos = slice (s, s + 1)
            , what = "TODO: Unicode support..."
            , explain = NONE
            }
        else
          NONE

      (** ====================================================================
        * STATE MACHINE
        *
        * This bunch of mutually-recursive functions implements an efficient
        * state machine. Each is named `loop_<STATE_NAME>`. The arguments
        * are always
        *   `loop_XXX s [args]`
        * where
        *   `s` is the current position, and
        *   `args` is a state-dependent state (haha)
        *)

      fun loop_topLevel s =
        if isEndOfFileAt s then
          NONE
        else
          case get s of
            #"(" => success (mkr Token.OpenParen (s, s + 1))
          | #")" => success (mkr Token.CloseParen (s, s + 1))
          | #"[" => success (mkr Token.OpenBracket (s, s + 1))
          | #"]" => success (mkr Token.CloseBracket (s, s + 1))
          | #"{" => success (mkr Token.OpenBrace (s, s + 1))
          | #"}" => success (mkr Token.CloseBrace (s, s + 1))
          | #":" => success (mkr Token.Colon (s, s + 1))
          | #";" => success (mkr Token.Semicolon (s, s + 1))
          | #"." => loop_afterDot (s + 1)
          | #"," => success (mkr Token.Comma (s, s + 1))
          | #"=" => success (mkr Token.Equals (s, s + 1))
          | #"*" => success (mkr Token.Asterisk (s, s + 1))
          | #"+" => success (mkr Token.Plus (s, s + 1))
          | #"-" =>
              if is #">" at s + 1 then success (mkr Token.Arrow (s, s + 2))
              else success (mkr Token.Minus (s, s + 1))

          | #"/" => loop_afterSlash (s + 1)

          | #"\"" => loop_inString (s + 1) {stringStart = s, firstChar = #"\""}

          | #"'" => loop_inString (s + 1) {stringStart = s, firstChar = #"'"}

          | #"0" => loop_afterZero (s + 1)

          | c =>
              if LexUtils.isDecDigit c then
                loop_decIntegerLiteralAfterDigit (s + 1) {constStart = s}
              else if Char.isSpace c then
                loop_whitespace {start = s} (s + 1)
              else
                case tryAdvancePastFirstCharacterOfIdentifier s of
                  SOME s' => loop_identifier s' {idStart = s}
                | NONE =>
                    error
                      { pos = slice (s, s + 1)
                      , what = "Unexpected character."
                      , explain = SOME "Perhaps from unsupported character-set?"
                      }


      and loop_identifier s (args as {idStart}) =
        if check LexUtils.isDecDigit at s then
          loop_identifier (s + 1) args
        else
          case tryAdvancePastFirstCharacterOfIdentifier s of
            SOME s' => loop_identifier s' args
          | NONE =>
              success (Token.Pretoken.reservedOrIdentifier (slice (idStart, s)))


      and loop_inString s (args as {stringStart, firstChar}) =
        if is firstChar at s then
          success (mk Token.StringLiteral (stringStart, s + 1))
        else if check LexUtils.isValidStringChar at s then
          loop_inString (s + 1) args
        else
          error
            { pos = slice (s, s + 1)
            , what = "Invalid character in string literal."
            , explain = NONE
            }


      and loop_afterDot s =
        if
          check LexUtils.isDecDigit s
          andalso not (check LexUtils.isDecDigit (s - 2))
        then
          loop_floatLiteral (s + 1) {constStart = s - 1}
        else
          error
            { pos = slice (s - 1, s)
            , what = "Unexpected character."
            , explain = NONE
            }


      and loop_whitespace {start} i =
        if check Char.isSpace i then loop_whitespace {start = start} (i + 1)
        else success (mk Token.Whitespace (start, i))


      (** After seeing "0", we're certainly at the beginning of some sort
        * of numeric constant. We need to figure out if this is an integer or
        * a word or a real, and if it is hex or decimal format.
        *)
      and loop_afterZero s =
        if is #"x" at s orelse is #"X" at s then
          loop_hexIntegerLiteralExpectDigit (s + 1) {constStart = s - 1}
        else if is #"o" s then
          loop_octIntegerLiteralExpectDigit (s + 1) {constStart = s - 1}
        else if is #"b" s orelse is #"B" s then
          loop_binIntegerLiteralExpectDigit (s + 1) {constStart = s - 1}
        else if is #"." at s then
          loop_floatLiteralAfterDot (s + 1) {constStart = s - 1}
        else if is #"e" at s orelse is #"E" at s then
          loop_floatLiteralAfterExponent (s + 1) {constStart = s - 1}
        else if is #"_" s then
          loop_decIntegerLiteralExpectDigit (s + 1) {constStart = s - 1}
        else if check LexUtils.isDecDigit s then
          loop_decIntegerLiteralAfterDigit (s + 1) {constStart = s - 1}
        else
          success (mk Token.DecimalIntegerLiteral (s - 1, s))


      and loop_decIntegerLiteralAfterDigit s (args as {constStart}) =
        if is #"_" s then
          loop_decIntegerLiteralExpectDigit (s + 1) args
        else if check LexUtils.isDecDigit at s then
          loop_decIntegerLiteralAfterDigit (s + 1) args
        else if is #"." at s then
          loop_floatLiteralAfterDot (s + 1) args
        else if is #"e" at s orelse is #"E" at s then
          loop_floatLiteralAfterExponent (s + 1) args
        else
          success (mk Token.DecimalIntegerLiteral (constStart, s))


      and loop_decIntegerLiteralExpectDigit s (args as {constStart}) =
        if is #"_" s then
          error
            { pos = slice (constStart, s)
            , what = "Invalid decimal integer literal."
            , explain = SOME
                "After an underline, there needs to be at least one decimal digit."
            }
        else if check LexUtils.isDecDigit at s then
          loop_decIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.DecimalIntegerLiteral (constStart, s))


      and loop_hexIntegerLiteralExpectDigit s (args as {constStart}) =
        if is #"_" s then
          error
            { pos = slice (constStart, s)
            , what = "Invalid hex integer literal."
            , explain = SOME
                "After an underline, there needs to be at least one hex digit."
            }
        else if check LexUtils.isHexDigit at s then
          loop_hexIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.HexIntegerLiteral (constStart, s))


      and loop_hexIntegerLiteralAfterDigit s (args as {constStart}) =
        if is #"_" s then
          loop_hexIntegerLiteralExpectDigit (s + 1) args
        else if check LexUtils.isHexDigit at s then
          loop_hexIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.HexIntegerLiteral (constStart, s))


      and loop_octIntegerLiteralExpectDigit s (args as {constStart}) =
        if is #"_" s then
          error
            { pos = slice (constStart, s)
            , what = "Invalid octal integer literal."
            , explain = SOME
                "After an underline, there needs to be at least one octal digit."
            }
        else if check LexUtils.isOctDigit at s then
          loop_octIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.OctalIntegerLiteral (constStart, s))


      and loop_octIntegerLiteralAfterDigit s (args as {constStart}) =
        if is #"_" s then
          loop_octIntegerLiteralExpectDigit (s + 1) args
        else if check LexUtils.isOctDigit at s then
          loop_octIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.OctalIntegerLiteral (constStart, s))


      and loop_binIntegerLiteralExpectDigit s (args as {constStart}) =
        if is #"_" s then
          error
            { pos = slice (constStart, s)
            , what = "Invalid binary integer literal."
            , explain = SOME
                "After an underline, there needs to be at least one binary digit."
            }
        else if check LexUtils.isBinDigit at s then
          loop_binIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.BinaryIntegerLiteral (constStart, s))


      and loop_binIntegerLiteralAfterDigit s (args as {constStart}) =
        if is #"_" s then
          loop_binIntegerLiteralExpectDigit (s + 1) args
        else if check LexUtils.isBinDigit at s then
          loop_binIntegerLiteralAfterDigit (s + 1) args
        else
          success (mk Token.BinaryIntegerLiteral (constStart, s))


      (** Immediately after the dot, we need to see at least one decimal digit *)
      and loop_floatLiteralAfterDot s (args as {constStart}) =
        if check LexUtils.isDecDigit at s then
          loop_floatLiteral (s + 1) args
        else
          error
            { pos = slice (constStart, s)
            , what = "Invalid float literal."
            , explain = SOME
                "After the dot, there needs to be at least one decimal digit."
            }


      (** Parsing the remainder of a float literal. This is already after the
        * dot, because the front of the float literal was already parsed as
        * an integer constant.
        *)
      and loop_floatLiteral s (args as {constStart}) =
        if check LexUtils.isDecDigit at s then
          loop_floatLiteral (s + 1) args
        else if is #"E" at s orelse is #"e" at s then
          loop_floatLiteralAfterExponent (s + 1) args
        else
          success (mk Token.FloatLiteral (constStart, s))


      (** Immediately after the "E" or "e", there might be + or -,
        * and then a bunch of decimal digits.
        *)
      and loop_floatLiteralAfterExponent s {constStart} =
        let
          fun walkExpectDigit i =
            if check LexUtils.isDecDigit at i then
              walk (i + 1)
            else
              error
                { pos = slice (constStart, i)
                , what = "Incomplete float literal."
                , explain = SOME "Expected to see a decimal digit next."
                }

          and walk i =
            if is #"_" at i then walkExpectDigit (i + 1)
            else if check LexUtils.isDecDigit at i then walk (i + 1)
            else i

          val s' = walkExpectDigit
            (if is #"-" at s orelse is #"+" at s then s + 1 else s)
        in
          success (mk Token.FloatLiteral (constStart, s'))
        end


      (** Might be start of a comment *)
      and loop_afterSlash s =
        if is #"*" at s then
          loop_inBlockComment (s + 1) {commentStart = s - 1, nesting = 1}
        else if is #"/" at s then
          loop_inLineComment (s + 1) {commentStart = s - 1}
        else
          success (mkr Token.Slash (s - 1, s))


      and loop_inLineComment s (args as {commentStart}) =
        if is #"\n" at s then
          success (mk Token.LineComment (commentStart, s))
        else if isEndOfFileAt s then
          success (mk Token.LineComment (commentStart, s))
        else
          loop_inLineComment (s + 1) args


      (** Inside a comment that started at `commentStart`
        * `nesting` is always >= 0 and indicates how many open-comments we've seen.
        *)
      and loop_inBlockComment s {commentStart, nesting} =
        if nesting = 0 then
          success (mk Token.BlockComment (commentStart, s))
        else if is #"/" at s andalso is #"*" at s + 1 then
          loop_inBlockComment (s + 2)
            {commentStart = commentStart, nesting = nesting + 1}
        else if is #"*" at s andalso is #"/" at s + 1 then
          loop_inBlockComment (s + 2)
            {commentStart = commentStart, nesting = nesting - 1}
        else if isEndOfFileAt s then
          error
            { pos = slice (commentStart, commentStart + 2)
            , what = "Unclosed comment."
            , explain = NONE
            }
        else
          loop_inBlockComment (s + 1)
            {commentStart = commentStart, nesting = nesting}

    in
      loop_topLevel startOffset
    end


  fun tokens src =
    let
      val startOffset = Source.absoluteStartOffset src
      val endOffset = Source.absoluteEndOffset src
      val src = Source.wholeFile src

      fun tokEndOffset tok =
        Source.absoluteEndOffset (Token.Pretoken.getSource tok)

      fun finish acc =
        Token.makeGroup (Seq.rev (Seq.fromList acc))

      fun loop acc offset =
        if offset >= endOffset then
          finish acc
        else
          case next (Source.drop src offset) of
            NONE => finish acc
          | SOME tok => loop (tok :: acc) (tokEndOffset tok)
    in
      loop [] startOffset
    end

end
