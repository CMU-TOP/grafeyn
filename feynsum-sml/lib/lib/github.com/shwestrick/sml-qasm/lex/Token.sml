(** Copyright (c) 2020-2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(* https://openqasm.com/grammar/index.html *)

structure Token :>
sig

  datatype reserved =
    Openqasm
  | Include
  | Def
  | Gate
  | Qreg
  | Qubit
  | Creg
  | Reset
  | Measure
  | Barrier
  | OpenBracket (* [ *)
  | CloseBracket (* ] *)
  | OpenParen (* ( *)
  | CloseParen (* ) *)
  | OpenBrace (* { *)
  | CloseBrace (* } *)
  | Colon (* : *)
  | Semicolon (* ; *)
  | Comma (* , *)
  | Equals (* = *)
  | Arrow (* -> *)
  | Slash (* / *)
  | Minus (* - *)
  | Asterisk (* * *)
  | Plus (* + *)

  (*
  | DoublePlus (* ++ *)
  | DoubleAsterisk (* ** *)
  | Percent (* % *)
  | Pipe (* | *)
  | DoublePipe (* || *)
  | Ampersand (* & *)
  | DoubleAmpersand (* && *)
  | Caret (* ^ *)
  | At (* @ *)
  | Tilde (* ~ *)
  | Exclamation (* ! *)
  | DoubleEq (* == *)
  | NotEq (* != *)
  | PlusEq (* += *)
  | MinusEq (* -= *)
  | MultEq (* *= *)
  | DivEq (* /= *)
  | AndEq (* &= *)
  | (* TODO... *)
  *)

  datatype class =
    LineComment
  | BlockComment
  | Reserved of reserved
  | BinaryIntegerLiteral
  | OctalIntegerLiteral
  | DecimalIntegerLiteral
  | HexIntegerLiteral
  | ImaginaryLiteral
  | FloatLiteral
  | StringLiteral
  | TimingLiteral
  | BooleanLiteral
  | HardwareQubit
  | Identifier
  | Whitespace

  type token
  type t = token

  val same: token * token -> bool
  val sameExceptForMultilineIndentation: {tabWidth: int}
                                         -> token * token
                                         -> bool

  val getClass: token -> class
  val getSource: token -> Source.t

  val toString: token -> string
  val reservedToString: reserved -> string
  val classToString: class -> string

  val nextToken: token -> token option
  val prevToken: token -> token option
  val nextTokenNotCommentOrWhitespace: token -> token option
  val prevTokenNotCommentOrWhitespace: token -> token option
  val hasCommentsAfter: token -> bool
  val hasCommentsBefore: token -> bool
  val commentsBefore: token -> token Seq.t
  val commentsAfter: token -> token Seq.t
  val commentsOrWhitespaceBefore: token -> token Seq.t
  val commentsOrWhitespaceAfter: token -> token Seq.t

  (** how many lines apart are these tokens?
    * raises Fail if not from the same file.
    *)
  val lineDifference: token * token -> int
  val effectiveOffset: {tabWidth: int} -> token -> int
  val spansMultipleLines: token -> bool

  val isReserved: token -> bool
  val isStringLiteral: token -> bool
  val isIdentifier: token -> bool
  val isLiteral: token -> bool
  val isComment: token -> bool
  val isWhitespace: token -> bool
  val isCommentOrWhitespace: token -> bool

  val isVersionIdentifier: token -> bool

  (** Tokens have to be constructed from a group of "pretokens". This makes it
    * possible to implement functions like nextToken and prevToken, above.
    *)
  structure Pretoken:
  sig
    type t
    type pretoken = t

    val getSource: pretoken -> Source.t
    val getClass: pretoken -> class

    val make: Source.t -> class -> pretoken
    val reserved: Source.t -> reserved -> pretoken
    val identifier: Source.t -> pretoken
    val reservedOrIdentifier: Source.t -> pretoken
  end

  val fromPre: Pretoken.t -> token
  val makeGroup: Pretoken.t Seq.t -> token Seq.t

end =
struct

  datatype reserved =
    Openqasm
  | Include
  | Def
  | Gate
  | Qreg
  | Qubit
  | Creg
  | Reset
  | Measure
  | Barrier
  | OpenBracket (* [ *)
  | CloseBracket (* ] *)
  | OpenParen (* ( *)
  | CloseParen (* ) *)
  | OpenBrace (* { *)
  | CloseBrace (* } *)
  | Colon (* : *)
  | Semicolon (* ; *)
  | Comma (* , *)
  | Equals (* = *)
  | Arrow (* -> *)
  | Slash (* / *)
  | Minus (* - *)
  | Asterisk (* * *)
  | Plus (* + *)

  datatype class =
    LineComment
  | BlockComment
  | Reserved of reserved
  | BinaryIntegerLiteral
  | OctalIntegerLiteral
  | DecimalIntegerLiteral
  | HexIntegerLiteral
  | ImaginaryLiteral
  | FloatLiteral
  | StringLiteral
  | TimingLiteral
  | BooleanLiteral
  | HardwareQubit
  | Identifier
  | Whitespace

  type pretoken = class WithSource.t

  type token = {idx: int, context: pretoken Seq.t}
  type t = token

  fun make src class = WithSource.make {value = class, source = src}

  fun reserved src rclass =
    WithSource.make {value = Reserved rclass, source = src}

  fun identifier src = WithSource.make {value = Identifier, source = src}

  fun getClass ({idx, context}: token) =
    WithSource.valOf (Seq.nth context idx)

  fun getSource ({idx, context}: token) =
    WithSource.srcOf (Seq.nth context idx)

  fun lineDifference (tok1, tok2) =
    let
      val src1 = getSource tok1
      val src2 = getSource tok2
      val {line = end1, ...} = Source.absoluteEnd (getSource tok1)
      val {line = start2, ...} = Source.absoluteStart (getSource tok2)
    in
      if FilePath.sameFile (Source.fileName src1, Source.fileName src2) then
        start2 - end1
      else
        raise Fail "Bug! lineDifference on tokens from different files"
    end

  fun spansMultipleLines tok =
    let
      val {line = lnStart, ...} = Source.absoluteStart (getSource tok)
      val {line = lnEnd, ...} = Source.absoluteEnd (getSource tok)
    in
      lnEnd <> lnStart
    end

  fun toString tok =
    let val src = getSource tok
    in CharVector.tabulate (Source.length src, Source.nth src)
    end

  fun tryReserved src =
    let
      val str = CharVector.tabulate (Source.length src, Source.nth src)
      fun r rclass = SOME rclass
    in
      case str of
        "OPENQASM" => r Openqasm
      | "include" => r Include
      | "def" => r Def
      | "gate" => r Gate
      | "qreg" => r Qreg
      | "qubit" => r Qubit
      | "creg" => r Creg
      | "reset" => r Reset
      | "measure" => r Measure
      | "barrier" => r Barrier
      | "[" => r OpenBracket
      | "]" => r CloseBracket
      | "(" => r OpenParen
      | ")" => r CloseParen
      | "{" => r OpenBrace
      | "}" => r CloseBrace
      | ":" => r Colon
      | ";" => r Semicolon
      | "," => r Comma
      | "=" => r Equals
      | "->" => r Arrow
      | "/" => r Slash
      | "-" => r Minus
      | "*" => r Asterisk
      | "+" => r Plus

      | _ => NONE (* (print ("not reserved: " ^ other ^ "\n"); NONE) *)
    end

  fun reservedToString rc =
    case rc of
      Openqasm => "OPENQASM"
    | Include => "include"
    | Def => "def"
    | Gate => "gate"
    | Qreg => "qreg"
    | Qubit => "qubit"
    | Creg => "creg"
    | Reset => "reset"
    | Measure => "measure"
    | Barrier => "barrier"
    | OpenBracket => "["
    | CloseBracket => "]"
    | OpenParen => "("
    | CloseParen => ")"
    | OpenBrace => "{"
    | CloseBrace => "}"
    | Colon => ":"
    | Semicolon => ";"
    | Comma => ","
    | Equals => "="
    | Arrow => "->"
    | Slash => "/"
    | Minus => "-"
    | Asterisk => "*"
    | Plus => "+"


  fun reservedOrIdentifier src =
    case tryReserved src of
      SOME r => reserved src r
    | NONE => identifier src

  fun isReserved (tok: token) =
    case getClass tok of
      Reserved _ => true
    | _ => false

  fun isIdentifier tok =
    case getClass tok of
      Identifier => true
    | _ => false

  fun isLiteral tok =
    case getClass tok of
      BinaryIntegerLiteral => true
    | OctalIntegerLiteral => true
    | DecimalIntegerLiteral => true
    | HexIntegerLiteral => true
    | ImaginaryLiteral => true
    | FloatLiteral => true
    | StringLiteral => true
    | TimingLiteral => true
    | BooleanLiteral => true
    | _ => false

  fun isStringLiteral tok =
    case getClass tok of
      StringLiteral => true
    | _ => false

  fun isComment tok =
    case getClass tok of
      LineComment => true
    | BlockComment => true
    | _ => false

  fun isBlockComment tok =
    case getClass tok of
      BlockComment => true
    | _ => false

  fun isWhitespace tok =
    case getClass tok of
      Whitespace => true
    | _ => false

  fun isCommentOrWhitespace tok = isComment tok orelse isWhitespace tok

  fun isVersionIdentifier tok =
    let
      val src = getSource tok

      fun okayClass () =
        case getClass tok of
          FloatLiteral => true
        | DecimalIntegerLiteral => true
        | _ => false

      fun okayStuff () =
        Source.length src >= 1 andalso LexUtils.isDecDigit (Source.nth src 0)
        andalso
        Util.all (0, Source.length src) (fn i =>
          let val c = Source.nth src i
          in LexUtils.isDecDigit c orelse c = #"."
          end)
    in
      okayClass () andalso okayStuff ()
    end


  fun classToString class =
    case class of
      LineComment => "line comment"
    | BlockComment => "block comment"
    | Reserved _ => "reserved"
    | BinaryIntegerLiteral => "binary integer literal"
    | OctalIntegerLiteral => "octal integer literal"
    | DecimalIntegerLiteral => "decimal integer literal"
    | HexIntegerLiteral => "hex integer literal"
    | ImaginaryLiteral => "imaginary literal"
    | FloatLiteral => "float literal"
    | StringLiteral => "string literal"
    | TimingLiteral => "timing literal"
    | BooleanLiteral => "boolean literal"
    | HardwareQubit => "hardware qubit"
    | Identifier => "identifier"
    | Whitespace => "whitespace"


  (** effective offset of the beginning of this token within its line,
    * counting tab-widths appropriately.
    *)
  fun effectiveOffset {tabWidth: int} tok =
    let
      val src = getSource tok
      val {col, line = lineNum} = Source.absoluteStart src
      val len = col - 1
      val charsBeforeOnSameLine = Source.take (Source.wholeLine src lineNum) len
      fun loop effOff i =
        if i >= len then
          effOff
        else if #"\t" = Source.nth charsBeforeOnSameLine i then
          (* advance up to next tabstop *)
          loop (effOff + tabWidth - effOff mod tabWidth) (i + 1)
        else
          loop (effOff + 1) (i + 1)
    in
      loop 0 0
    end


  (** Check that the text of t1 exactly matches the text of t2. Useful for
    * comparing identifier names, e.g. for infix lookup.
    *)
  fun same (t1, t2) =
    let
      val s1 = getSource t1
      val s2 = getSource t2
      val n = Source.length s1

      fun loop i =
        if i >= n then true
        else (Source.nth s1 i = Source.nth s2 i) andalso loop (i + 1)
    in
      n = Source.length s2 andalso loop 0
    end


  fun piecesAsStrings {tabWidth} tok =
    let
      val effectiveOffset = effectiveOffset {tabWidth = tabWidth} tok

      fun strip line =
        let
          val {result, ...} =
            StripEffectiveWhitespace.strip
              {tabWidth = tabWidth, removeAtMost = effectiveOffset} line
        in
          result
        end

      val src = getSource tok
      val asString = CharVector.tabulate (Source.length src, Source.nth src)
    in
      Seq.map (fn (i, j) => strip (String.substring (asString, i, j - i)))
        (Source.lineRanges src)
    end


  (** Check that t1 and t2 have exactly the same text, similar to function
    * `same` above, except that this function also handles the possibility of a
    * multiline token (i.e. a multiline block comment) whose
    * indentation might be different between t1 and t2 but otherwise is
    * identical.
    *)
  fun sameExceptForMultilineIndentation {tabWidth: int} (t1, t2) =
    if not (isBlockComment t1 orelse isBlockComment t2) then
      same (t1, t2)
    else
      Seq.equal op=
        ( piecesAsStrings {tabWidth = tabWidth} t1
        , piecesAsStrings {tabWidth = tabWidth} t2
        )

  fun makeGroup (s: pretoken Seq.t) : token Seq.t =
    Seq.tabulate (fn i => {idx = i, context = s}) (Seq.length s)

  fun fromPre (t: pretoken) =
    Seq.nth (makeGroup (Seq.singleton t)) 0

  fun nextToken ({idx = i, context}: token) =
    if i + 1 < Seq.length context then SOME {idx = i + 1, context = context}
    else NONE

  fun prevToken ({idx = i, context}: token) =
    if i > 0 then SOME {idx = i - 1, context = context} else NONE

  fun prevTokenNotCommentOrWhitespace tok =
    case prevToken tok of
      NONE => NONE
    | SOME t' =>
        if isCommentOrWhitespace t' then prevTokenNotCommentOrWhitespace t'
        else SOME t'

  fun nextTokenNotCommentOrWhitespace tok =
    case nextToken tok of
      NONE => NONE
    | SOME t' =>
        if isCommentOrWhitespace t' then nextTokenNotCommentOrWhitespace t'
        else SOME t'

  fun commentsOrWhitespaceBefore tok =
    let
      fun loop acc t =
        case prevToken t of
          SOME t' =>
            if isCommentOrWhitespace t' then loop (t' :: acc) t' else acc
        | NONE => acc
    in
      Seq.fromList (loop [] tok)
    end

  fun commentsOrWhitespaceAfter tok =
    let
      fun loop acc t =
        case nextToken t of
          SOME t' =>
            if isCommentOrWhitespace t' then loop (t' :: acc) t' else acc
        | NONE => acc
    in
      Seq.fromRevList (loop [] tok)
    end


  fun hasCommentsBefore t =
    case prevToken t of
      SOME t' =>
        isComment t' orelse (isWhitespace t' andalso hasCommentsBefore t')
    | NONE => false


  fun hasCommentsAfter t =
    case nextToken t of
      SOME t' =>
        isComment t' orelse (isWhitespace t' andalso hasCommentsAfter t')
    | NONE => false


  fun commentsBefore tok =
    let
      fun loop acc t =
        case prevToken t of
          SOME t' =>
            if isWhitespace t' then loop acc t'
            else if isComment t' then loop (t' :: acc) t'
            else acc
        | NONE => acc
    in
      Seq.fromList (loop [] tok)
    end


  fun commentsAfter tok =
    let
      fun loop acc t =
        case nextToken t of
          SOME t' =>
            if isWhitespace t' then loop acc t'
            else if isComment t' then loop (t' :: acc) t'
            else acc
        | NONE => acc
    in
      Seq.fromRevList (loop [] tok)
    end


  structure Pretoken =
  struct
    type t = pretoken
    type pretoken = pretoken

    fun getSource p = WithSource.srcOf p
    fun getClass p = WithSource.valOf p

    val make = make
    val reserved = reserved
    val identifier = identifier
    val reservedOrIdentifier = reservedOrIdentifier
  end

end
