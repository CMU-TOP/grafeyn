structure ParseQASM:
sig
  exception ParseError of string
  val loadFromFile: string -> Circuit.t
end =
struct

  exception ParseError of string

  (* see e.g.
   * https://github.com/Qiskit/qiskit-terra/blob/main/qiskit/qasm/libs/qelib1.inc
   *)
  fun parseGate (name, args) =
    let
      val arity = Seq.length args
      fun getArg i = Seq.nth args i
    in
      case (name, arity) of
        ("h", 1) => Gate.Hadamard (getArg 0)
      | ("y", 1) => Gate.PauliY (getArg 0)
      | ("z", 1) => Gate.PauliZ (getArg 0)
      | ("t", 1) => Gate.T (getArg 0)
      | ("cx", 2) => Gate.CX {control = getArg 0, target = getArg 1}
      | _ =>
          raise ParseError
            ("unknown gate: " ^ name ^ " (arity: " ^ Int.toString arity ^ ")")
    end

  fun charSeqToString s =
    CharVector.tabulate (Seq.length s, Seq.nth s)

  type parser_state =
    {index: int, qreg: (string * int) option, gates: Gate.t list}

  fun index ({index = i, ...}: parser_state) = i

  fun advanceBy j ({index = i, qreg, gates}: parser_state) =
    {index = i + j, qreg = qreg, gates = gates}

  fun declareQReg (name, size) ({index, qreg, gates}: parser_state) =
    case qreg of
      NONE =>
        ( (*print ("declaring qreg: " ^ name ^ "[" ^ Int.toString size ^ "]\n")
          ;*){index = index, qreg = SOME (name, size), gates = gates})
    | SOME _ => raise ParseError "only one qreg supported at the moment"


  fun doGate (name, args) ({index, qreg, gates}: parser_state) =
    let in
      { index = index
      , qreg = qreg
      , gates = parseGate (name, Seq.map #index args) :: gates
      }
    end


  fun loadFromFile path =
    let
      val chars = ReadFile.contentsSeq path
      fun char i = Seq.nth chars i
      val numChars = Seq.length chars

      fun checkChar f state =
        index state < numChars andalso f (char (index state))


      fun isChar c state =
        checkChar (fn c' => c = c') state


      fun isString s state =
        index state + String.size s <= numChars
        andalso
        Util.all (0, String.size s) (fn j =>
          char (index state + j) = String.sub (s, j))


      fun goPastChar c state =
        if index state >= numChars then
          raise ParseError "unexpected end of file"
        else if isChar c state then
          advanceBy 1 state
        else
          goPastChar c (advanceBy 1 state)


      fun goUntilOrEndOfFile f state =
        if index state >= numChars then state
        else if f state then state
        else goUntilOrEndOfFile f (advanceBy 1 state)


      fun goPastWhitespace state =
        if checkChar Char.isSpace state then
          goPastWhitespace (advanceBy 1 state)
        else
          state


      fun expectChar c state =
        if isChar c state then
          advanceBy 1 state
        else
          raise ParseError
            ("expected " ^ Char.toString c ^ " but found: "
             ^ charSeqToString (Seq.drop chars (index state)))


      fun parse_toplevel state =
        let
          (* val _ = print "parse_toplevel\n" *)
          val state = goPastWhitespace state
        in
          if index state >= numChars then
            state

          else if isString "//" state then
            parse_toplevel (goPastChar #"\n" state)

          else if isString "OPENQASM" state then
            parse_toplevel (goPastChar #";" state)

          else if isString "include" state then
            let
              (* just ignore for now *)
              val (state, _) = parse_stringLiteral (advanceBy 7 state)
              val state = goPastChar #";" state
            in
              parse_toplevel state
            end

          else if isString "qreg" state then
            let
              val (state, {name, index = size}) = parse_nameWithIndex
                (advanceBy 4 state)
              val state = declareQReg (name, size) state
              val state = goPastChar #";" state
            in
              parse_toplevel state
            end

          else
            parse_toplevel (parse_gateAndArgs state)
        end


      and parse_gateAndArgs state =
        let
          (* val _ = print "parse_gateAndArgs\n" *)
          val state = goPastWhitespace state
          val (state, name) = parse_name state
          val (state, args) = parse_args state
          val state = goPastWhitespace state
          val state = expectChar #";" state

          val state = doGate (name, args) state
        in
          state
        end


      and parse_args state : parser_state * {name: string, index: int} Seq.t =
        let
          (* val _ = print "parse_args\n" *)
          fun loop acc state =
            let
              val state = goPastWhitespace state
              val (state, element) = parse_nameWithIndex state
              val acc = element :: acc
              val state = goPastWhitespace state
            in
              if isChar #";" state then (state, Seq.fromRevList acc)
              else if isChar #"," state then loop acc (advanceBy 1 state)
              else raise ParseError "failed to parse gate argument list"
            end
        in
          loop [] state
        end


      and parse_stringLiteral state =
        let
          (* val _ = print "parse_stringLiteral\n" *)
          val state = goPastWhitespace state
          val start = index state
          val state = expectChar #"\"" state
          val state = goPastChar #"\"" state
          val stop = index state
        in
          (state, Seq.subseq chars (start, stop - start))
        end


      and parse_nameWithIndex state =
        let
          (* val _ = print "parse_nameWithIndex\n" *)
          val state = goPastWhitespace state
          val (state, name) = parse_name state
          val state = goPastWhitespace state
          val state = expectChar #"[" state
          val (state, index) = parse_integer state
          val state = goPastWhitespace state
          val state = expectChar #"]" state
        in
          (state, {name = name, index = index})
        end


      and parse_name state =
        let
          (* val _ = print "parse_name\n" *)
          val state = goPastWhitespace state
          val start = index state
          val state =
            goUntilOrEndOfFile
              (fn s => isChar #"[" s orelse checkChar Char.isSpace s) state
          val stop = index state

          val name = charSeqToString (Seq.subseq chars (start, stop - start))
        in
          (state, name)
        end


      and parse_integer state =
        let
          (* val _ = print "parse_integer\n" *)
          val state = goPastWhitespace state
          val start = index state
          val state = goUntilOrEndOfFile (not o checkChar Char.isDigit) state
          val stop = index state

          val x =
            case Parse.parseInt (Seq.subseq chars (start, stop - start)) of
              NONE => raise ParseError "invalid integer"
            | SOME x => x
        in
          (state, x)
        end

      val state: parser_state = {index = 0, qreg = NONE, gates = []}
      val state = parse_toplevel state
    (* val _ = print ("got to: " ^ Int.toString (index state) ^ "\n") *)
    in
      case #qreg state of
        NONE => raise ParseError "no qreg declared"
      | SOME (_, numQubits) =>
          {numQubits = numQubits, gates = Seq.fromRevList (#gates state)}
    end
end
