structure ParseQASM:
sig
  exception ParseError of string
  val loadFromFile: string -> Circuit.t
end =
struct

  exception ParseError of string

  structure RealExp =
  struct
    datatype t = LiteralPi | Slash of t * t | LiteralNum of char Seq.t

    fun eval exp =
      case exp of
        LiteralPi => Math.pi
      | Slash (e1, e2) => eval e1 / eval e2
      | LiteralNum x =>
          case Parse.parseReal x of
            NONE =>
              raise ParseError
                ("could not convert to real: "
                 ^ (CharVector.tabulate (Seq.length x, Seq.nth x)))
          | SOME r => r
  end

  (* Examples:
   *   h              GateName("h")
   *   cx             GateName("cx")
   *   cphase(pi/2)   GateNameAndArg("cphase", Slash(LiteralPi, LiteralNum "2"))
   *)
  datatype gate_desc =
    GateName of string
  | GateNameAndArg of string * RealExp.t

  (* see e.g.
   * https://github.com/Qiskit/qiskit-terra/blob/main/qiskit/qasm/libs/qelib1.inc
   *)
  fun parseGate (gateDesc, args) =
    let
      val arity = Seq.length args
      fun getArg i = Seq.nth args i

      fun err name =
        raise ParseError
          ("unknown gate: " ^ name ^ " (arity: " ^ Int.toString arity ^ ")")
    in
      case (gateDesc, arity) of
        (GateName "h", 1) => Gate.Hadamard (getArg 0)
      | (GateName "y", 1) => Gate.PauliY (getArg 0)
      | (GateName "z", 1) => Gate.PauliZ (getArg 0)
      | (GateName "t", 1) => Gate.T (getArg 0)
      | (GateName "x", 1) => Gate.X (getArg 0)
      | (GateName "cx", 2) => Gate.CX {control = getArg 0, target = getArg 1}
      | (GateNameAndArg ("cphase", realexp), 2) =>
          Gate.CPhase
            {control = getArg 0, target = getArg 1, rot = RealExp.eval realexp}
      | (GateName name, _) => err name
      | (GateNameAndArg (name, _), _) => err name
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
          (* val _ = print ("parse_toplevel " ^ Int.toString (index state) ^ "\n") *)
          val state = goPastWhitespace state
        in
          if index state >= numChars then
            state

          else if isString "//" state then
            parse_toplevel (advanceBy 1
              (goUntilOrEndOfFile (isChar #"\n") state))

          else if isString "OPENQASM" state then
            parse_toplevel (goPastChar #";" state)

          else if isString "reset" state then
            let
              val state = advanceBy 5 state
              val (state, _) = parse_name state
              val state = goPastWhitespace state
              val state = expectChar #";" state
            in
              parse_toplevel state
            end

          else if isString "barrier" state then
            let
              val state = advanceBy 7 state
              val (state, _) = parse_name state
              val state = goPastWhitespace state
              val state = expectChar #";" state
            in
              parse_toplevel state
            end

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
          val (state, gateDesc) = parse_gateDesc state
          val (state, args) = parse_args state
          val state = goPastWhitespace state
          val state = expectChar #";" state

          val state = doGate (gateDesc, args) state
        in
          state
        end


      and parse_gateDesc state =
        let
          val state = goPastWhitespace state
          val (state, name) = parse_name state
          val state = goPastWhitespace state
        in
          if not (isChar #"(" state) then
            (state, GateName name)
          else
            let
              val state = advanceBy 1 state
              val (state, exp) = parse_realExp state
              val state = goPastWhitespace state
              val state = expectChar #")" state
            in
              (state, GateNameAndArg (name, exp))
            end
        end


      (* TODO: this is NOT good. Mega hack. *)
      and parse_realExp state =
        let
          fun loop acc state =
            let
              val state = goPastWhitespace state
            in
              if index state >= numChars then
                raise ParseError "unexpected end of real expression"
              else if isChar #")" state then
                (state, acc)
              else if isString "pi" state then
                if Option.isSome acc then
                  raise ParseError ("could not parse real expression")
                else
                  loop (SOME RealExp.LiteralPi) (advanceBy 2 state)
              else if isChar #"/" state then
                let
                  val state = advanceBy 1 state
                  val (state, yy) = parse_realExp state
                  val acc =
                    case acc of
                      NONE =>
                        raise ParseError ("could not parse real expression")
                    | SOME xx => SOME (RealExp.Slash (xx, yy))
                in
                  loop acc state
                end
              else if checkChar Char.isDigit state then
                let
                  val start = index state
                  (* TODO: this is NOT good. Mega hack. *)
                  val state =
                    goUntilOrEndOfFile
                      (fn s =>
                         not (checkChar Char.isDigit s orelse isChar #"." s))
                      state
                  val stop = index state
                  val stuff = Seq.subseq chars (start, stop - start)
                in
                  if Option.isSome acc then
                    raise ParseError ("could not parse real expression")
                  else
                    loop (SOME (RealExp.LiteralNum stuff)) state
                end
              else
                raise ParseError ("count not parse real expression!")
            end

          val (state, realExp) = loop NONE state
        in
          case realExp of
            NONE => raise ParseError ("could not parse real expression")
          | SOME re => (state, re)
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
              (fn s =>
                 isChar #"(" s orelse isChar #";" s orelse isChar #"[" s
                 orelse checkChar Char.isSpace s) state
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
