(** Copyright (c) 2023 Sam Westrick                                            
  *                                                                                 
  * See the file LICENSE for details.                                            
  *)

structure SimpleCircuit =
struct

  type qubit_idx = int
  type stmt =
    {gateName: string, params: Ast.Exp.t Seq.t option, args: qubit_idx Seq.t}

  type circuit = {numQubits: int, gates: stmt Seq.t}
  type t = circuit

  (* ===================================================================== *)

  fun toString ({numQubits, gates}: circuit) =
    let
      val header = "qreg q[" ^ Int.toString numQubits ^ "];\n"

      fun qi i =
        "q[" ^ Int.toString i ^ "]"

      fun gateToString {gateName, params, args} =
        let
          val pstr =
            case params of
              NONE => ""
            | SOME xx =>
                if Seq.length xx = 0 then
                  "()"
                else
                  "("
                  ^
                  Seq.iterate (fn (acc, e) => acc ^ ", " ^ Ast.Exp.toString e)
                    (Ast.Exp.toString (Seq.nth xx 0)) (Seq.drop xx 1) ^ ")"

          val front = gateName ^ pstr

          val args =
            if Seq.length args = 0 then
              ""
            else
              Seq.iterate (fn (acc, i) => acc ^ ", " ^ qi i)
                (qi (Seq.nth args 0)) (Seq.drop args 1)
        in
          front ^ " " ^ args ^ ";"
        end
    in
      Seq.iterate op^ header (Seq.map (fn g => gateToString g ^ "\n") gates)
    end

  (* ===================================================================== *)


  fun fromAst (Ast.Ast {stmts, ...}) : circuit =
    let
      fun fail msg =
        raise Fail ("SimpleCircuit.fromAst: " ^ msg)

      fun integerFromLiteral x =
        case Token.getClass x of
          Token.DecimalIntegerLiteral =>
            let
              val str = Token.toString x
              val nice = String.implode
                (List.filter (fn c => c <> #"_") (String.explode str))
            in
              case Int.fromString nice of
                NONE => fail ("integerFromLiteral: error parsing '" ^ str ^ "'")
              | SOME x => x
            end

        | c =>
            fail ("integerFromLiteral: unsupported: " ^ Token.classToString c)


      type fromAst_state =
        { numQubitsSoFar: int
        , qreg: {name: string, start: int, stop: int} list
        , stmts: stmt list
        }


      fun declareQReg (name, size) (state: fromAst_state) =
        if List.exists (fn x => #name x = name) (#qreg state) then
          fail ("qreg '" ^ name ^ "' declared more than once")
        else
          { numQubitsSoFar = #numQubitsSoFar state + size
          , qreg =
              { name = name
              , start = #numQubitsSoFar state
              , stop = #numQubitsSoFar state + size
              } :: #qreg state
          , stmts = #stmts state
          }


      fun indexFromDesignator designator =
        case designator of
          NONE => fail ("unsupported gate arg: missing qubit index")
        | SOME {exp, ...} =>
            case exp of
              Ast.Exp.Literal x =>
                let
                  val sz = integerFromLiteral x
                in
                  if sz < 0 then fail ("invalid gate arg: negative qubit index")
                  else sz
                end

            | _ => fail ("unsupported gate arg: non-literal size")


      fun lookupQubits (state: fromAst_state) (args: Ast.Stmt.operand Seq.t) =
        let
          fun toNameIndex (Ast.Stmt.IndexedIdentifier {name, index}) =
            let
              val name = Token.toString name
              val index = indexFromDesignator index
            in
              (name, index)
            end

          fun lookupOne (name, index) =
            case List.find (fn x => #name x = name) (#qreg state) of
              NONE => fail ("unknown qreg '" ^ name ^ "'")
            | SOME {start, stop, ...} =>
                if index < 0 orelse index >= (stop - start) then
                  fail
                    ("out-of-bounds: " ^ name ^ "[" ^ Int.toString index ^ "]")
                else
                  start + index
        in
          Seq.map (lookupOne o toNameIndex) args
        end


      fun addStmt stmt (state: fromAst_state) =
        { qreg = #qreg state
        , numQubitsSoFar = #numQubitsSoFar state
        , stmts = stmt :: #stmts state
        }


      fun sizeFromDesignator designator =
        case designator of
          NONE => fail ("qubit/qreg/creg declaration: missing size")
        | SOME {exp, ...} =>
            case exp of
              Ast.Exp.Literal x =>
                let
                  val sz = integerFromLiteral x
                in
                  if sz <= 0 then
                    fail ("qubit/qreg/creg declaration: invalid size")
                  else
                    sz
                end

            | _ =>
                fail
                  ("unsupported qubit/qreg/creg declaration: non-literal size")


      fun doStmt (state, s) =
        case s of
          Ast.Stmt.OldStyleDeclare {reg, ident, designator, ...} =>
            let
              val isQreg =
                case Token.getClass reg of
                  Token.Reserved Token.Qreg => true
                | _ => false
              val name = Token.toString ident
              val size = sizeFromDesignator designator
            in
              if not isQreg then state else declareQReg (name, size) state
            end

        | Ast.Stmt.Declare {designator, ident, ...} =>
            let
              val name = Token.toString ident
              val size = sizeFromDesignator designator
            in
              declareQReg (name, size) state
            end

        | Ast.Stmt.GateCall {name, params, args, ...} =>
            let
              val stmt =
                { gateName = Token.toString name
                , params =
                    case params of
                      NONE => NONE
                    | SOME {elems, ...} => SOME elems
                , args = lookupQubits state (#elems args)
                }
            in
              addStmt stmt state
            end

        | _ => state

      val initialState = {numQubitsSoFar = 0, qreg = [], stmts = []}
      val {numQubitsSoFar, stmts, ...} = Seq.iterate doStmt initialState stmts
    in
      {numQubits = numQubitsSoFar, gates = Seq.fromRevList stmts}
    end
end
