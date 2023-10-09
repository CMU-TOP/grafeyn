(* Copyright (c) 2023 Sam Westrick
 *
 * See the file LICENSE for details.
 *)

structure AstType =
struct

  (* ====================================================================== *)
  structure Exp =
  struct

    datatype exp =
      Identifier of Token.t

    | Literal of Token.t

    | Parens of {lparen: Token.t, exp: exp, rparen: Token.t}

    | UnaryOp of {opp: Token.t, exp: exp}

    | BinaryOp of {exp1: exp, opp: Token.t, exp2: exp}

    type t = exp

  end


  (* ====================================================================== *)
  structure Stmt =
  struct

    datatype operand =
      IndexedIdentifier of
        { name: Token.t
        , index: {lbracket: Token.t, exp: Exp.t, rbracket: Token.t} option
        }


    datatype stmt =
      GateCall of
        { name: Token.t
        , params:
            { lparen: Token.t
            , elems: Exp.t Seq.t
            , delims: Token.t Seq.t
            , rparen: Token.t
            } option
        , args: { elems: operand Seq.t
                (* commas. NOTE: final optional comma is permitted! *)
                , delims: Token.t Seq.t
                }
        , semicolon: Token.t
        }

    | Include of
        { includee: Token.t
        , path: Token.t (* string literal *)
        , semicolon: Token.t
        }

    (* qreg x;
     * qreg y[10];
     * creg z[1]; 
     *)
    | OldStyleDeclare of
        { reg: Token.t (* `creg` or `qreg` *)
        , ident: Token.t
        , designator: {lbracket: Token.t, exp: Exp.t, rbracket: Token.t} option
        , semicolon: Token.t
        }

    (* qubit x;
     * qubit[10] y;
     *)
    | Declare of
        { qubit: Token.t (* `qubit` *)
        , designator: {lbracket: Token.t, exp: Exp.t, rbracket: Token.t} option
        , ident: Token.t
        , semicolon: Token.t
        }

    | Measure of
        { measure: Token.t (* `measure` *)
        , arg: operand
        , into: {arrow: Token.t, arg: operand} option
        , semicolon: Token.t
        }

    | Barrier of
        { barrier: Token.t (* `barrier` *)
        , elems: operand Seq.t
        , delims: Token.t Seq.t (* commas *)
        , semicolon: Token.t
        }

    | Reset of {reset: Token.t (* `reset` *), arg: operand, semicolon: Token.t}


    type t = stmt

  end


  (* ====================================================================== *)
  datatype ast =
    Ast of
      { version: {openqasm: Token.t, version: Token.t, semicolon: Token.t} option
      , stmts: Stmt.t Seq.t
      }

  type t = ast

end
