(* Copyright (c) 2023 Sam Westrick
 *
 * See the file LICENSE for details.
 *)

structure Ast =
struct
  open AstType

  structure Exp =
  struct
    open AstType.Exp

    fun toString e =
      case e of
        Identifier id => Token.toString id
      | Literal x => Token.toString x
      | Parens {exp, ...} => "(" ^ toString exp ^ ")"
      | UnaryOp {opp, exp} => Token.toString opp ^ "(" ^ toString exp ^ ")"
      | BinaryOp {exp1, opp, exp2} =>
          "(" ^ toString exp1 ^ " " ^ Token.toString opp ^ " " ^ toString exp2
          ^ ")"

  end


  structure Stmt =
  struct
    open AstType.Stmt

    fun toString s =
      case s of
        GateCall {name, params, args, ...} =>
          let
            val n = Token.toString name
            val p =
              case params of
                NONE => ""
              | SOME {elems, ...} =>
                  if Seq.length elems = 0 then
                    "()"
                  else
                    "("
                    ^
                    Seq.iterate (fn (acc, e) => acc ^ ", " ^ Exp.toString e)
                      (Exp.toString (Seq.nth elems 0)) (Seq.drop elems 1) ^ ")"
          in
            n ^ p ^ " " ^ "<args>;\n"
          end

      | _ => "<stmt>;\n"
  end


  fun toString (Ast {version, stmts}) =
    let
      val top =
        case version of
          NONE => ""
        | SOME {version, ...} => "OPENQASM " ^ Token.toString version ^ ";\n"
    in
      Seq.iterate (fn (acc, stmt) => acc ^ Stmt.toString stmt) top stmts
    end

end
