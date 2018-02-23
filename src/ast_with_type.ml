module AstWithType : Ast =
  struct
    type 'a node = {position: Lexing.position; value: 'a; exp_type: Ast.type}
  end