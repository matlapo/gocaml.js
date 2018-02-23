module AstWithPosition : Ast =
  struct
    type 'a node = {position: Lexing.position; value: 'a}
  end