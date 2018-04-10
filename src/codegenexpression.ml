open Batteries
open Ast
open Codegenutils

let rec codegen_unary_op (op: unary) (exp: exp gen_node) :string =
  let e = codegen_exp true exp in
  let code = match op with
    | Not -> "!" ^ e
    | UMinus -> "-" ^ e
    | UPlus -> "+" ^ e
    | UCaret -> "-1" ^ "^" ^ e
  in
  paren code
and codegen_binary_op (op: binary) (left: exp gen_node) (right: exp gen_node) :string =
  let l = codegen_exp true left in
  let r = codegen_exp true right in
  match op with
    | Plus -> l ^ "+" ^ r
    | Minus -> l ^ "-" ^ r
    | Times -> l ^ "*" ^ r
    | Div -> l ^ "/" ^ r
    | Mod -> l ^ "%" ^ r
    | Equals -> l ^ "==" ^ r
    | NotEquals -> l ^ "!=" ^ r
    | And -> l ^ "&&" ^ r
    | Or -> l ^ "||" ^ r
    | Smaller -> l ^ "<" ^ r
    | Greater -> l ^ ">" ^ r
    | SmallerEq -> l ^ "<=" ^ r
    | GreaterEq -> l ^ ">=" ^ r
    | DGreater -> l ^ ">>" ^ r
    | DSmaller -> l ^ "<<" ^ r
    | AndHat -> l ^ "& ~" ^ r
    | BAnd -> l ^ "&" ^ r
    | BOr -> l ^ "|" ^ r
    | Caret -> l ^ "^" ^ r
and codegen_bare_exp (p: bool) (e: exp) :string =
  let code = match e with
    | Id ref -> mangle ref
    | Indexing (array, index) ->
      let array_code = codegen_exp p array in
      let index_code = codegen_exp p index in
      "at(" ^ array_code ^ "," ^ index_code ^ ")"
    | Selection (struct_expr, member) ->
      (codegen_exp p struct_expr) ^
      "." ^
      member
    | Int i -> Int64.to_string i
    | Float f -> string_of_float f
    | String s -> s
    | RawStr s ->
      let replace a b s = String.nreplace ~str:s ~sub:a ~by:b in
      s
        |> replace "\\" "\\\\"
        |> replace "\n" "\\n"
    | Rune r -> r
    | BinaryOp (op, (left, right)) -> codegen_binary_op op left right
    | Unaryexp (op, exp) -> codegen_unary_op op exp
    | FuncCall (name, params) -> (mangle name) ^ "(" ^ (codegen_exps p params) ^ ")"
    | Append (slice_expr, elt_expr) ->
      "append(" ^
      (codegen_exp p slice_expr) ^
      "," ^
      (codegen_exp p elt_expr) ^
      ")"
  in
  if p then (paren code) else code
and codegen_exp (parenthesize: bool) (exp: exp gen_node) :string = codegen_bare_exp parenthesize (unwrap_gen_node exp)
and codegen_bare_exps (parenthesize: bool) (exps: exp list): string =
  exps
    |> List.map (codegen_bare_exp parenthesize)
    |> concat_comma
and codegen_exps (parenthesize: bool) (exps: exp gen_node list): string = codegen_bare_exps parenthesize (List.map unwrap_gen_node exps)
