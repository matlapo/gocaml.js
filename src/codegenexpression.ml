open Batteries
open Ast
open Codegenutils

let rec codegen_unary_op (s: scope) (op: unary) (exp: exp gen_node) :string =
  let e = codegen_exp s true exp in
  match op with
    | Not -> "!" ^ e
    | UMinus -> "-" ^ e
    | UPlus -> "+" ^ e
    | UCaret -> "-1" ^ "^" ^ e
and codegen_binary_op (s: scope) (op: binary) (left: exp gen_node) (right: exp gen_node) :string =
  let l_type = type_of_expr s left in
  let r_type = type_of_expr s right in
  let int = l_type = Basetype BInt && r_type = Basetype BInt in
  let l = codegen_exp s true left in
  let r = codegen_exp s true right in
  match op with
    | Plus -> l ^ "+" ^ r
    | Minus -> l ^ "-" ^ r
    | Times -> l ^ "*" ^ r
    | Div -> if int then
        "Math.floor(" ^ l ^ "/" ^ r ^ ")"
      else
        l ^ "/" ^ r
    | Mod -> l ^ "%" ^ r
    | Equals ->
      (match l_type with
        | Struct _
        | Array _  -> "deepEqual(" ^ l ^ "," ^ r ^ ")"
        | _ -> l ^ "==" ^ r)
    | NotEquals ->
      (match l_type with
        | Struct _
        | Array _  -> "!deepEqual(" ^ l ^ "," ^ r ^ ")"
        | _ -> l ^ "!=" ^ r)
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
and codegen_bare_exp (s: scope) (p: bool) (e: exp) :string =
  let code = match e with
    | Id ref -> mangle_expr s ref
    | Indexing (array, index) ->
      let array_code = codegen_exp s p array in
      let index_code = codegen_exp s p index in
      if p then
        "at(" ^ array_code ^ "," ^ index_code ^ ")"
      else
        array_code ^ "[" ^ index_code ^ "]"
    | Selection (struct_expr, member) ->
      (codegen_exp s p struct_expr) ^
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
    | Rune r ->
      let replace a b s = String.nreplace ~str:s ~sub:a ~by:b in
      let unescaped = r
        |> replace "\\n" "\n"
        |> replace "\\\\" "\\"
        in
      string_of_int (BatChar.code (BatString.get unescaped 1))
    | BinaryOp (op, (left, right)) -> codegen_binary_op s op left right
    | Unaryexp (op, exp) -> codegen_unary_op s op exp
    | FuncCall (name, params) ->
      (mangle_fct name) ^
      "(" ^ (
        params
          |> List.map (codegen_copy s p)
          |> concat_comma
        ) ^
      ")"
    | Append (slice_expr, elt_expr) ->
      "append(" ^
        (codegen_exp s p slice_expr) ^
        "," ^
        (codegen_exp s p elt_expr) ^
      ")"
  in
  if p then (paren code) else code
and codegen_exp (s: scope) (parenthesize: bool) (exp: exp gen_node) :string = codegen_bare_exp s parenthesize (unwrap_gen_node exp)
and codegen_copy (s: scope) (parenthesize: bool) (expr: exp gen_node) :string =
  match type_of_expr s expr with
    | Array _ -> "[...(" ^ codegen_exp s parenthesize expr ^ ")]"
    | Struct _ -> "{...(" ^ codegen_exp s parenthesize expr ^ ")}"
    | _ -> codegen_exp s false expr
and codegen_bare_exps (s: scope) (parenthesize: bool) (exps: exp list): string =
  exps
    |> List.map (codegen_bare_exp s parenthesize)
    |> concat_comma
and codegen_exps (s: scope) (parenthesize: bool) (exps: exp gen_node list): string = codegen_bare_exps s parenthesize (List.map unwrap_gen_node exps)
