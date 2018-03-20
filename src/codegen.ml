open Astwithposition

let concat_map (f: 'a -> string) (l: 'a list) :string = l |> List.map f |> List.fold_left (^) ""

let unwrap_gen_node (node:'a gen_node) :'a = match node with
  | Position { value=v } -> v
  | Typed { value=v } -> v
  | Scoped { value=v } -> v

let paren (code: string) :string = "(" ^ code ^ ")"

let rec codegen_unary_op (op: unary) (exp: exp gen_node) :string =
  let e = codegen_exp exp in
  let code = match op with
    | Not -> "!" ^ e
    | UMinus -> "-" ^ e
    | UPlus -> "+" ^ e
    | UCaret -> "-1" ^ "^" ^ e
  in
  paren code
and codegen_binary_op (op: binary) (left: exp gen_node) (right: exp gen_node) :string =
  let l = codegen_exp left in
  let r = codegen_exp right in
  let code = match op with
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
    | AndHat -> "(undefined /* UNSUPPORTED_AND_HAT */)"
    | BAnd -> l ^ "&" ^ r
    | BOr -> l ^ "|" ^ r
    | Caret -> l ^ "^" ^ r
  in
  paren code
and codegen_exp (exp: exp gen_node) :string =
  let e = unwrap_gen_node exp in
  let code = match e with
    | Id _ -> "(undefined /* UNIMPLEMENTED_EXP_ID */)"
    | Int i -> Int64.to_string i
    | Float f -> string_of_float f
    | String s -> s
    | RawStr s -> "('' /* UNIMPLEMENTED_RAW_STRING */)"
    | Rune r -> r
    | Bool b -> string_of_bool b
    | BinaryOp (op, (left, right)) -> codegen_binary_op op left right
    | Unaryexp (op, exp) -> codegen_unary_op op exp
    | FuncCall (name, params) -> "('' /* UNIMPLEMENTED_FUNC_CALL */)"
    | Append (l, v) -> "('' /* UNIMPLEMENTED_APPEND */)"
  in
  paren code

let codegen_exps (exps: exp gen_node list): string = exps
  |> List.map codegen_exp
  |> List.fold_left
    (fun acc elt -> match acc with
      | "" -> elt
      | _ -> acc ^ "," ^ elt
    )
    ""

let codegen_stmt (stmt:stmt) :string = match stmt with
  | Print exps -> "print(" ^ codegen_exps exps ^ ");"
  | Println exps -> "println(" ^ codegen_exps exps ^ ");"
  | Return _ -> "return;"
  | s -> "/* UNIMPLEMENTED_STMT */"

let codegen_decl (decl:decl) :string = match decl with
  | Type _ -> ""
  | Fct (name, [], None, stmts) ->
    "function " ^
    name ^
    "(){" ^
    (stmts |> List.map unwrap_gen_node |> concat_map codegen_stmt)
    ^ "}"
  | _ -> "/* UNIMPLEMENTED_DECL */"

let codegen ((_, decls):string * decl list) :string = Runtime.prelude ^ concat_map codegen_decl decls ^ Runtime.postlude
