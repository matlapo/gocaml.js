open Astwithposition
open BatOption

let concat = List.fold_left (^) ""
let concat_comma =
  List.fold_left
    (fun acc elt -> match acc with
      | "" -> elt
      | _ -> acc ^ "," ^ elt
    )
    ""

let concat_map (f: 'a -> string) (l: 'a list) :string = l |> List.map f |> List.fold_left (^) ""

let unwrap_gen_node (node:'a gen_node) :'a = match node with
  | Position { value=v } -> v
  | Typed { value=v } -> v
  | Scoped { value=v } -> v

let paren (code: string) :string = "(" ^ code ^ ")"
let mangle (name: string) :string = "_" ^ name

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
    | Id [Variable id] -> mangle id
    | Id _ -> "(undefined /* UNIMPLEMENTED COMPLEX IDENTIFIERS */)"
    | Int i -> Int64.to_string i
    | Float f -> string_of_float f
    | String s -> s
    | RawStr s -> "('' /* UNIMPLEMENTED_RAW_STRING */)"
    | Rune r -> r
    | Bool b -> string_of_bool b
    | BinaryOp (op, (left, right)) -> codegen_binary_op op left right
    | Unaryexp (op, exp) -> codegen_unary_op op exp
    | FuncCall (name, params) -> (mangle name) ^ "(" ^ (codegen_exps params) ^ ")"
    | Append (l, v) -> "('' /* UNIMPLEMENTED_APPEND */)"
  in
  paren code
and codegen_exps (exps: exp gen_node list): string =
  exps
    |> List.map codegen_exp
    |> concat_comma

let codegen_var_decl (names: string list) :string =
  names
    |> List.map (fun name -> "let " ^ (mangle name) ^ ";")
    |> concat

let codegen_assign (names: string list) (exps: exp gen_node list) :string =
  "[" ^
  (names |> List.map mangle |> concat_comma) ^
  "]=[" ^
  (codegen_exps exps) ^
  "];"

let codegen_decl_assign (decls: (string list * typesRef option * (exp gen_node) list) list) :string =
  decls
    |> List.map (fun decl -> match decl with
        | (names, _, []) -> codegen_var_decl names
        | (names, _, exps) -> (codegen_var_decl names) ^ (codegen_assign names exps)
      )
    |> concat

let rec codegen_stmt (stmt:stmt gen_node) :string =
  let s = unwrap_gen_node stmt in
  match s with
    | Block stmts -> "{" ^ codegen_stmts stmts ^ "}"
    | Print exps -> "print(" ^ codegen_exps exps ^ ");"
    | Println exps -> "println(" ^ codegen_exps exps ^ ");"
    | Declaration decls -> codegen_decl_assign decls
    | TypeDeclaration _ -> ""
    | If (None, condition, stmts, _else) ->
      "if(" ^ codegen_exp condition ^
      "){" ^
      codegen_stmts stmts ^
      "}" ^
      map_default codegen_stmts "" _else
    | If _ -> "/* UNIMPLEMENTED INIT IF*/"
    | Loop While (cond, stmts) ->
      "while(" ^
      (map_default codegen_exp "true" cond) ^
      "){" ^
      codegen_stmts stmts ^
      "}"
    | Loop For _ -> "/* UNIMPLEMENTED FOR */"
    | Return _ -> "return;"
    | Switch _ -> "/* UNIMPLEMENTED SWITCH */"
    | Simple simpleStm -> "/* UMIMPLEMENTED SIMPLE STMT */"
    | Break -> "break;"
    | Continue -> "continue;"
    | Default -> "/* UNIMPLEMENTED DEFAULT */"
and codegen_stmts (stmts: stmt gen_node list) :string = stmts |> List.map codegen_stmt |> concat

let codegen_args (args:argument list) :string = args |> List.map fst |> concat_comma

let codegen_decl (decl:decl) :string = match decl with
  | Var decls -> codegen_decl_assign decls
  | Type _ -> ""
  | Fct (name, args, _, stmts) ->
    "function " ^
    (mangle name) ^
    "(" ^
    (args |> List.map fst |> concat_comma) ^
    "){" ^
    (stmts |> concat_map codegen_stmt) ^
    "}"

let codegen ((_, decls):string * decl list) :string = Runtime.prelude ^ concat_map codegen_decl decls ^ Runtime.postlude
