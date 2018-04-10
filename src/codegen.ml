open Ast
open Batteries

let map_filter (f: 'a -> 'b option) (l: 'a list) :'b list =
  l
    |> List.map f
    |> List.filter Option.is_some
    |> List.map Option.get


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
    | AndHat -> l ^ "& ~" ^ r
    | BAnd -> l ^ "&" ^ r
    | BOr -> l ^ "|" ^ r
    | Caret -> l ^ "^" ^ r
  in
  paren code
and codegen_bare_exp (exp: exp) :string =
  let code = match exp with
    | Id ref -> mangle ref
    | Indexing (_,_) -> "/* UNIMPLEMENTED*/"
    | Selection (_,_) -> "/* UNIMPLEMENTED*/"
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
    | FuncCall (name, params) -> (mangle name) ^ "(" ^ (codegen_exps params) ^ ")"
    | Append (l, v) -> "('' /* UNIMPLEMENTED_APPEND */)"
  in
  paren code
and codegen_exp (exp: exp gen_node) :string = codegen_bare_exp (unwrap_gen_node exp)
and codegen_bare_exps (exps: exp list): string =
  exps
    |> List.map codegen_bare_exp
    |> concat_comma
and codegen_exps (exps: exp gen_node list): string = codegen_bare_exps (List.map unwrap_gen_node exps)


let zero_value_of_basetype (t: basetype): string = match t with
  | BInt -> "0"
  | BFloat64 -> "0.0"
  | BString -> "\"\""
  | BRune -> "0"
  | BBool -> "0.0"

let zero_value_of_type (t: gotype): string = match t with
  | Basetype bt -> zero_value_of_basetype bt
  | Defined _ -> raise (Failure "Can't compute the zero value of a defined type")
  | Array _ -> raise (Failure "unimplemented")
  | Slice _ -> raise (Failure "unimplemented")
  | Struct _ -> raise (Failure "unimplemented")
  | Null -> raise (Failure "Can't compute the zero value of type Null")

let codegen_var_decl (names: string list) :string =
  names
    |> List.map (fun name -> "let " ^ (mangle name) ^ ";")
    |> concat

(* "\x5B" = "[", but opening square brackets screw up indentation in my IDE. *)
let codegen_bare_assign (refs: exp list) (exps: exp list) :string =
  "\x5B" ^
  (refs |> codegen_bare_exps) ^
  "]=\x5B" ^
  (codegen_bare_exps exps) ^
  "];"
let codegen_assign (refs: exp gen_node list) (exps: exp gen_node list) :string =
  codegen_bare_assign (List.map unwrap_gen_node refs) (List.map unwrap_gen_node exps)

let codegen_assign_op (op: assign) (ref: exp gen_node) (exp: exp gen_node) =
  let r = codegen_exp ref in
  let e = codegen_exp exp in
  match op with
    | Regular -> raise (Failure "unreachable")
    | PlusEqual -> r ^ "+=" ^ e ^ ";"
    | MinusEqual -> r ^ "-=" ^ e ^ ";"
    | TimesEqual -> r ^ "*=" ^ e ^ ";"
    | DivEqual -> r ^ "/=" ^ e ^ ";"
    | AndEqual -> r ^ "&=" ^ e ^ ";"
    | OrEqual -> r ^ "|=" ^ e ^ ";"
    | HatEqual -> r ^ "^=" ^ e ^ ";"
    | PercentEqual -> r ^ "%=" ^ e ^ ";"
    | AndHatEqual -> r ^ "=" ^ r ^ "& ~" ^ e ^ ";"
    | DoubleGreaterEqual -> r ^ ">>=" ^ e ^ ";"
    | DoubleSmallerEqual -> r ^ "<<=" ^ e ^ ";"

let codegen_bare_simple_stmt (simple_stmt: simpleStm) =
  match simple_stmt with
  | Assign (Regular, (refs, exps)) -> codegen_assign refs exps
  | Assign (op, ([ref], [exp])) -> (codegen_exp ref) ^ ""
  | Assign _ -> raise (Failure "Invalid Assignment")
  | ExpStatement exp -> (codegen_exp exp) ^ ";"
  | DoublePlus ref -> (codegen_exp ref) ^ "++;"
  | DoubleMinus ref -> (codegen_exp ref) ^ "--;"
  | ShortDeclaration (refs, exps) ->
    (refs
      (* Only include simple identifiers. Not compound refs like a.b[2] *)
      |> List.map codegen_exp
      |> codegen_var_decl
    ) ^
    (codegen_assign refs exps)
  | Empty -> "/* Empty Simple Statement */"
let codegen_simple_stmt (simple_stmt: simpleStm gen_node) = codegen_bare_simple_stmt (unwrap_gen_node simple_stmt)

let codegen_decl (decl: (string list * gotype option * (exp gen_node) list)): string =
  let (names, t, exps) = decl in
  let rhs = match (t, exps) with
    | (Some gotype, []) ->
      let length = List.length names in
      List.map zero_value_of_type (List.make length gotype)
    | (_, exps) -> List.map codegen_exp exps in
  "let [" ^
  (names |> List.map mangle |> concat_comma) ^
  "]=[" ^
  (rhs |> concat_comma) ^
  "];"
let codegen_decl_assign (decls: (string list * gotype option * (exp gen_node) list) list) :string =
  decls
    |> List.map codegen_decl
    |> concat

let rec codegen_stmt (stmt:stmt gen_node) :string =
  let s = unwrap_gen_node stmt in
  match s with
    | Block stmts -> "{" ^ codegen_stmts stmts ^ "}"
    | Print exps -> "print(" ^ codegen_exps exps ^ ");"
    | Println exps -> "println(" ^ codegen_exps exps ^ ");"
    | Declaration decls -> codegen_decl_assign decls
    | TypeDeclaration _ -> ""
    | If (initOption, condition, stmts, _else) ->
      let generated_if =
        "if(" ^
          codegen_exp condition ^
        "){" ^
          codegen_stmts stmts ^
        "}" in
      let generated_else = match _else with
        | Some stmts ->
          "else{" ^
          codegen_stmts stmts ^
          "}"
        | None -> "" in
      (match initOption with
        (* | Empty -> generated_if ^ generated_else *) (* TODO: match empty without unwrapping *)
        | init ->
          "{" ^
            codegen_simple_stmt init ^
            generated_if ^
            generated_else ^
          "}"
      )
    | Loop While (cond, stmts) ->
      "while(" ^
      (Option.map_default codegen_exp "true" cond) ^
      "){" ^
      codegen_stmts stmts ^
      "}"
    | Loop For _ -> "/* UNIMPLEMENTED FOR */"
    | Return _ -> "return;"
    | Switch _ -> "/* UNIMPLEMENTED SWITCH */"
    | Simple simple_stmt -> codegen_simple_stmt simple_stmt
    | Break -> "break;"
    | Continue -> "continue;"
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
