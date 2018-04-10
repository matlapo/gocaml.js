open Batteries
open Ast
open Codegenexpression
open Codegenutils

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