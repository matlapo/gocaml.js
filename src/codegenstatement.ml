open Batteries
open Ast
open Codegenexpression
open Codegenutils

(* "\x5B" = "[", but opening square brackets screw up indentation in my IDE. *)
let codegen_bare_assign (s: scope) (refs: exp list) (exps: exp list) :string =
  "\x5B" ^
  (refs |> codegen_bare_exps s false) ^
  "]=\x5B" ^
  (codegen_bare_exps s true exps) ^
  "];"
let codegen_assign (s: scope) (refs: exp gen_node list) (exps: exp gen_node list) :string =
  codegen_bare_assign s (List.map unwrap_gen_node refs) (List.map unwrap_gen_node exps)

let codegen_assign_op (s: scope) (op: assign) (ref: exp gen_node) (exp: exp gen_node) =
  let r = codegen_exp s false ref in
  let e = codegen_exp s true exp in
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

let codegen_bare_simple_stmt (s: scope) (simple_stmt: simpleStm) =
  match simple_stmt with
    | Assign (Regular, (refs, exps)) -> codegen_assign s refs exps
    | Assign (op, ([ref], [exp])) -> codegen_assign_op s op ref exp
    | Assign _ -> raise (Failure "Invalid Assignment")
    | ExpStatement exp -> (codegen_exp s true exp) ^ ";"
    | DoublePlus ref -> (codegen_exp s false ref) ^ "++;"
    | DoubleMinus ref -> (codegen_exp s false ref) ^ "--;"
    | ShortDeclaration (refs, exps) ->
      (refs
        |> List.map unwrap_gen_node
        |> List.filter_map (fun ref -> match ref with
            | Id s -> Some s
            | _ -> None
          )
        (* TODO: Filter to exclude predeclared identifiers *)
        |> List.map (fun name -> "let " ^ (mangle s name) ^ ";")
        |> concat
      ) ^
      (codegen_assign s refs exps)
    | Empty -> "/* Empty Simple Statement */"

let codegen_simple_stmt (stmt_scope: scope) (simple_stmt: simpleStm gen_node) =
  let scope = match simple_stmt with
    | Scoped { scope=simple_stmt_scope; value=stmt } -> simple_stmt_scope
    | _ -> stmt_scope in
  codegen_bare_simple_stmt scope (unwrap_gen_node simple_stmt)

let codegen_decl (s: scope) (decl: (string list * gotype option * (exp gen_node) list)): string =
  let (names, t, exps) = decl in
  let rhs = match (t, exps) with
    | (Some gotype, []) ->
      let length = List.length names in
      List.make length (zero_value_of_type s gotype)
    | (_, exps) -> List.map (codegen_exp s true) exps in
  "let [" ^
  (names |> List.map (mangle s) |> concat_comma) ^
  "]=[" ^
  (rhs |> concat_comma) ^
  "];"
let codegen_decls (s: scope) (decls: (string list * gotype option * (exp gen_node) list) list) :string =
  decls
    |> List.map (codegen_decl s)
    |> concat

let with_init (s: scope) (init: simpleStm gen_node) (innerCode: string): string = match unwrap_gen_node init with
  | Empty -> innerCode
  | _ ->
    "{" ^
      codegen_simple_stmt s init ^
      innerCode ^
    "}"

let is_empty (expr: stmt gen_node) = match unwrap_gen_node expr with
  | Simple simple_stmt -> (match unwrap_gen_node simple_stmt with
    | Empty -> true
    | _ -> false )
  | _ -> false

let rec fold_cases (cases: case list) = match cases with
  | [] -> []
  | case::[] -> [case]
  | (values_opt1, no_stmts)::(values_opt2, stmts)::rest when List.for_all is_empty no_stmts ->
    let values_opt = (match (values_opt1, values_opt2) with
      | (None, None) -> None
      | (Some values, None)|(None, Some values) -> Some values
      | (Some values1, Some values2) -> Some (values1@values2)
    ) in
    (values_opt, stmts)::rest
  | case::rest -> case::(fold_cases rest)

let rec codegen_stmt (stmt_gen_node:stmt gen_node) :string =
  let (scope, stmt) = match stmt_gen_node with
    | Scoped { scope=scope; value=stmt } -> (scope, stmt)
    | _ -> raise (Failure "unreachable") in
  match stmt with
    | Block stmts ->
      "{" ^
        (scope_id_comment scope) ^
        codegen_stmts stmts ^
      "}"
    | Print exps -> "print(" ^ codegen_exps scope true exps ^ ");"
    | Println exps -> "println(" ^ codegen_exps scope true exps ^ ");"
    | Declaration decls -> codegen_decls scope decls
    | TypeDeclaration _ -> ""
    | If (init, condition, stmts, _else) ->
      let generated_if =
        "if(" ^
          codegen_exp scope true condition ^
        "){" ^
          (scope_id_comment scope) ^
          codegen_stmts stmts ^
        "}" in
      let generated_else = match _else with
        | Some stmts ->
          "else{" ^
            (scope_id_comment scope) ^
            codegen_stmts stmts ^
          "}"
        | None -> "" in
      with_init scope init (generated_if ^ generated_else)
    | Loop While (cond, stmts) ->
      "while(" ^
      (Option.map_default (codegen_exp scope true) "true" cond) ^
      "){" ^
      codegen_stmts stmts ^
      "}"
    | Loop For (init, cond, increment, stmts) ->
      "for(" ^
      (codegen_simple_stmt scope init) ^
      ";" ^
      (cond |> Option.map_default (codegen_exp scope true) "") ^
      ";" ^
      (codegen_simple_stmt scope increment) ^
      "){" ^
      (codegen_stmts stmts) ^
      "}"
    | Return Some expr -> "return" ^ (codegen_exp scope true expr) ^ ";"
    | Return None -> "return;"
    | Switch (init, target, cases) ->
      let target_code = Option.map_default (codegen_exp scope true) "true" target in
      let ifs_code = cases
        |> fold_cases
        |> List.map (
          fun (exprs_opt, stmts) ->
            let condition_code = match exprs_opt with
              | Some exprs -> exprs
                |> List.map (codegen_exp scope true)
                |> List.map (fun expr_code -> expr_code ^ "=== target")
                |> String.join "||"
              | None -> "true" in
            "if(" ^
            condition_code ^
            "){" ^
            (codegen_stmts stmts) ^
            "}"
        )
        |> String.join "else " in
          with_init scope init (
          "{" ^
            "let target =" ^ target_code ^ ";" ^
            ifs_code ^
          "}"
      )
    | Simple simple_stmt -> codegen_simple_stmt scope simple_stmt
    | Break -> "break;"
    | Continue -> "continue;"
and codegen_stmts (stmts: stmt gen_node list) :string = stmts |> List.map codegen_stmt |> concat
