open Batteries
open Ast
open Codegenstatement
open Codegenutils

let codegen_decl (scope: scope) (decl:decl) :string = match decl with
  | Var decls -> codegen_decls scope decls
  | Type _ -> ""
  | Fct (name, args, _, stmts) ->
    let inner_scope = List.at scope.children 0 in
    "function " ^ (mangle_decl scope name) ^
    "(" ^
    (args |> List.map fst |> List.map (mangle_decl inner_scope) |> concat_comma) ^
    "){" ^
      (stmts |> concat_map codegen_stmt) ^
    "}"

let codegen (scope: scope) ((_, decls):string * decl list) :string =
  let first_child_scope = List.at scope.children 0 in
  Runtime.prelude ^
  (concat_map (codegen_decl first_child_scope) decls) ^
  (match (List.exists (fun (name, _) -> name = "main") first_child_scope.functions) with
    | true -> (mangle_expr first_child_scope "main") ^ "();"
    | false -> ""
  ) ^
  Runtime.postlude
