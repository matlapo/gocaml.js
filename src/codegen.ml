open Batteries
open Ast
open Codegenstatement
open Codegenutils


let codegen_decl (scope: scope) (decl:decl) :string = match decl with
  | Var decls -> codegen_decls scope decls
  | Type _ -> ""
  | Fct (name, args, _, stmts) ->
    "function " ^
    (mangle scope name) ^
    "(" ^
    (args |> List.map fst |> List.map (mangle scope) |> concat_comma) ^
    "){" ^
      (scope_id_comment scope) ^
      (stmts |> concat_map codegen_stmt) ^
    "}"

let codegen (scope: scope) ((_, decls):string * decl list) :string =
  Runtime.prelude ^
  (concat_map (codegen_decl scope) decls) ^
  (mangle scope "main") ^ "();" ^
  Runtime.postlude
