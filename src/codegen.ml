open Batteries
open Ast
open Codegenstatement
open Codegenutils


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
