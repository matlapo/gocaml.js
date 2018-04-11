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
      (stmts |> concat_map codegen_stmt) ^
    "}"

let codegen (scope: scope) ((_, decls):string * decl list) :string =
  let main = mangle scope "main" in
  let fcts_scope = List.at scope.children 0 in
  Runtime.prelude ^
  (concat_map (codegen_decl scope) decls) ^
  (match (List.exists (fun (name, _) -> name = "main") fcts_scope.functions) with
    | true -> main ^ "();"
    | false -> ""
  ) ^
  Runtime.postlude
