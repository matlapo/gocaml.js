open Ast

let concat_map (f: 'a -> string) (l: 'a list) :string = l |> List.map f |> List.fold_left (^) ""

let unwrap_gen_node (node:'a gen_node) :'a = match node with
  | Position { value=v } -> v
  | Typed { value=v } -> v
  | Scoped { value=v } -> v

let codegen_exp (exp:exp) :string = match exp with
  | String s -> s
  | _ -> "(undefined /* UNIMPLEMENTED_EXP */)"

let codegen_stmt (stmt:stmt) :string = match stmt with
  | Print exps ->
    "print(" ^
    (exps |> List.map unwrap_gen_node |> concat_map codegen_exp)
    ^ ");"
  | Return _ -> "return;"
  | s -> "/* UNIMPLEMENTED_STMT */"

let codegen_decl (decl:decl) :string = match decl with
  | Type _ -> ""
  | Fct (name, [], Void, stmts) ->
    "function " ^
    name ^
    "(){" ^
    (stmts |> List.map unwrap_gen_node |> concat_map codegen_stmt)
    ^ "}"
  | _ -> "/* UNIMPLEMENTED_DECL */"

let codegen ((_, decls):string * decl list) :string = Runtime.prelude ^ concat_map codegen_decl decls ^ Runtime.postlude
