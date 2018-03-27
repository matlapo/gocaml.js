open Ast

(* ################## *)
(* #### PRINTING #### *)
(* ################## *)
let string_of_list printer sep l =
  l
  |> List.fold_left (fun acc v ->
    acc ^ (if acc <> "" then sep else "") ^ printer v)
  ""

let rec repeat_string s r =
  if (r == 0) then ""
  else s ^ repeat_string s (r-1)

let rec string_of_structdef l =
  "struct { "
  ^
  (
    l
    |> List.fold_left (fun acc (names, def) ->
          acc
          ^ string_of_list (fun s -> s) "," names
          ^ " "
          ^ string_of_typedef def ^ "; "
        )
        ""
  )
  ^
  " }"

and string_of_typedef d = match d with
  | TypeT name -> name
  | StructT members -> string_of_structdef members
  | ArrayT (name, dim) -> string_of_list (fun d -> "[" ^ Int64.to_string d ^ "]") "" dim ^ name
  | SliceT (name, dim) -> repeat_string "[]" (Int64.to_int dim) ^ name

let string_of_var_binding (name, def) =
  name ^ " [var] = " ^ string_of_typedef def

let string_of_type_binding (name, def) =
  name ^ " [type] = " ^ string_of_typedef def

let indent lvl = repeat_string "  " lvl

let string_of_function_binding (name, args, ret) =
  name ^ " [function] = " ^ "("
  ^ string_of_list (fun a -> string_of_typedef a) ", " args
  ^ ") -> "
  ^ (match ret with Void -> "void" | NonVoid r -> string_of_typedef r)

(* Prints a scope *)
let rec string_of_scope lvl scope =
  (* Prints the variable bindings of a scope *)
  (scope.bindings |> string_of_list (fun b -> indent lvl ^ string_of_var_binding b) "\n")
  ^ "\n" ^
  (* Prints the type bindings of a scope *)
  (scope.types |> string_of_list (fun b -> indent lvl ^ string_of_type_binding b) "\n")
  ^
  (* Print the children scopes *)
  (scope.children |> string_of_list (fun s ->
      indent lvl ^ "{\n"
      ^ string_of_scope (lvl+1) s
      ^ indent lvl ^ "}\n"
    )
    "\n"
  )

(* Prints the top-level scope of an application *)
let string_of_top_level_symbol_table scope =
  indent 1 ^ "{\n" ^
  (* Print the variable bindings *)
  (scope.bindings |> string_of_list (fun b -> indent 2 ^ string_of_var_binding b) "\n")
  ^ "\n" ^
  (* Print the type bindings *)
  (scope.types |> string_of_list (fun b -> indent 2 ^ string_of_type_binding b) "\n")
  ^ "\n" ^
  (* Print each function and its associated scope *)
  (List.map2 (fun a b -> (a, b)) scope.functions scope.children
    |> string_of_list (fun (f, s) ->
        indent 2 ^ string_of_function_binding f ^ "\n" ^
        indent 2 ^ "{\n"
        ^ string_of_scope 3 s
        ^ indent 2 ^ "}"
      )
      "\n"
  )
  ^ "\n" ^ indent 1 ^ "}\n"

(* given the full symbol table, prints all of its content *)
let string_of_symbol_table scope =
  "{\n" ^
  (* Print the base types *)
  (scope.types |> string_of_list (fun b -> indent 1 ^ string_of_type_binding b) "\n")
  ^ "\n" ^
  (* Print the top-level scope *)
  string_of_top_level_symbol_table (List.hd scope.children)
  ^ "}\n"
