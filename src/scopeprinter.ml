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
          acc ^ names ^ " " ^ string_of_gotype def ^ "; "
        )
        ""
  )
  ^
  " }"

and string_of_basetype t = match t with
  | BInt -> "(base)int"
  | BFloat64 -> "(base)float64"
  | BString -> "(base)string"
  | BRune -> "(base)rune"
  | BBool -> "(base)bool"

and string_of_gotype d = match d with
  | Basetype t -> string_of_basetype t
  | Defined s -> "(defined)" ^ s
  | Struct members -> string_of_structdef members
  | Array (t, size) -> "[" ^ Int64.to_string size ^ "]" ^ string_of_gotype t
  | Slice t -> "[]" ^ string_of_gotype t

let string_of_var_binding (name, def) =
  name ^ " [var] = " ^ string_of_gotype def.gotype

let string_of_type_binding (name, def) =
  name ^ " [type] = " ^ string_of_gotype def.gotype

let indent lvl = repeat_string "  " lvl

let string_of_function_binding (name, args, ret) =
  name ^ " [function] = " ^ "("
  ^ string_of_list (fun a -> string_of_gotype a.gotype) ", " args
  ^ ") -> "
  ^ (match ret with Void -> "void" | NonVoid r -> string_of_gotype r.gotype)

(* Prints a scope *)
let rec string_of_scope lvl (scope: Ast.scope) =
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
