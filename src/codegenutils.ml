open Batteries
open Ast

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
let paren (code: string) :string = "(" ^ code ^ ")"

let unwrap_gen_node (node:'a gen_node) :'a = match node with
  | Position { value=v } -> v
  | Typed { value=v } -> v
  | Scoped { value=v } -> v

let prevscope_of_snode (node: 'a gen_node): scope = match node with
  | Scoped { prevscope=scope } -> scope
  | _ -> raise (Failure "Can't extract the prevscope of a simple statement. It is not an snode.")

let prevscope_of_simple_stmt: simpleStm gen_node -> scope = prevscope_of_snode

let scope_of_snode (node: 'a gen_node): scope = match node with
  | Scoped { scope=scope } -> scope
  | _ -> raise (Failure "Can't extract the scope of a simple statement. It is not an snode.")

let scope_of_simple_stmt: simpleStm gen_node -> scope = scope_of_snode

let type_of_expr (scope: scope) (node: exp gen_node): gotype = match node with
  | Typed { typ=t } -> (match t.gotype with
    | Defined typename -> (
        let scopedType = Option.get (Typecheck.scopedtype_of_typename_opt scope typename) in
        let reducedScopedType = Option.get (Typecheck.resolve_to_reducedtype_opt scope scopedType) in
        let reducedType = reducedScopedType.gotype in
        match reducedType with
          | Defined _ -> raise (Failure "Error: tried to resolve a type and got another Defined type")
          | t -> t
      )
    | resolved_type -> resolved_type)
  | Scoped _ -> raise (Failure "Error: expected tnode, got snode")
  | Position _ -> raise (Failure "Error: expected tnode, got position node")

let mangle_fct (name: string): string = "_" ^ name

let mangle_decl (scope: scope) (name: string) :string =
  if name = "_" then
    "_"
  else
    "_" ^ (string_of_int scope.scopeid) ^ "_" ^ name

let mangle_expr (scope: scope) (name: string) :string =
  if name = "_" then
    "_"
  else
    let scope_id = name
      |> Typecheck.find_scope_of_varname_opt scope
      |> Option.map (fun var_scope -> string_of_int var_scope.scopeid)
      |> Option.default "unknown"
    in
    "_" ^ scope_id ^ "_" ^ name

let mangle (scope: scope) (use_in_expr: bool) (name: string) :string =
  if use_in_expr then
    mangle_expr scope name
  else
    mangle_decl scope name

let zero_value_of_basetype (t: basetype): string = match t with
  | BInt -> "0"
  | BFloat64 -> "0.0"
  | BString -> "\"\""
  | BRune -> "0"
  | BBool -> "_0_false"

let rec zero_value_of_type (s: scope) (t: gotype): string =
  match t with
    | Basetype bt -> zero_value_of_basetype bt
    | Defined typename -> (
      let scopedType = Option.get (Typecheck.scopedtype_of_typename_opt s typename) in
      let reducedScopedType = Option.get (Typecheck.resolve_to_reducedtype_opt s scopedType) in
      let reducedType = reducedScopedType.gotype in
      match reducedType with
        | Defined _ -> raise (Failure "unreachable")
        | t -> zero_value_of_type s t
      )
    | Array (t, length) ->
      let t_zero = zero_value_of_type s t in
      "[" ^
      (List.make (Int64.to_int length) t_zero |> concat_comma) ^
      "]"
    | Slice _ -> "[]"
    | Struct fields ->
      "{" ^
        (fields
          |> List.map (fun (name, t) -> name ^ ":" ^ (zero_value_of_type s t) ^ ",")
          |> concat
        ) ^
      "}"
    | Null -> raise (Failure "Can't compute the zero value of type Null")
