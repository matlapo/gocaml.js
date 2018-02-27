open Astwithposition

let string_of_list printer separator l = List.fold_left (fun a e -> a ^ (if a <> "" then separator else "") ^ (printer e)) "" l

let string_of_string_list = string_of_list (fun s -> s)

let string_of_bool b = match b with
  | true -> "true"
  | false -> "false"

let string_of_binary_op op = match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equals -> "=="
  | NotEquals -> "!="
  | And -> "&&"
  | Or -> "||"
  | Smaller -> "<"
  | Greater -> ">"
  | SmallerEq -> "<="
  | GreaterEq -> ">="
  | DGreater -> ">>"
  | DSmaller -> "<<"
  | AndHat -> "&^"
  | BAnd -> "&"
  | BOr -> "|"
  | Caret -> "^"

let string_of_unary_op op = match op with
  | Not -> "!"
  | UMinus -> "-"
  | UPlus -> "+"
  | UCaret -> "^"

let rec string_of_exp {value = e; _} = match e with
  | Id k -> string_of_kind k
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> s
  | RawStr s -> s
  | Rune s -> s
  | Bool b -> string_of_bool b
  | BinaryOp (b, (e1, e2)) -> "(" ^ (string_of_exp e1) ^ (string_of_binary_op b) ^ (string_of_exp e2) ^ ")"
  | Unaryexp (u, e) -> "(" ^ string_of_unary_op u ^ string_of_exp e ^ ")"
  | Append (e1, e2) -> "append(" ^ string_of_exp e1 ^ "," ^ string_of_exp e2 ^ ")"
  | FuncCall (name, exps) -> name ^ "(" ^ string_of_exps exps ^ ")"
and string_of_exps exps = string_of_list (fun e -> string_of_exp e) ", " exps

and string_of_kind_elem e = match e with
  | Variable s -> s
  | Array (s, exps) -> s ^
    List.fold_left (fun a e -> a ^ "[" ^ string_of_exp e ^ "]") "" exps

and string_of_kind k = List.fold_left (fun a e -> a ^ string_of_kind_elem e) "" k

let string_of_array_indexes indexes =
  List.fold_left (fun a i -> a ^ "[" ^ string_of_int i ^ "]") "" indexes

let rec string_of_slice_dimensions d =
  "[]" ^ string_of_slice_dimensions (d - 1)

let string_of_typeref t = match t with
  | TypeR s -> s
  | ArrayR (name, sizes) -> string_of_array_indexes sizes ^ name
  | SliceR (name, dim) -> string_of_slice_dimensions dim ^ name

let rec string_of_typedef type_def = match type_def with
  | TypeT s -> s
  | ArrayT (name, sizes) -> string_of_array_indexes sizes ^ name
  | SliceT (name, dim) -> string_of_slice_dimensions dim ^ name
  | StructT members ->
    " struct {\n"
    ^ string_of_list (fun t -> string_of_typedef_with_names t) "\n" members ^ "\n"
    ^ "}\n"
and string_of_typedef_with_names (names, type_def) =
  string_of_string_list "," names ^ string_of_typedef type_def

let rec string_of_stmts stmts = string_of_list (fun s -> string_of_stmt s) "\n" stmts
and string_of_stmt { value = s; _} = match s with
  | Block stmts -> "{\n" ^ string_of_stmts stmts ^ "\n}\n"
  | Print exps -> "print(" ^ string_of_exps exps ^ ")"
  | Println exps -> "println(" ^ string_of_exps exps ^ ")"
  | TypeDeclaration decls ->
    string_of_list
      (fun (name, types) -> "type " ^ name ^ " " ^ string_of_typedef types)
      "\n"
      decls
  | _ -> ""
  (*
  | Declaration of (string list * typesRef option * (exp node) list) list
  | If of (simpleStm node) option * exp node option * (stmt node) list * (stmt node list) option
  | Loop of loop
  | LeftArrow of (string * string)
  | Return of exp node option
  | Switch of (simpleStm node) option * exp node option * case list
  | Simple of simpleStm node
  | Break
  | Continue
  | Default
  *)

let string_of_decl {value = decl; _} = match decl with
  | Var vars -> List.fold_left
    (fun a (ids, t, exps) ->
      a ^
      "var " ^
      string_of_string_list "," ids ^
      (match t with
      | Some t -> " " ^ string_of_typeref t
      | None -> "")
      ^
      " = " ^
      string_of_string_list "," (List.map (fun e -> string_of_exp e) exps) ^
      "\n"
    )
    ""
    vars
  | Type nts -> string_of_list (fun (name, types) -> "type " ^ string_of_typedef_with_names ([name], types)) "\n" nts
  | Fct (name, args, ret, stmts) -> name ^ "("
    ^ string_of_list (fun (argname, t) -> argname ^ match t with None -> "" | Some t -> string_of_typeref t) "," args ^ "\n"
    ^ ") " ^ match ret with None -> "" | Some r -> string_of_typeref r ^ " {\n"
    ^ string_of_stmts stmts ^ "\n"
    ^ "}\n"

let string_of_prog (_, decls) = string_of_list (fun d -> string_of_decl d) "" decls

let pretty_print ast = print_string (string_of_prog ast)
