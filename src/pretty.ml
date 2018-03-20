open Ast

let string_of_list printer separator l = List.fold_left (fun a e -> a ^ (if a <> "" then separator else "") ^ (printer e)) "" l

let string_of_string_list = string_of_list (fun s -> s)

let rec indent lvl = if lvl < 1 then "" else ("  " ^ indent (lvl - 1))

let none_or_print printer e = match e with
  | None -> ""
  | Some e -> printer e

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

let extract_gen_node_value gn = match gn with
| Position {value = e; _} -> e
| Typed {value = e; _} -> e
| Scoped {value = e; _} -> e

let rec string_of_exp gn =
  let e = extract_gen_node_value gn in
  match e with
  | Id k -> string_of_kind k
  | Int i -> Int64.to_string i
  | Float f -> string_of_float f
  | String s -> s
  | RawStr s -> s
  | Rune s -> s
  | Bool b -> string_of_bool b
  | BinaryOp (b, (e1, e2)) -> "(" ^ (string_of_exp e1) ^ " " ^ (string_of_binary_op b) ^ " " ^ (string_of_exp e2) ^ ")"
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
  List.fold_left (fun a i -> a ^ "[" ^ Int64.to_string i ^ "]") "" indexes

let rec string_of_slice_dimensions d =
  if Int64.compare d Int64.one < 0 then "" else
  "[]" ^ string_of_slice_dimensions (Int64.sub d Int64.one)

let string_of_typeref lvl t = match t with
  | TypeR s -> indent lvl ^ s
  | ArrayR (name, sizes) -> indent lvl ^ string_of_array_indexes sizes ^ name
  | SliceR (name, dim) -> indent lvl ^ string_of_slice_dimensions dim ^ name

let rec string_of_typedef lvl type_def = match type_def with
  | TypeT s -> indent lvl ^ s
  | ArrayT (name, sizes) -> indent lvl ^ string_of_array_indexes sizes ^ name
  | SliceT (name, dim) -> indent lvl ^ string_of_slice_dimensions dim ^ name
  | StructT members ->
    " struct {\n"
    ^ string_of_list (fun t -> string_of_typedef_with_names (lvl + 1) t) "\n" members ^ "\n"
    ^ indent lvl ^ "}\n"
and string_of_typedef_with_names lvl (names, type_def) =
  string_of_string_list "," names ^ string_of_typedef lvl type_def

let rec string_of_stmts lvl stmts = List.fold_left (fun a s -> a ^ string_of_stmt lvl s ^ "") "" stmts
and string_of_stmt lvl gn =
  let s = extract_gen_node_value gn in
  match s with
  | Block stmts -> indent lvl ^ "{\n"
    ^ string_of_stmts (lvl + 1) stmts
    ^ indent lvl ^ "}\n"
  | Print exps -> indent lvl ^ "print(" ^ string_of_exps exps ^ ")\n"
  | Println exps -> indent lvl ^ "println(" ^ string_of_exps exps ^ ")\n"
  | TypeDeclaration decls ->
    string_of_list
      (fun (name, types) -> indent lvl ^ "type " ^ name ^ " " ^ string_of_typedef 0 types)
      "\n"
      decls
  | Declaration decls -> string_of_list
      (fun (names, typesref, exps) ->
        indent lvl ^ "var " ^ string_of_string_list "," names ^ " "
        ^ none_or_print (string_of_typeref 0) typesref ^ " "
        ^ string_of_exps exps
      )
      "\n"
      decls
      ^ "\n"
  | If (sstm, exp, stmts1, else_stmts) -> indent lvl ^ "if " ^ string_of_simple_stmt sstm ^ "; "
    ^ string_of_exp exp ^ " {\n"
    ^ string_of_stmts (lvl + 1) stmts1 ^ indent lvl ^ "}"
    ^ none_or_print (fun stmts -> " else {\n" ^ string_of_stmts (lvl + 1) stmts ^ indent lvl ^ "}") else_stmts
    ^ "\n"
  | Loop (While (exp, stmts)) -> indent lvl ^ "for " ^ none_or_print string_of_exp exp ^ " {\n"
    ^ string_of_stmts (lvl + 1) stmts
    ^ indent lvl ^ "}\n"
  | Loop (For (sstmt1, exp, sstmt2, stmts)) -> indent lvl ^ "for "
    ^ string_of_simple_stmt sstmt1 ^ "; "
    ^ none_or_print string_of_exp exp ^ "; "
    ^ string_of_simple_stmt sstmt2 ^ " {\n"
    ^ string_of_stmts (lvl + 1) stmts
    ^ indent lvl ^ "}\n"
  | Return exp -> indent lvl ^ "return " ^ none_or_print string_of_exp exp ^ "\n"
  | Switch (sstmt, exp, cases) -> indent lvl ^ "switch "
    ^ string_of_simple_stmt sstmt ^ "; "
    ^ none_or_print string_of_exp exp ^ " {\n"
    ^ string_of_list (string_of_case lvl) "" cases ^ indent lvl ^ "}\n"
  | Simple sstmt -> if (extract_gen_node_value sstmt) = Empty then string_of_simple_stmt sstmt else indent lvl ^ string_of_simple_stmt sstmt ^ "\n"
  | Break -> indent lvl ^ "break\n"
  | Continue -> indent lvl ^ "continue\n"
  | Default -> indent lvl ^ "default\n"
and string_of_simple_stmt gn =
  let s = extract_gen_node_value gn in
  match s with
  | ExpStatement exp -> string_of_exp exp
  | DoublePlus k -> string_of_kind k ^ "++"
  | DoubleMinus k -> string_of_kind k ^ "--"
  | ShortDeclaration (kl, exps) -> string_of_list string_of_kind "," kl ^ " := " ^ string_of_exps exps
  | Empty -> ""
  | Assign (a, (kl, exps)) -> string_of_list string_of_kind "," kl ^ " "
    ^ string_of_assign a ^ " " ^ string_of_exps exps
and string_of_assign a = match a with
  | Regular -> "="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | DivEqual -> "/="
  | TimesEqual -> "*="
  | AndEqual -> "&="
  | OrEqual -> "|="
  | HatEqual -> "^="
  | PercentEqual -> "%="
  | AndHatEqual -> "&^="
  | DoubleGreaterEqual -> ">>="
  | DoubleSmallerEqual -> "<<="

and string_of_case lvl (exps, stmts) = indent lvl ^ "case "
    ^ none_or_print (string_of_list string_of_exp ",") exps ^ ":\n"
    ^ string_of_stmts (lvl + 1) stmts

let string_of_decl gn =
  let decl = extract_gen_node_value gn in
  match decl with
  | Var vars -> List.fold_left
    (fun a (ids, t, exps) ->
      a ^
      "var " ^
      string_of_string_list "," ids ^
      (match t with
      | Some t -> " " ^ string_of_typeref 0 t
      | None -> "")
      ^
      (if List.length exps > 0 then
        " = " ^
        string_of_string_list "," (List.map (fun e -> string_of_exp e) exps)
      else
        "")
      ^ "\n"
    )
    ""
    vars
    ^ "\n"
  | Type nts -> string_of_list (fun (name, types) -> "type " ^ string_of_typedef_with_names 0 ([name], types)) "\n" nts
  | Fct (name, args, ret, stmts) -> "func " ^ name ^ "("
    ^ string_of_list (fun (argname, t) -> argname ^ " " ^ string_of_typeref 0 t) ", " args
    ^ ") " ^ none_or_print (fun r -> string_of_typeref 0 r ^ " ") ret ^ "{\n"
    ^ string_of_stmts 1 stmts
    ^ "}\n\n"

let string_of_prog (package, decls) = "package " ^ package ^ "\n\n" ^ string_of_list (fun d -> string_of_decl d) "" decls

let pretty_print ast = print_string (string_of_prog ast)
