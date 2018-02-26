open Astwithposition

let string_of_list printer comma l = List.fold_left (fun a e -> a ^ (if a <> "" && comma then "," else "") ^ (printer e)) "" l

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
  | _ -> "[TODO]"

let string_of_unary_op op = match op with
  | Not -> "!"
  | UMinus -> "-"
  | UPlus -> "+"
  | UCaret -> "^"

let rec string_of_exp {value = e; _} = match e with
  | Id s -> s
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> s
  | RawStr s -> s
  | Rune s -> s
  | Bool b -> string_of_bool b
  | Octal s -> s
  | Hex s -> s
  | BinaryOp (b, (e1, e2)) -> "(" ^ (string_of_exp e1) ^ (string_of_binary_op b) ^ (string_of_exp e2) ^ ")"
  | Unaryexp (u, e) -> "(" ^ string_of_unary_op u ^ string_of_exp e ^ ")"
  | FuncCall exps -> "" (* TODO *)

let string_of_type _ = "[TYPE]" (* TODO *)

let string_of_decl {value = decl; _} = match decl with
  | Var vars -> List.fold_left
    (fun a (ids, t, exps) ->
      a ^
      "var " ^
      string_of_list (fun s -> s) true ids ^
      (match t with
      | Some t -> " " ^ string_of_type t
      | None -> "")
      ^
      " = " ^
      string_of_list (fun s -> s) true (List.map (fun e -> string_of_exp e) exps) ^
      "\n"
    )
    ""
    vars
  | _ -> "[OTHER CODE]" (* TODO *)

let string_of_prog (_, decls) = string_of_list (fun d -> string_of_decl d) false decls

let pretty_print ast = print_string (string_of_prog ast)
