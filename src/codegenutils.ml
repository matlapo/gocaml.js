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

let unwrap_gen_node (node:'a gen_node) :'a = match node with
  | Position { value=v } -> v
  | Typed { value=v } -> v
  | Scoped { value=v } -> v

let paren (code: string) :string = "(" ^ code ^ ")"
let mangle (name: string) :string = "_" ^ name

let zero_value_of_basetype (t: basetype): string = match t with
  | BInt -> "0"
  | BFloat64 -> "0.0"
  | BString -> "\"\""
  | BRune -> "0"
  | BBool -> "0.0"

let zero_value_of_type (t: gotype): string = match t with
  | Basetype bt -> zero_value_of_basetype bt
  | Defined _ -> raise (Failure "Can't compute the zero value of a defined type")
  | Array _ -> raise (Failure "unimplemented")
  | Slice _ -> raise (Failure "unimplemented")
  | Struct _ -> raise (Failure "unimplemented")
  | Null -> raise (Failure "Can't compute the zero value of type Null")
