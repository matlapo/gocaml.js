open Astwithposition
open Utils
open BatOption

(*
  Interesting functions
  List.assoc_opt -> find
  List.mem_assoc

  Each func eval to list of bindings

  search in symbol -> find in reversed list

  you can have duplicates across sublists, but not
  inside the same sublist

  a sublist is of the form (name, type option)::rest and
  represents a scope

  assumption for now: function = single scope (no inner scope)
*)

(* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

(*

#### TO SOLVE ####
1. base types (or types in general) are weak representation
2. need to resolve for underlying type
3. var x alone?
4. context should also include info about type declarations
3. don't return string list, fail on error

*)

(* a = (name, type) list list b = decl *)
module Option = BatOption
let bind x f = Option.bind f x
let id x = x

let map_flat f l =
  l
  |> List.map f
  |> List.flatten

let id_undeclared id = Printf.sprintf "Variable %s is used before being declared" id
let binary_different_types = Printf.sprintf "Expecting both expressions to be of type bool but got type yolo"

let base_int = "int"
let base_float = "float"
let base_string = "string"
let base_bool = "bool"
let base_rune = "rune"

(* definition of a symbol table *)
(* type scope =
  {
    bindings: (string * typesRef) list;
    types: typesDef list;
    scopes: scope list
  }

let base_types =
  [
    TypeT base_int;
    TypeT base_float;
    TypeT base_string;
    TypeT base_bool;
    TypeT base_rune
  ]

let top_level =
  {
    bindings = [];
    types = base_types;
    scopes = []
  }

(* this lookup doesn't work, needs to be backward *)
let lookup table name: typesRef = TypeR "not implemented" *)

(* let enode (x: Astwithposition.exp Astwithposition.node) t value: Astwithtype.exp Astwithtype.enode =
  {
    position = x.position;
    types = t;
    value = value
  } *)

let some x = Some x

let to_tnode (e: exp node) t = { position = e.position; typ = t; value = e.value }

(* converts a exp node to exp enode (node with type) *)
let rec typecheck_exp (e: exp gen_node) sym: (exp tnode) option =
  match e with
  | Position e ->
    (match e.value with
    | Int i -> to_tnode e (TypeR base_int) |> some
    | Float f -> to_tnode e (TypeR base_float) |> some
    | RawStr s
    | String s -> to_tnode e (TypeR base_string) |> some
    | Bool b -> to_tnode e (TypeR base_bool) |> some
    | Rune r -> to_tnode e (TypeR base_rune) |> some
    | BinaryOp (bin, (a, b)) ->
      typecheck_exp a sym
      |> bind (fun a ->
        typecheck_exp b sym
        |> bind (fun b ->
          match bin with
          | Plus ->
            (match a.typ, b.typ with
            | TypeR x, TypeR y ->
              if x = base_int && y = base_int then None
              else None
            | _ -> None)
          | _ -> None
        )
      )
    | _ -> None)
  | Typed e -> Some e

  (*| BinaryOp (bin, (a, b)) ->
    let test =
      typecheck_exp a sym
      |> bind (fun a ->
        typecheck_exp b sym
        |> bind (fun b ->
          match bin with
          | Plus ->
            (match a.types, b.types with
            | TypeR x, TypeR y ->
              if x = base_int && y = base_int then
                Some (enode e (TypeR base_int) (BinaryOp (Plus, (a,b))))
              else
                None
            | _ -> None)
          | _ -> Some b
        )
      ) in
    test
  | _ -> None) *)

(* let table_func (name, args, ret, body): table =
  let rec helper s =
    match s.value with
    | Block b -> map_flat helper b
    | Declaration x -> table_var x
    | If (_, _, s, e) ->
      e
      |> Option.map (map_flat helper)
      |> Option.default []
      |> List.append (map_flat helper s)
    | Loop l ->
      (match l with
      | While (_, l) -> map_flat helper l
      | For (_, _, _, l) -> map_flat helper l)
    | Switch (_, _, l) ->
      l
      |> List.map (fun (_, x) -> map_flat helper x)
      |> List.flatten
    | _ -> []
  in
  (name, ret)::(List.append args (map_flat helper body)) *)

(* let build_table prog =
  let _, decl = prog in
  decl
  |> List.fold_left (fun context d ->
    match d.value with
    | Var v -> { context with bindings = List.append context.bindings [] }
    | Type t -> context
    | Fct f -> context
  ) *)
