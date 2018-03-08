open Astwithposition
open Utils
open BatOption

(*
  Interesting functions
  List.assoc_opt -> find
  List.mem_assoc

  #### TO SOLVE ####
  0. really ugly AST duplication and type conversion (Done)
  1. base types (or types in general) are weak representation
  2. need to resolve for underlying type
  4. context should also include info about type declarations (Done)
*)

(* a = (name, type) list list b = decl *)
module Option = BatOption
let bind x f = Option.bind f x
let id x = x

let map_flat f l =
  l
  |> List.map f
  |> List.flatten

let some x = Some x

let id_undeclared id = Printf.sprintf "Variable %s is used before being declared" id
let binary_different_types = Printf.sprintf "Expecting both expressions to be of type bool but got type yolo"

let base_int = "int"
let base_float = "float"
let base_string = "string"
let base_bool = "bool"
let base_rune = "rune"

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
    types = [];
    parent = None
  }

let to_tnode (e: exp node) t = { position = e.position; typ = t; value = e.value }

let lookup table name: typesRef = TypeR "not implemented"
let type_exists (r: typesRef): typesDef option = None
let resolve (t: typesRef) (scope: scope) = None

let merge (old_scope: scope) (new_scope: scope) : scope =
  { old_scope with
    bindings = List.append old_scope.bindings new_scope.bindings;
    types = List.append old_scope.types new_scope.types
  }

let rec typecheck_stm (s: stmt gen_node) (current: scope) : stmt snode =
  match s with
  | Position e ->
    (match e.value with
    | Block l ->
      let new_scope = { bindings = []; types = []; parent = Some current } in
      let (stms, new_scope) =
        l
        |> List.fold_left (fun (s_nodes, context) g_node ->
          let s = typecheck_stm g_node context in
          let merged = merge context s.scope in
          let s_nodes = List.append s_nodes [Scoped s] in
          (s_nodes, merged)
        ) ([], new_scope) in
      { position = e.position; scope = new_scope; value = Block stms }
    | _ -> failwith "")
  | Typed e -> failwith ""
  | Scoped e -> e

(* converts a exp node to exp enode (node with type) *)
(* type rules are not implemented, just trying to get "best" structure for everything *)
let rec typecheck_exp (e: exp gen_node) (scope: scope): (exp tnode) option =
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
      typecheck_exp a scope
      |> bind (fun a ->
        typecheck_exp b scope
        |> bind (fun b ->
          match bin with
          | Plus ->
            (match a.typ, b.typ with
            | TypeR x, TypeR y ->
              if x = base_int && y = base_int then None (* TODO *)
              else None
            | _ -> None)
          | _ -> None
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None
