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
  5. need to add function signatures to symbol table
*)

(* a = (name, type) list list b = decl *)
module Option = BatOption
let bind x f = Option.bind f x
let id x = x
let inline (<|) f x = f x

let map_flat f l =
  l
  |> List.map f
  |> List.flatten

let some x = Some x

let id_undeclared id = Printf.sprintf "Variable %s is used before being declared" id
let binary_different_types t1 t2 = Printf.sprintf "Expecting both expressions to be of type %s but got type %s and %s" t1 t1 t2

type base_types =
  | Int
  | Float
  | String
  | Bool
  | Rune

type resolved_type =
  | Base of base_types
  | Struct of (string list * typesDef) list

let base_int = "int"
let base_float = "float"
let base_string = "string"
let base_bool = "bool"
let base_rune = "rune"

let to_string (b: base_types) =
  match b with
  | Int -> base_int
  | Float -> base_float
  | String -> base_string
  | Bool -> base_bool
  | Rune -> base_rune

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

let try_base_type (s: string) =
  if s = base_int then s |> some
  else if s = base_float then s |> some
  else if s = base_string then s |> some
  else if s = base_bool then s |> some
  else if s = base_rune then s |> some
  else None

let rec lookup_type (scope: scope) (name: string) =
  let t =
    scope.types
    |> List.assoc_opt name
    |> bind (fun x -> lookup_typedef scope x) in
  match t with
  | Some t as found -> found
  | None ->
    scope.parent
    |> bind (fun x -> lookup_type x name)
and lookup_typedef (scope: scope) (t: typesDef) =
  match t with
  | TypeT s ->
    (match try_base_type s with
    | Some s -> TypeT s |> some
    | None -> lookup_type scope s)
  | StructT members as def ->
    let l =
      members
      |> List.map (fun (_, t) -> t)
      |> List.map (fun t -> lookup_typedef scope t)
      |> List.filter is_some in
    if List.length l <> List.length members then None else def |> some
  | SliceT (t, _) as a ->
    lookup_type scope t
    |> bind (fun x -> Some a)
  | ArrayT (t, _) as a ->
    lookup_type scope t
    |> bind (fun x -> Some a)

let lookup_typeref (scope: scope) (t: typesRef) =
  match t with
  | TypeR s -> lookup_type scope s
  | ArrayR (s, _) -> lookup_type scope s
  | SliceR (s, _) -> lookup_type scope s

let rec lookup_kind_elem (scope: scope) (e: kind_elem) =
  let name =
    match e with
    | Variable name -> name
    | Array (name, _) -> name in
  scope.bindings
  |> List.assoc_opt name
  |> bind (fun x -> lookup_typeref scope x)

let rec lookup_kind (scope: scope) (kind: kind): typesDef option =
  match kind with
  | [] -> None
  | x::[] -> lookup_kind_elem scope x
  | x::y::xs ->
    let member =
      match y with
      | Variable n
      | Array (n, _) -> n in
    lookup_kind_elem scope x
    |> bind (fun def ->
      match def with
      | TypeT s as t -> Some t
      | StructT members ->
        members
        |> List.map (fun (names, t) ->
          names
          |> List.find_opt (fun x -> x = member)
          |> bind (fun x ->
            if lookup_typedef scope t = lookup_kind_elem scope y then
              lookup_kind scope (y::xs)
            else
              None
          )
        )
        |> List.find_opt is_some
        |> bind id
      | _ -> None
    )

let merge (old_scope: scope) (new_scope: scope) : scope =
  { old_scope with
    bindings = List.append old_scope.bindings new_scope.bindings;
    types = List.append old_scope.types new_scope.types
  }

let new_scope parent = { bindings = []; types = []; parent = Some parent }

(* before calling this function, need to resolve the types to get base types *)
let check_ops (a: typesRef) (b:typesRef) (l: base_types list) comparable: string option =
  l
  |> List.map (fun t ->
    let t = to_string t in
    match a, b with
    | TypeR x, TypeR y ->
      if x = t && y = t then Some t
      else None
    | _ -> None (* TODO not sure what are the rules for this *)
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> if comparable then Some base_bool else x
    | None -> print_string "Error: ..."; None
  )

(* similar to above but for unary ops, might be able to refactor this into one larger function *)
let check_op (a: typesRef) (l: base_types list) =
  l
  |> List.map (fun t ->
    let t = to_string t in
    match a with
    | TypeR x -> if x = t then Some t else None
    | _ -> None
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> x
    | None -> print_string "some error message"; None
  )

(* converts a exp node to exp enode (node with type) *)
(* type rules are not implemented, just trying to get "best" structure for everything *)
(* TODO Support for Append and Function calls *)
let rec typecheck_exp (scope: scope) (e: exp gen_node): (exp tnode) option =
  match e with
  | Position e ->
    (match e.value with
    | Id kind ->
      let todo = lookup_kind scope kind in None (* TODO *)
    | Int i -> to_tnode e (TypeR base_int) |> some
    | Float f -> to_tnode e (TypeR base_float) |> some
    | RawStr s
    | String s -> to_tnode e (TypeR base_string) |> some
    | Bool b -> to_tnode e (TypeR base_bool) |> some
    | Rune r -> to_tnode e (TypeR base_rune) |> some
    | BinaryOp (bin, (a, b)) ->
      typecheck_exp scope a
      |> bind (fun a ->
        typecheck_exp scope b
        |> bind (fun b -> (* TODO resolve the types *)
          let types, comparable =
            match bin with
            | Plus -> [Int; Float; String], false
            | Minus
            | Times
            | Div -> [Int; Float], false
            | Equals
            | NotEquals -> [Int; Float; String; Bool; Rune], true
            | And
            | Or -> [Bool], true
            | Smaller
            | Greater
            | SmallerEq
            | GreaterEq -> [Int; Float], true (* TODO ordered? *)
            | DGreater
            | DSmaller
            | AndHat
            | BAnd
            | BOr
            | Caret
            | Mod -> [Int], false in
          check_ops a.typ b.typ types comparable
          |> bind (fun x ->
            to_tnode e (TypeR x) |> some
          )
        )
      )
    | Unaryexp (un, a) ->
      typecheck_exp scope a
      |> bind (fun a ->
        let types =
          match un with
          | UCaret -> [Int; Rune]
          | UMinus
          | UPlus -> [Int; Float; Rune]
          | Not -> [Bool] in
        check_op a.typ types
        |> bind (fun x ->
          to_tnode e (TypeR x) |> some
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None


let rec typecheck_stm (s: stmt gen_node) (current: scope) : (stmt snode) option =
  match s with
  | Position e ->
    (* helper to avoid duplicated code *)
    let print_helper (l: exp gen_node list) (ln: bool) =
      let tnodes =
        l
        |> List.map (fun x -> typecheck_exp current x)
        |> List.filter is_some in
      if List.length l <> List.length tnodes then None
      else
        let lst = (List.map (fun x -> Typed (Option.get x)) tnodes) in
        Some { position = e.position; scope = current; value = if ln then Println lst else Print lst } in
    (match e.value with
    | Block l ->
      let new_scope = new_scope current in
      type_context_check l new_scope
      |> bind (fun (stms, new_scope) ->
        Some { position = e.position; scope = new_scope; value = Block (List.map (fun x -> Scoped x) stms) }
      )
    | Print l -> print_helper l false
    | Println l -> print_helper l true
    (* | If  *)
    | _ -> failwith "not implemented")
  | Typed e -> None
  | Scoped e -> Some e

and type_context_check (l: stmt gen_node list) (scope: scope): (stmt snode list * scope) option =
    l
    |> List.fold_left (fun acc g_node ->
      acc
      |> bind (fun (s_nodes, context) ->
        typecheck_stm g_node context
        |> bind (fun s ->
          let merged = merge context s.scope in
          let s_nodes = List.append s_nodes [s] in
          Some (s_nodes, merged)
        )
      )
    ) (Some ([], scope))
