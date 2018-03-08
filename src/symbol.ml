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
let inline (<|) f x = f x

let map_flat f l =
  l
  |> List.map f
  |> List.flatten

let compare_length n l = n = List.length l

let some x = Some x

let id_undeclared id = Printf.sprintf "Variable %s is used before being declared" id
let binary_different_types t1 t2 = Printf.sprintf "Expecting both expressions to be of type %s but got type %s and %s" t1 t1 t2

type base_types =
  | Int
  | Float
  | String
  | Bool
  | Rune

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

let rec lookup (scope: scope) (name: string): typesRef option =
  let binding =
    scope.bindings
    |> List.assoc_opt name in
  match binding with
  | Some t -> Some t
  | None ->
    match scope.parent with
    | Some p -> lookup p name
    | None -> print_string (id_undeclared name); None

(* let type_exists (r: typesRef): typesDef option = None *)
(* let resolve (scope: scope) (t: typesRef) =  *)

let merge (old_scope: scope) (new_scope: scope) : scope =
  { old_scope with
    bindings = List.append old_scope.bindings new_scope.bindings;
    types = List.append old_scope.types new_scope.types
  }

let new_scope parent = { bindings = []; types = []; parent = Some parent }

let check_ops (a: typesRef) (b:typesRef) (l: base_types list): string option =
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
  |> bind id


(* converts a exp node to exp enode (node with type) *)
(* type rules are not implemented, just trying to get "best" structure for everything *)
let rec typecheck_exp (e: exp gen_node) (scope: scope): (exp tnode) option =
  match e with
  | Position e ->
    (match e.value with
    | Id ids ->
      let test =
        ids
        |> List.map (fun id ->
          match id with
          | Variable v -> lookup scope v
          | Array (n, _) -> lookup scope n (* TODO *)
        ) in
      if List.length test <> List.length ids then None else None (* TODO what's the resulting type? *)
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
              if x = base_int then
                (if y = base_int then
                  to_tnode e (TypeR base_int) |> some
                else
                  (print_string (binary_different_types base_int base_string);
                  None))
              (* else if x = base_string then *)
              else None
            | _ -> None)
          | _ -> None
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None

let rec typecheck_stm (s: stmt gen_node) (current: scope) : (stmt snode) option =
  match s with
  | Position e ->
    (match e.value with
    | Block l ->
      let new_scope = new_scope current in
      type_context_check l new_scope
      |> bind (fun (stms, new_scope) ->
        Some { position = e.position; scope = new_scope; value = Block (List.map (fun x -> Scoped x) stms) }
      )
    | Print l ->
      let tnodes =
        l
        |> List.map (fun x -> typecheck_exp x current)
        |> List.filter is_some in
      if List.length l <> List.length tnodes then None
      else
        Some { position = e.position; scope = current; value = Print (List.map (fun x -> Typed (Option.get x)) tnodes) }
    | _ -> failwith "")
  | Typed e -> failwith ""
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
