open Astwithposition
open Utils
open BatOption

module Option = BatOption
let bind x f = Option.bind f x
let id x = x
let inline (<|) f x = f x

let map_flat f l =
  l
  |> List.map f
  |> List.flatten

let rev_assoc l =
  l
  |> List.map (fun (a,b) -> (b,a))

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
    base_int, (TypeT base_int);
    base_float, (TypeT base_float);
    base_string, (TypeT base_string);
    base_bool, (TypeT base_bool);
    base_rune, (TypeT base_rune)
  ]

let top_level =
  {
    bindings = [];
    types = base_types;
    functions = [];
    parent = None
  }

let to_tnode (e: exp node) t = { position = e.position; typ = t; value = e.value }

(* before calling this function, need to resolve the types to get base types *)
(* TODO, do the resolving here instead *)
let check_ops (a: typesDef) (b: typesDef) (l: base_types list) comparable: string option =
  l
  |> List.map (fun t ->
    let t = to_string t in
    match a, b with
    | TypeT x, TypeT y ->
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
let check_op (a: typesDef) (l: base_types list) =
  l
  |> List.map (fun t ->
    let t = to_string t in
    match a with
    | TypeT x -> if x = t then Some t else None
    | _ -> None
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> x
    | None -> print_string "some error message"; None
  )

let try_base_type (s: string) =
  if s = base_int then s |> some
  else if s = base_float then s |> some
  else if s = base_string then s |> some
  else if s = base_bool then s |> some
  else if s = base_rune then s |> some
  else None

(* checks if a list of vars are already declared in the current scope *)
let check_vars_declared (scope: scope) (names: string list) =
    let onames =
      names
      |> List.map (fun name ->
        if List.mem_assoc name scope.bindings = true then None else Some name
      )
      |> List.filter is_some in
    if List.length onames <> List.length names then true else false

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
  | SliceT (t, _) as slice ->
    lookup_type scope t
    |> bind (fun x -> Some slice)
  | ArrayT (t, _) as a ->
    lookup_type scope t
    |> bind (fun x -> Some a)

let lookup_typeref (scope: scope) (t: typesRef) =
  match t with
  | TypeR typ
  | ArrayR (typ, _)
  | SliceR (typ, _) -> lookup_type scope typ

let part_of_list (start: int) l =
  l
  |> List.mapi (fun i x -> if i < start then None else Some x)
  |> List.filter is_some
  |> List.map Option.get

(* TODO: Split into both kinds *)
let rec lookup_kind_elem (scope: scope) (e: kind_elem) =
  let try_in_scope =
    match e with
    | Variable name ->
      scope.bindings
      |> List.assoc_opt name
      |> bind (fun x -> lookup_typedef scope x)
    | Array (name, exps) ->
      scope.bindings
      |> List.assoc_opt name
      |> bind (fun x ->
        lookup_typedef scope x
        |> bind (fun t ->
          match x with
          | ArrayT (typ, sizes) ->
            let typed_exps =
              exps
              |> List.map (fun x -> typecheck_exp scope x)
              |> List.map (fun x ->
                x
                |> bind (fun x ->
                  lookup_typedef scope x.typ
                  |> bind (fun x ->
                    if x = TypeT base_int then Some x else None
                  )
                )
              ) in
            if List.length typed_exps = List.length exps then
              if List.length exps = List.length sizes then TypeT typ |> some
              else if List.length exps < List.length sizes then
                let new_dimension = part_of_list (List.length exps) sizes in
                ArrayT (typ, new_dimension) |> some
              else None
            else None
          | _ -> None
        )
      ) in
    (match try_in_scope with
    | Some t as found -> found
    | None ->
      scope.parent
      |> bind (fun x -> lookup_kind_elem x e))

and lookup_kind (scope: scope) (kind: kind): typesDef option =
  let rec helper (scope: scope) (kind: kind) (typ: typesDef option) =
    match kind with
    | [] -> None
    | x::[] -> typ
    | x::y::xs ->
      let member =
        match y with
        | Variable n
        | Array (n, _) -> n in
      typ
      |> bind (fun def ->
        match def with
        | StructT members ->
          members
          |> List.map (fun (names, t) ->
            names
            |> List.find_opt (fun x -> x = member)
            |> bind (fun _ -> helper scope (y::xs) (Some t))
          )
          |> List.find_opt is_some
          |> bind id
        | _ -> None
      ) in
    match kind with
    | [] -> None
    | x::[] -> lookup_kind_elem scope x
    | x::xs -> helper scope kind (lookup_kind_elem scope x)

(* converts a exp node to exp enode (node with type) *)
(* type rules are not implemented, just trying to get "best" structure for everything *)
(* TODO Support for Append and Function calls *)
and typecheck_exp (scope: scope) (e: exp gen_node): (exp tnode) option =
  match e with
  | Position e ->
    (match e.value with
    | Id kind ->
      lookup_kind scope kind
      |> bind (fun d ->
        to_tnode e d |> some
      )
    | Int i -> to_tnode e (TypeT base_int) |> some
    | Float f -> to_tnode e (TypeT base_float) |> some
    | RawStr s
    | String s -> to_tnode e (TypeT base_string) |> some
    | Bool b -> to_tnode e (TypeT base_bool) |> some
    | Rune r -> to_tnode e (TypeT base_rune) |> some
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
            to_tnode e (TypeT x) |> some
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
          to_tnode e (TypeT x) |> some
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None

let merge (old_scope: scope) (new_scope: scope) : scope =
  let rec helper old_scope new_bindings =
    match new_bindings with
    | [] -> old_scope
    | (name, typ)::xs ->
      if List.mem_assoc name old_scope = true then helper old_scope xs
      else helper (List.append old_scope [(name, typ)]) xs in
  { old_scope with
    bindings = helper old_scope.bindings new_scope.bindings;
    types = helper old_scope.types new_scope.types
  }

let new_scope parent = { bindings = []; types = []; functions = []; parent = Some parent }

let rec typecheck_simple (s: simpleStm gen_node) (current: scope) =
  match s with
  | Position e ->
    (match e.value with
    | Assign (assign_type, (kinds, exps)) ->
      None
    | _ -> None)
  | _ -> failwith "wut wut"

(* TODO typecheck the types in struct declaration *)
(* TODO typecheck the declarations before storing new binding *)
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

and type_context_check l scope =
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

let rec type_ref_to_def (scope: scope) (t: typesRef) =
  match t with
  | ArrayR (typ, l) -> ArrayT (typ, l) |> some
  | SliceR (typ, l) -> SliceT (typ, l) |> some
  | TypeR typ ->
    let ot =
      scope.types
      |> List.assoc_opt typ in
    match ot with
    | Some typ -> Some typ
    | None ->
      scope.parent
      |> bind (fun x -> type_ref_to_def x t)

let check_and_scope l check_func init_scope =
  l
  |> List.fold_left (fun acc decl ->
    acc
    |> bind (fun (acc_scope, acc_decls) ->
      check_func acc_scope decl
      |> bind (fun (d, scope) ->
        let new_scope = merge acc_scope scope in
        let new_decls = List.append acc_decls [d] in
        Some (new_scope, new_decls)
      )
    )
  ) (Some (init_scope, []))

let typecheck_fct_args (args: argument list) (scope: scope) =
  List.map (fun (_, t) -> lookup_typeref scope t)

let print_scope scope =
  print_string "SCOPE: ";
  scope.bindings
  |> List.iter (fun (name, _) -> print_endline name;)

let typecheck_var_decl (scope: scope) ((vars, t, exps): string list * typesRef option * (exp gen_node) list) =
  let otyped_exps =
    exps
    |> List.map (fun exp -> typecheck_exp scope exp)
    |> List.filter is_some in
  if List.length otyped_exps <> List.length exps then None
  else
    if check_vars_declared scope vars = true then None
    else
      let typed_exps =
        otyped_exps
        |> List.map Option.get in
      match t with
      | None ->
        let exps =
          typed_exps
          |> List.map (fun x -> Typed x) in
        let new_bindings =
          typed_exps
          |> List.map2 (fun name node -> (name, node.typ)) vars in
        let new_scope = merge scope { scope with bindings = new_bindings } in
        ((vars, t, exps), new_scope) |> some
      | Some t ->
        lookup_typeref top_level t
        |> bind (fun typ ->
          let exps_with_annotation =
            typed_exps
            |> List.filter (fun x -> x.typ = typ)
            |> List.map (fun x -> Typed x) in
          if List.length exps_with_annotation <> List.length typed_exps then None
          else
            let new_bindings =
              vars
              |> List.map (fun name -> (name, typ)) in
            let new_scope = merge scope { scope with bindings = new_bindings } in
            ((vars, Some t, exps), new_scope) |> some
        )

let typecheck_decl scope decl =
  match decl with
  | Position x ->
    (match x.value with
    | Var l ->
      check_and_scope l typecheck_var_decl scope
      |> bind (fun (scope, ol) ->
          (scope, Var ol) |> some
      )
    | Type decls ->
      let typed_decls =
        decls
        |> List.map (fun (name, x) ->
          lookup_typedef scope x
          |> bind (fun typ -> Some (name, typ)))
        |> List.filter is_some in
      if List.length typed_decls <> List.length decls then None
      else
        let new_types =
          typed_decls
          |> List.map Option.get in
        let new_scope = { scope with types = List.append scope.types new_types } in
        (new_scope, Type new_types) |> some
    | Fct (name, args, typ, stmts) ->
      type_context_check stmts scope
      |> bind (fun (typed_stmts, new_scope) ->
        let typed_stmts = List.map (fun x -> Scoped x) typed_stmts in
        (new_scope, Fct (name, args, typ, typed_stmts)) |> some
      )
    ) (* add the func to scope, typecheck the body first *)
  | _ -> None

let typecheck (p: program) =
  let package, decls = p in
  let typed_decls =
    decls
    |> List.fold_left (fun acc decl ->
      acc
      |> bind (fun (acc_scope, acc_decls) ->
        typecheck_decl acc_scope decl
        |> bind (fun (new_scope, new_decl) ->
          let new_scope = merge acc_scope new_scope in
          let new_decls = List.append acc_decls [new_decl] in
          Some (new_scope, new_decls)
        )
      )
    ) (Some (top_level, [])) in
  typed_decls
