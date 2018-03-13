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


let contains_duplicate l =
  l
  |> List.map (fun x ->
    l
    |> List.find_all (fun name -> x = name)
  )
  |> List.exists (fun x -> List.length x > 1)

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

let typesDef_to_string d =
  match d with
  | TypeT s -> s
  | StructT _ -> "struct"
  | ArrayT (typ, _) -> Printf.sprintf "%s array" typ
  | SliceT (typ, _) -> Printf.sprintf "%s slice" typ

let rec print_scope scope : unit =
  print_endline "##### START OF SCOPE #####";
  let _ =
    print_endline "#Name bindings#";
    scope.bindings
    |> List.iter (fun (name, typ) ->
      let s = Printf.sprintf "%s: %s" name (typesDef_to_string typ) in
      print_endline s;
    ) in
  let _ =
    print_endline "#Type bindings#";
    scope.types
    |> List.iter (fun (name, typ) ->
      let s = Printf.sprintf "%s: %s" name (typesDef_to_string typ) in
      print_endline s;
    ) in
  let _ =
    print_endline "#Function bindings#";
    scope.functions
    |> List.iter (fun (name, args, oreturn_type) ->
      let s = Printf.sprintf "%s: function" name in
      print_endline s;
    ) in
  match scope.parent with
  | Some scope -> print_scope scope
  | None -> ()

let to_tnode (e: exp node) t = { position = e.position; typ = t; value = e.value }

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

let merge (old_scope: scope) (new_scope: scope) : scope option =
  let rec helper old_scope new_bindings =
    match new_bindings with
    | [] -> Some old_scope
    | (name, typ)::xs ->
      if List.mem_assoc name old_scope = true then (print_string "ERROR: "; print_endline name; None)
      else helper (List.append old_scope [(name, typ)]) xs in
  helper old_scope.bindings new_scope.bindings
  |> bind (fun bindings ->
    helper old_scope.types new_scope.types
    |> bind (fun types ->
      { old_scope with
        bindings = bindings;
        types = types
      } |> some
    )
  )

let typecheck_args (scope: scope) (args: argument list) =
  let typed_args =
    args
    |> List.map (fun (name, typ) -> lookup_typeref scope typ)
    |> List.filter is_some in
  if List.length args <> List.length typed_args then None
  else
    typed_args
    |> List.map Option.get
    |> some

let new_scope parent = { bindings = []; types = []; functions = []; parent = Some parent }
let empty_scope = { bindings = []; types = []; functions = []; parent = None }

let rec typecheck_simple (s: simpleStm gen_node) (current: scope) =
  match s with
  | Position e ->
    (match e.value with
    | Assign (assign_type, (kinds, exps)) ->
      None
    | Empty -> Scoped { position = e.position; scope = current; value = Empty } |> some
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
    | Declaration l ->
      check_and_scope l typecheck_var_decl current
      |> bind (fun (scope, ol) ->
          Some { position = e.position; scope = scope; value = Declaration ol }
      )
    | Simple simple ->
      typecheck_simple simple current
      |> bind (fun simple ->
        Some { position = e.position; scope = current; value = Simple simple }
      )
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
          merge context s.scope
          |> bind (fun merged ->
            let s_nodes = List.append s_nodes [s] in
            Some (s_nodes, merged)
          )
        )
      )
    ) (Some ([], scope))

(*
Takes a list of Var declarations and for each one it calls check_fun
(typecheck_var_decl in the case for vars), it 'accumulate' the scopes returned
by each Var declaration and merge all of them together in order to return a single
updated top-level scope.
*)
and check_and_scope l check_func init_scope =
  l
  |> List.fold_left (fun acc decl ->
    acc
    |> bind (fun (acc_scope, acc_decls) ->
      check_func acc_scope decl
      |> bind (fun (d, scope) ->
        merge acc_scope scope
        |> bind (fun new_scope ->
          let new_decls = List.append acc_decls [d] in
          Some (new_scope, new_decls)
        )
      )
    )
  ) (Some (init_scope, []))

(*
this function takes a single Var declaration and checks if it typechecks. If a type annotation is
also given, it makes sure that it matches the type of each expression. This function returns the same
Var declaration but with tnodes (nodes with an extra field representing the type). Note that it assumes
that the number of variables matches the number of expression (this is handled by weeding).
*)
and typecheck_var_decl (scope: scope) ((vars, t, exps): string list * typesRef option * (exp gen_node) list) =
  let otyped_exps =
    exps
    |> List.map (fun exp -> typecheck_exp scope exp)
    |> List.filter is_some in
  if List.length otyped_exps <> List.length exps then None
  else
    if check_vars_declared scope vars = true then None
    else if contains_duplicate vars = true then None
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
        let new_scope = { empty_scope with bindings = new_bindings } in
        ((vars, t, exps), new_scope) |> some
      | Some t ->
        lookup_typeref scope t
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
            let new_scope = { empty_scope with bindings = new_bindings } in
            ((vars, Some t, exps), new_scope) |> some
        )

let add_function_binding (scope: scope) (binding: (string * typesDef list * typesDef option)) =
  match scope.parent with
  | None -> None
  | Some parent ->
    let (name, _, _) = binding in
    let new_bindings =
      parent.functions
      |> List.map (fun (name, _, _) -> name)
      |> List.append [name] in
    if contains_duplicate new_bindings then None
    else
      Some { scope with parent = Some { parent with functions = List.append parent.functions [binding] } }

(*
 Depending on the type of declaration (i.e Var, Type or Fct) this function will
 add the new bindings to the given scope and return the updated scope along with
 the same declaration but in a different type of node.
*)
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
        { empty_scope with types = new_types }
        |> merge scope
        |> bind (fun new_scope ->
          (new_scope, Type new_types) |> some
        )
    | Fct (name, args, typ, stmts) ->
      let empty_function_scope = new_scope scope in
      args
      |> typecheck_args scope
      |> bind (fun typed_args ->
        type_context_check stmts empty_function_scope
        |> bind (fun (typed_stmts, new_scope) ->
          let typed_stmts = List.map (fun x -> Scoped x) typed_stmts in
          let function_binding = (name, typed_args, None) in
          add_function_binding new_scope function_binding
          |> bind (fun new_scope ->
            (new_scope, Fct (name, args, typ, typed_stmts)) |> some
          )
        )
      )
    )
  | _ -> None

(*
Root function: for each declaration, it calls typecheck_decl that will return the same
declaration but using a different type of node (a node with a type or a node with a scope).
typecheck_decl also returns an updated scope. Typecheck_decl is called for each declaration
so that's why we use a fold (we accumulate the new scopes).
*)
let typecheck (p: program) =
  let package, decls = p in
  let init_scope = new_scope top_level in
  let typed_decls =
    decls
    |> List.fold_left (fun acc decl ->
      acc
      |> bind (fun (acc_scope, acc_decls) ->
        typecheck_decl acc_scope decl
        |> bind (fun (new_scope, new_decl) ->
          let new_decls = List.append acc_decls [new_decl] in
          Some (new_scope, new_decls)
        )
      )
    ) (Some (init_scope, [])) in
  typed_decls
  |> bind (fun (scope, decls) ->
    print_scope scope;
    Some decls
  )
