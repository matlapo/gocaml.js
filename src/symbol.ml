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

let get_rest_of_list (start: int) l =
  l
  |> List.mapi (fun i x -> if i < start then None else Some x)
  |> List.filter is_some
  |> List.map Option.get

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
let base_float = "float64"
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

let try_base_type (s: string) =
  if s = base_int then s |> some
  else if s = base_float then s |> some
  else if s = base_string then s |> some
  else if s = base_bool then s |> some
  else if s = base_rune then s |> some
  else None

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
    parent = None;
    children = []
  }

let new_scope parent = { bindings = []; types = []; functions = []; parent = Some parent; children = [] }
let empty_scope = { bindings = []; types = []; functions = []; parent = None; children = [] }

let typesdef_to_string d =
  match d with
  | TypeT s -> s
  | StructT _ -> "struct"
  | ArrayT (typ, _) -> Printf.sprintf "%s array" typ
  | SliceT (typ, _) -> Printf.sprintf "%s slice" typ

(* given a scope, prints all of its children *)
let rec print_scope scope =
  print_endline "##### START OF SCOPE #####";
  let _ =
    print_endline "#Name bindings#";
    scope.bindings
    |> List.iter (fun (name, typ) ->
      let s = Printf.sprintf "%s: %s" name (typesdef_to_string typ) in
      print_endline s;
    ) in
  let _ =
    print_endline "#Type bindings#";
    scope.types
    |> List.iter (fun (name, typ) ->
      let s = Printf.sprintf "%s: %s" name (typesdef_to_string typ) in
      print_endline s;
    ) in
  let _ =
    print_endline "#Function bindings#";
    scope.functions
    |> List.iter (fun (name, args, oreturn_type) ->
      let s = Printf.sprintf "%s: function" name in
      print_endline s;
    ) in
  List.iter (fun x -> print_scope x) scope.children

(* given the full symbol table, prints all of its content *)
let print_symbol_table scope = print_scope scope

(* converts a regular node to a node with the given type *)
let tnode_of_node (e: exp node) t = { position = e.position; typ = t; value = e.value }

(* given a scope and a type reference, try finding the associated type definition *)
let rec def_of_ref_opt scope t =
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
      |> bind (fun x -> def_of_ref_opt x t)

(* given the types of 2 arguments (for a binary operation) and a list of accepted types for the binary operation, return the resulting type *)
let check_ops_opt a b l comparable =
  l
  |> List.map (fun t ->
    let t = to_string t in
    match a, b with
    | TypeT x, TypeT y -> if x = t && y = t then Some t else None
    | _ -> None
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> if comparable then Some base_bool else x
    | None -> print_string "Error: ..."; None
  )

(* given type of an argument (for a unary operation) and a list of accepted types for the operation, return the resulting type *)
let check_op a l =
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
    | None -> print_string "Error: some error message"; None
  )

(* checks if any variable name in a list is already declared in the current scope *)
let check_vars_declared scope names =
    let onames =
      names
      |> List.map (fun name ->
        if List.mem_assoc name scope.bindings = true then None else Some name
      )
      |> List.filter is_some in
    if List.length onames <> List.length names then true else false

(* given a name and a scope, tries to find the associated type definition starting from the given scope up to the top-level scope *)
let rec type_of_name_opt scope name =
  let t =
    scope.types
    |> List.assoc_opt name
    |> bind (fun x -> resolve_typedef_opt scope x) in
  match t with
  | Some t as found -> found
  | None ->
    scope.parent
    |> bind (fun x -> type_of_name_opt x name)

(* given a type definition, make sure that it is a well-defined type (meaning that all the types it refer are also valid).
Additionally, if the type is a TypeT (so basically just a name) it will try to convert it to a base type, an array, struct or slice (in other words, anything but simply a name!) *)
and resolve_typedef_opt scope t =
  print_endline "HERE";
  match t with
  | TypeT s ->
    print_endline "TYPE";
    (match try_base_type s with
    | Some s -> TypeT s |> some
    | None -> type_of_name_opt scope s)
  | StructT members as def ->
    let l =
      members
      |> List.map (fun (_, t) -> t)
      |> List.map (fun t -> resolve_typedef_opt scope t)
      |> List.filter is_some in
    if List.length l <> List.length members then None else def |> some
  | SliceT (t, _) as slice ->
    type_of_name_opt scope t
    |> bind (fun x -> Some slice)
  | ArrayT (t, _) as a ->
    print_endline "ARRAY";
    type_of_name_opt scope t
    |> bind (fun x -> Some a)

(* given a type reference, it will try to find the associated type definition in the current scope or above, and then resolve the type definition.
Note that we type check every type declarations before adding them to the symbole table, BUT we still resolve the type here to get something other than just a type name because
when this function is called, we not only want to know if the type is valid, but also what is it precisely (is it an array, struct, etc?)*)
let resolve_typeref_opt scope t =
  match t with
  | TypeR typ
  | ArrayR (typ, _)
  | SliceR (typ, _) -> type_of_name_opt scope typ

let check_if_exps_are_all_ints scope exps typecheck_func =
  exps
  |> List.map (fun x -> typecheck_func scope x)
  |> List.map (fun x ->
    x
    |> bind (fun x ->
      resolve_typedef_opt scope x.typ
      |> bind (fun x ->
        if x = TypeT base_int then Some x else None
      )
    )
  )

(* given an array name and a list of expressions (i.e my_array[2][3+6]), return the resulting type, if possible*)
let typedef_of_array_opt scope (name, exps) typecheck_func =
  scope.bindings
  |> List.assoc_opt name
  |> bind (fun x ->
    resolve_typedef_opt scope x
    |> bind (fun t ->
      match x with
      | ArrayT (typ, sizes) ->
        let typed_exps = check_if_exps_are_all_ints scope exps typecheck_func in
        if List.length typed_exps = List.length exps then
          if List.length exps = List.length sizes then TypeT typ |> some
          else if List.length exps < List.length sizes then
            let new_dimension = get_rest_of_list (List.length exps) sizes in
            ArrayT (typ, new_dimension) |> some
          else None
        else None
      | _ -> None
    )
  )

(* given a kind element (i.e a variable name or an array name with a list of expressions) returns the resulting type *)
let rec typecheck_kind_element_opt scope e =
  print_endline "WUT";
  let try_in_scope =
    match e with
    | Variable name ->
      scope.bindings
      |> List.assoc_opt name
      |> bind (fun x -> resolve_typedef_opt scope x)
    | Array (name, exps) -> typedef_of_array_opt scope (name, exps) typecheck_exp_opt in
    (match try_in_scope with
    | Some t as found -> found
    | None ->
      scope.parent
      |> bind (fun x -> typecheck_kind_element_opt x e))

(* given a type, checks that it is of type stuct and that the given name is one of its members *)
and check_struct_member_opt typ member cont =
  typ
  |> bind (fun def ->
    match def with
    | StructT members ->
      members
      |> List.map (fun (names, t) ->
        names
        |> List.find_opt (fun x -> x = member)
        |> bind (fun _ -> cont t)
      )
      |> List.find_opt is_some
      |> bind id
    | _ -> None
  )

(* given a list of kind_elements, type check the whole sequence and return the resulting type if possible *)
and typecheck_kind_opt scope kind =
  let rec helper scope kind typ =
    match kind with
    | [] -> None
    | x::[] -> typ
    | x::y::xs ->
      let member =
        match y with
        | Variable n
        | Array (n, _) -> n in
      let rest_of_sequence = y::xs in
      (check_struct_member_opt typ member (fun t -> helper scope rest_of_sequence (Some t))) in
    match kind with
    | [] -> None
    | x::[] -> typecheck_kind_element_opt scope x
    | x::xs -> helper scope kind (typecheck_kind_element_opt scope x)

(* given an expression node, typecheck_opt the expression and return an expression tnode (a node record with a field for its type) *)
and typecheck_exp_opt scope e =
  match e with
  | Position e ->
    (match e.value with
    | Id kind ->
      typecheck_kind_opt scope kind
      |> bind (fun d ->
        tnode_of_node e d |> some
      )
    | Int i -> tnode_of_node e (TypeT base_int) |> some
    | Float f -> tnode_of_node e (TypeT base_float) |> some
    | RawStr s
    | String s -> tnode_of_node e (TypeT base_string) |> some
    | Bool b -> tnode_of_node e (TypeT base_bool) |> some
    | Rune r -> tnode_of_node e (TypeT base_rune) |> some
    | BinaryOp (bin, (a, b)) ->
      typecheck_exp_opt scope a
      |> bind (fun a ->
        typecheck_exp_opt scope b
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
          check_ops_opt a.typ b.typ types comparable
          |> bind (fun x ->
            tnode_of_node e (TypeT x) |> some
          )
        )
      )
    | Unaryexp (un, a) ->
      typecheck_exp_opt scope a
      |> bind (fun a ->
        let types =
          match un with
          | UCaret -> [Int; Rune]
          | UMinus
          | UPlus -> [Int; Float; Rune]
          | Not -> [Bool] in
        check_op a.typ types
        |> bind (fun x ->
          tnode_of_node e (TypeT x) |> some
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None

(* given two scopes, merge them together so that all their bindings are all at the same level. If the resulting bindings contain duplicates, it returns None *)
let merge_scope_opt old_scope new_scope =
  let rec helper old_scope new_bindings =
    match new_bindings with
    | [] -> Some old_scope
    | (name, typ)::xs ->
      if List.mem_assoc name old_scope = true then (print_string "Error: "; print_endline name; None)
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

(* given the arguments of a function (i.e (name, type reference)), typecheck all the type references *)
let typecheck_args_opt scope args =
  let typed_args =
    args
    |> List.map (fun (name, typ) -> resolve_typeref_opt scope typ)
    |> List.filter is_some in
  if List.length args <> List.length typed_args then None
  else
    typed_args
    |> List.map Option.get
    |> some

(* given a simple statement node, typecheck it *)
let rec typecheck_simple_opt current s =
  match s with
  | Position e ->
    (match e.value with
    | Assign (assign_type, (kinds, exps)) ->
      let typ =
        match assign_type with
        | Regular -> None
        | PlusEqual
        | MinusEqual
        | DivEqual
        | TimesEqual -> Some [Int; Float]
        | AndEqual
        | OrEqual
        | HatEqual
        | PercentEqual
        | AndHatEqual -> Some [Int]
        | DoubleGreaterEqual
        | DoubleSmallerEqual -> Some [Int] in
      (match typ with
      | None ->
        let otyped_kinds =
          kinds
          |> List.map (fun x -> typecheck_kind_opt current x)
          |> List.filter is_some in
        if List.length otyped_kinds <> List.length kinds then None
        else
          let typed_kinds = List.map Option.get otyped_kinds in
          let otyped_exps =
            exps
            |> List.map (fun x -> typecheck_exp_opt current x)
            |> List.filter is_some in
          if List.length otyped_exps <> List.length exps then None
          else
            let typed_exps = List.map Option.get otyped_exps in
            let test =
              typed_kinds
              |> List.map2 (fun exp kind -> kind = exp.typ) typed_exps
              |> List.exists (fun x -> x = false) in
            if test then None
            else
              let gen_nodes = List.map (fun x -> Typed x) typed_exps in
              Scoped { position = e.position; scope = current; value = Assign (assign_type, (kinds, gen_nodes)) } |> some
      | Some l -> None)
    | Empty -> Scoped { position = e.position; scope = current; value = Empty } |> some
    | _ -> None)
  | _ -> failwith "wut wut"

(* print helper function for typecheck print and println statements *)
let print_helper current (e: stmt node) l is_println =
  let tnodes =
    l
    |> List.map (fun x -> typecheck_exp_opt current x)
    |> List.filter is_some in
  if List.length l <> List.length tnodes then None
  else
    let lst = (List.map (fun x -> Typed (Option.get x)) tnodes) in
    Some { position = e.position; scope = current; value = if is_println then Println lst else Print lst }

(* given a statement node, typecheck it *)
let rec typecheck_stm_opt current s =
  match s with
  | Position e ->
    (match e.value with
    | Block l ->
      let new_scope = new_scope current in
      typecheck_stm_list_opt l new_scope
      |> bind (fun (stms, new_scope) ->
        let new_scope = { current with children = [new_scope] } in
        Some { position = e.position; scope = new_scope; value = Block (List.map (fun x -> Scoped x) stms) }
      )
    | If (osimple, oexp, ifs, oelses) ->
      let simple_scope = new_scope current in
      let typed_exp =
        typecheck_exp_opt current oexp
        |> bind (fun typed -> Typed typed |> some) in
      let typed_simple =
        osimple
        |> bind (fun simple ->
          typecheck_simple_opt simple_scope simple
        ) in
      let typed_elses =
        oelses
        |> bind (fun elses ->
          typecheck_stm_list_opt elses current
          |> bind (fun (snodes, _) ->
            snodes
            |> List.map (fun x -> Scoped x)
            |> some
          )
        ) in
      typed_exp
      |> bind (fun typed_exp ->
        match oelses with
        | None ->
          (match osimple with
          | None -> if_helper current ifs e None typed_exp None
          | Some _ ->
            typed_simple
            |> bind (fun typed_simple ->
              if_helper current ifs e (Some typed_simple) typed_exp None
            ))
        | Some _ ->
          typed_elses
          |> bind (fun typed_else ->
            (match osimple with
            | None -> if_helper current ifs e None typed_exp (Some typed_else)
            | Some _ ->
              typed_simple
              |> bind (fun typed_simple ->
                if_helper current ifs e (Some typed_simple) typed_exp (Some typed_else)
              ))
          )
      )
    | Print l -> print_helper current e l false
    | Println l -> print_helper current e l true
    | Declaration l ->
      typecheck_decl_list_opt l typecheck_var_decl_opt current
      |> bind (fun (scope, ol) ->
          Some { position = e.position; scope = scope; value = Declaration ol }
      )
    | TypeDeclaration decls ->
      let typed_decls =
        decls
        |> List.map (fun (name, x) ->
          resolve_typedef_opt current x
          |> bind (fun typ -> Some (name, typ)))
        |> List.filter is_some in
      if List.length typed_decls <> List.length decls then None
      else
        let new_types =
          typed_decls
          |> List.map Option.get in
        { empty_scope with types = new_types }
        |> merge_scope_opt current
        |> bind (fun new_scope ->
          Some { position = e.position; scope = new_scope; value = TypeDeclaration new_types }
        )
    | Simple simple ->
      typecheck_simple_opt current simple
      |> bind (fun simple ->
        Some { position = e.position; scope = current; value = Simple simple }
      )
    | _ -> failwith "not implemented")
  | Typed e -> None
  | Scoped e -> Some e

and if_helper current ifs (e: stmt node) typed_simple typed_exp typed_elses =
  let if_scope = new_scope current in
  typecheck_stm_list_opt ifs if_scope
  |> bind (fun (stms, new_scope) ->
    let new_current_scope =
      typed_simple
      |> bind (fun simple ->
        match simple with
        | Scoped s ->
          { current with children = [new_scope; s.scope] } |> some
        | _ -> None
      )
      |> default { current with children = [new_scope] } in
    let else_scope =
        typed_elses
        |> bind (fun elses ->
          elses
          |> List.rev
          |> List.hd
          |> (fun x -> match x with Scoped s -> Some s.scope | _ -> None)
      ) in
    let scoped_stms = List.map (fun x -> Scoped x) stms in
    match typed_elses with
    | None ->
        Some { position = e.position; scope = new_current_scope; value = If (typed_simple, typed_exp, scoped_stms, typed_elses) }
    | Some _ ->
      else_scope
      |> bind (fun else_scope ->
        let new_current_scope = { new_current_scope with children = List.append new_current_scope.children [else_scope] } in
        Some { position = e.position; scope = new_current_scope; value = If (typed_simple, typed_exp, scoped_stms, typed_elses) }
      )
  )

(* geiven a list of statement nodes, typecheck_opt them *)
and typecheck_stm_list_opt l scope =
    l
    |> List.fold_left (fun acc g_node ->
      acc
      |> bind (fun (s_nodes, acc_scope) ->
        typecheck_stm_opt acc_scope g_node
        |> bind (fun s ->
          let s_nodes = List.append s_nodes [s] in
          Some (s_nodes, s.scope)
        )
      )
    ) (Some ([], scope))

(*
Takes a list of declarations and for each one it calls a function that will typecheck_opt the declaration and return the typed
declaration along with an updated scope. It 'accumulate' the scopes returned by each declaration and merge_scope_opt all of them
together in order to return a single updated scope containing all the new bindings.
*)
and typecheck_decl_list_opt l check_func init_scope =
  l
  |> List.fold_left (fun acc decl ->
    acc
    |> bind (fun (acc_scope, acc_decls) ->
      check_func acc_scope decl
      |> bind (fun (d, scope) ->
        merge_scope_opt acc_scope scope
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
and typecheck_var_decl_opt scope (vars, t, exps) =
  let otyped_exps =
    exps
    |> List.map (fun exp -> typecheck_exp_opt scope exp)
    |> List.filter is_some in
  if List.length otyped_exps <> List.length exps then None
  else
    if check_vars_declared scope vars then None
    else if contains_duplicate vars then None
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
        resolve_typeref_opt scope t
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
(*
 Depending on the type of declaration (i.e Var, Type or Fct) this function will
 add the new bindings to the given scope and return the updated scope along with
 the same declaration but in a different type of node.
*)
let typecheck_decl_opt scope decl =
  match decl with
  | Position x ->
    (match x.value with
    | Var l ->
      typecheck_decl_list_opt l typecheck_var_decl_opt scope
      |> bind (fun (scope, ol) ->
          (scope, Var ol) |> some
      )
    | Type decls ->
      let typed_decls =
        decls
        |> List.map (fun (name, x) ->
          resolve_typedef_opt scope x
          |> bind (fun typ -> Some (name, typ)))
        |> List.filter is_some in
      if List.length typed_decls <> List.length decls then None
      else
        let new_types =
          typed_decls
          |> List.map Option.get in
        { empty_scope with types = new_types }
        |> merge_scope_opt scope
        |> bind (fun new_scope ->
          (new_scope, Type new_types) |> some
        )
    | Fct (name, args, otyp, stmts) ->
      let empty_function_scope = new_scope scope in
      args
      |> typecheck_args_opt scope
      |> bind (fun typed_args ->
        typecheck_stm_list_opt stmts empty_function_scope
        |> bind (fun (typed_stmts, new_scope) ->
          let typed_stmts = List.map (fun x -> Scoped x) typed_stmts in
          let oreturn_type =
            otyp
            |> bind (fun typ ->
              def_of_ref_opt scope typ
            ) in
          let function_binding = (name, typed_args, oreturn_type) in
          let scope =
            { scope with
              children = List.append scope.children [new_scope];
              functions = List.append scope.functions [function_binding]
            } in
          (scope, Fct (name, args, otyp, typed_stmts)) |> some
        )
      )
    )
  | _ -> None

(*
Root function: for each declaration, it calls typecheck_decl_opt that will return the same
declaration but using a different type of node (a node with a type or a node with a scope).
typecheck_decl_opt also returns an updated scope. Typecheck_decl is called for each declaration
so that's why we use a fold (we accumulate the new scopes).
*)
let typecheck_opt (p: program) =
  let package, decls = p in
  let init_scope = new_scope top_level in
  let typed_decls =
    decls
    |> List.fold_left (fun acc decl ->
      acc
      |> bind (fun (acc_scope, acc_decls) ->
        typecheck_decl_opt acc_scope decl
        |> bind (fun (new_scope, new_decl) ->
          let new_decls = List.append acc_decls [new_decl] in
          Some (new_scope, new_decls)
        )
      )
    ) (Some (init_scope, [])) in
  typed_decls
  |> bind (fun (scope, decls) ->
    let top_level = { top_level with children = [scope] } in
    (* print_scope top_level; *)
    Some decls
  )
