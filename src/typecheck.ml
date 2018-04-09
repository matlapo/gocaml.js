open Ast
open BatOption
open Utils
open Scopeprinter
module Option = BatOption

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

let basetype a = { gotype = Basetype a; scopeid = 0 }

let base_types =
  [
    base_int, basetype BInt;
    base_float, basetype BFloat64;
    base_string, basetype BString;
    base_bool, basetype BBool;
    base_rune, basetype BRune;
  ]

let top_level =
  {
    scopeid = 0;
    bindings = [("true", basetype BBool); ("false", basetype BBool)];
    types = base_types;
    functions = [];
    parent = None;
    children = []
  }

let new_scope parent = { scopeid = Random.int 99999999; bindings = []; types = []; functions = []; parent = Some parent; children = [] }
let empty_scope = { scopeid = Random.int 99999999; bindings = []; types = []; functions = []; parent = None; children = [] }

(*
####################
### MAXIME START ###
####################
*)

(* ### TYPE RESOLVING ### *)

(* Finds a scope with the specified scopeid.
Will only look at the parents of th given scope (including the given scope) *)
let rec find_scope_with_scopeid_opt (s: scope) (id: scopeid): scope option =
  if s.scopeid = id then Some s
  else
    s.parent
    |> bind (fun p -> find_scope_with_scopeid_opt p id)

(** Finds the type associated with a string *)
let rec scopedtype_of_typename_opt (s: scope) (typename: string): scopedtype option =
  (* Look for a type in the current scope *)
  match List.find_opt (fun (id, _) -> id = typename) s.types with
  (* If found, create a scopedtype with the current scope id *)
  | Some (id, t) -> Some { gotype = Defined id; scopeid = s.scopeid }
  | None -> match s.parent with
    (* Look in parent scope *)
    | Some p -> scopedtype_of_typename_opt p typename
    | None -> None

(** Finds the type associated with a variable name *)
let rec scopedtype_of_varname_opt (s: scope) (varname: string): scopedtype option =
  match List.find_opt (fun (id, _) -> id = varname) s.bindings with
  | Some (_, t) -> Some t
  | None -> match s.parent with
    | Some p -> scopedtype_of_varname_opt p varname
    | None -> None

(** Find the basetype related to a scopedtype.
    It can either be a struct, array, slice or a base type.
*)
let rec resolve_to_reducedtype_opt (s: scope) (t: scopedtype): scopedtype option =
  match t.gotype with
  | Defined typename ->
    find_scope_with_scopeid_opt s t.scopeid
    |> bind (fun p -> List.find_opt (fun (id, _) -> id = typename) p.types)
    |> bind (fun (_, t) -> resolve_to_reducedtype_opt s t)
  | _ -> Some t

let basetype_of_string_opt s = match s with
  | "int" -> Some (Basetype BInt)
  | "float64" -> Some (Basetype BFloat64)
  | "string" -> Some (Basetype BString)
  | "rune" -> Some (Basetype BRune)
  | "bool" -> Some (Basetype BBool)
  | _ -> None

let rec scopedtype_of_gotype (s: scope) (t: gotype) : scopedtype option =
  match t with
  | Defined typename ->
    (match List.find_opt (fun (id, _) -> id = typename) s.types with
    | Some _ ->
      if s.scopeid <> 0 then
        Some { gotype = Defined typename; scopeid = s.scopeid }
      (* If the Defined goes back to the initial scope, it is a base type.*)
      else
        (basetype_of_string_opt typename) |> bind (fun t -> Some { gotype = t; scopeid = 0 })
    | None -> s.parent |> bind (fun p -> scopedtype_of_gotype p t))
  | _ -> Some { gotype = t; scopeid = s.scopeid }

(* Find the base type inside of an array. (needs to resolve gotype inside arrays scope) *)
let resolve_inside_type_of_indexable (s: scope) (t: scopedtype) : scopedtype option =
  (* Get the gotype of type inside the array *)
  match t.gotype with
  | Array (insidetype, _)
  | Slice insidetype -> (
    (* Find the scope where the array type was defined *)
    find_scope_with_scopeid_opt s t.scopeid
    |> bind (fun indexablescope ->
      (* Find the scopedtype of the type inside the array *)
      scopedtype_of_gotype indexablescope insidetype
    )
  )
  (* If the type is not indexable, error *)
  | _ -> None

(* Returns the type of a selection *)
let selection_type_opt (s: scope) (t: scopedtype) (a: string) : scopedtype option =
  (* Get the scope where the selection was defined *)
  find_scope_with_scopeid_opt s t.scopeid
  |> bind (fun selectionscope ->
    (* Check if the type resolves to a struct *)
    resolve_to_reducedtype_opt s t
    |> bind (fun resolved_type ->
      match resolved_type.gotype with
      | Struct members ->
        (* Check if the selection is a valid member of the struct *)
        members
        |> List.find_opt (fun (name, _) -> name = a)
        |> bind (fun (_, member_type) ->
          (* Find the scopedtype of the member by doing a type search from the scope where the struct as defined *)
          scopedtype_of_gotype selectionscope member_type
        )
      | _ -> None
    )
  )

(* ### TYPE VERIFICATION ### *)

let is_indexable (s: scope)(t: scopedtype) : bool option =
  resolve_to_reducedtype_opt s t
  |> bind (fun scoped ->
    match scoped.gotype with
    | Array _ -> Some true
    | Slice _ -> Some true
    | _ -> Some false
  )

let is_selectable (s: scope) (t: scopedtype) : bool option =
  resolve_to_reducedtype_opt s t
  |> bind (fun scoped ->
    match scoped.gotype with
    | Struct _ -> Some true
    | _ -> Some false
  )

let are_types_equal (t1: scopedtype) (t2: scopedtype): bool = match t1.gotype with
  (* Compare scope ids only when it's a defined type. *)
  | Defined _ -> t1.gotype = t2.gotype && t1.scopeid = t2.scopeid
  (* Null types can't be compared. *)
  | Null -> false
  (* Do not check scopeid otherwise. This is because, for example, two [3]int array
    are considered the same type even if they were not defined in the same scope. *)
  | _ -> t1.gotype = t2.gotype

(* ### SCOPE MANIPULATION ### *)

(** Add a type declaration to the current scope
    @param typename : the identifier of the type
    @param t : the type associated with the identifier
    @return the new scope or None if the type is already declared
*)
let add_type_declaration_to_scope_opt (s: scope) ((typename: string), (t: scopedtype)): scope option =
  if List.exists (fun (id, _) -> id = typename) s.types then None
  else
    Some {s with types = (typename, t)::s.types}

(** Add a variable binding to the current scope
    @param varname : the identifier of the variable
    @param t : the type associated with the variable
    @return the new scope or None if the variable is already declared
  *)
let add_variable_to_scope_opt (s: scope) ((varname: string), (t: scopedtype)): scope option =
  if List.exists (fun (id, _) -> id = varname) s.bindings then None
  else
    Some {s with bindings = (varname, t)::s.bindings}

(* new_scope *)

(*
##################
### MAXIME END ###
##################
*)

(* converts a regular node to a node with the given type *)
let tnode_of_node (e: exp node) t = { position = e.position; typ = t; value = e.value }

(* given the types of 2 arguments (for a binary operation) and a list of accepted types for the binary operation, return the resulting type *)
let check_ops_opt (s: scope) (a: scopedtype) (b: scopedtype) (l: basetype list) comparable : scopedtype option =
  l
  |> List.map (fun t ->
    (resolve_to_reducedtype_opt s a, resolve_to_reducedtype_opt s b)
    |> bind2 (fun a b ->
      match a, b with
      | { gotype = Basetype x; _}, { gotype = Basetype y; _} ->
        if x = t && y = t then Some t else None
      | _ -> None
    )
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> if comparable then Some (basetype BBool) else Some a
    | None -> print_string "Error: ..."; None
  )

(* given type of an argument (for a unary operation) and a list of accepted types for the operation, return the resulting type *)
let check_op_opt (s:scope) (a:scopedtype) (l: basetype list) : scopedtype option =
  l
  |> List.map (fun t ->
    resolve_to_reducedtype_opt s a
    |> bind (fun a -> match a with
      | { gotype = Basetype x; _} -> if x = t then Some t else None
      | _ -> None
    )
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> Some a
    | None -> print_string "Error: ..."; None
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

let position_to_tnode (e: 'a node) (value: 'a) (typ: scopedtype) =
  { position = e.position; value = value; typ = typ }

let rec find_function_signature_opt (scope: scope) (name: string) =
  match List.find_opt (fun (id, sign) -> id = name) scope.functions with
  | Some (_, t) -> Some t
  | None -> match scope.parent with
    | Some p -> find_function_signature_opt p name
    | None -> None

let check_both_list_same_exps_types (a: scopedtype list) (b: scopedtype list) =
  a
  |> List.map2 (fun x y -> are_types_equal x y) b
  |> List.exists (fun x -> x = false)
  |> (fun x -> not x)


(* given an expression node, typecheck_opt the expression and return an expression tnode (a node record with a field for its type) *)
let rec typecheck_exp_opt scope e =
  match e with
  | Position e ->
    (match e.value with
    | Id s ->
      scopedtype_of_varname_opt scope s
      |> bind (fun scoped ->
        tnode_of_node e scoped |> some
      )
    | Indexing (target, index) ->
      (typecheck_exp_opt scope target, typecheck_exp_opt scope index)
      |> bind2 (fun target index ->
        (is_indexable scope target.typ, resolve_to_reducedtype_opt scope index.typ)
        |> bind2 (fun indexable index_basetype ->
          if indexable && are_types_equal index_basetype (basetype BInt) then
            resolve_inside_type_of_indexable scope target.typ
            |> bind (fun t ->
              position_to_tnode e (Indexing (Typed target, Typed index)) t |> some
            )
          else
            None
        )
      )
    | Selection (target, selection) ->
      typecheck_exp_opt scope target
      |> bind (fun target ->
        selection_type_opt scope target.typ selection
        |> bind (fun selection_type ->
            position_to_tnode e (Selection (Typed target, selection)) selection_type |> some
        )
      )
    | Int i -> tnode_of_node e (basetype BInt) |> some
    | Float f -> tnode_of_node e (basetype BFloat64) |> some
    | RawStr s
    | String s -> tnode_of_node e (basetype BString) |> some
    | Rune r -> tnode_of_node e (basetype BRune) |> some
    | BinaryOp (bin, (a, b)) ->
      typecheck_exp_opt scope a
      |> bind (fun a ->
        typecheck_exp_opt scope b
        |> bind (fun b -> (* TODO resolve the types *)
          let types, comparable =
             match bin with
            | Plus -> [BInt; BFloat64; BString; BRune], false
            | Minus
            | Times
            | Div -> [BInt; BFloat64; BRune], false
            | Equals
            | NotEquals -> [BInt; BFloat64; BString; BBool; BRune], true
            | And
            | Or -> [BBool], true
            | Smaller
            | Greater
            | SmallerEq
            | GreaterEq -> [BInt; BFloat64; BString; BRune], true (* TODO ordered? *)
            | DGreater
            | DSmaller
            | AndHat
            | BAnd
            | BOr
            | Caret
            | Mod -> [BInt], false in
          check_ops_opt scope a.typ b.typ types comparable
          |> bind (fun x ->
            tnode_of_node e x |> some (* TODO: LHS and RHS are not typed *)
          )
        )
      )
    | Unaryexp (un, a) ->
      typecheck_exp_opt scope a
      |> bind (fun a ->
        let types =
          match un with
          | UCaret -> [BInt; BRune]
          | UMinus
          | UPlus -> [BInt; BFloat64; BRune]
          | Not -> [BBool] in
        check_op_opt scope a.typ types
        |> bind (fun x ->
          tnode_of_node e x |> some
        )
      )
    | FuncCall (name, exps) ->
      find_function_signature_opt scope name
      |> bind (fun signature ->
        typecheck_exp_list_opt scope exps
        |> bind (fun typed_exps ->
          let scoped_exps = List.map (fun { typ = x; _ } -> x) typed_exps in
          if check_both_list_same_exps_types scoped_exps signature.arguments then
            match signature.returnt with
            | Void -> tnode_of_node e { gotype = Null; scopeid = scope.scopeid } |> some
            | NonVoid t -> tnode_of_node e t |> some
          else
            None
        )
      )
    | _ -> None)
  | Typed e -> Some e
  | Scoped e -> None

and typecheck_exp_list_opt (scope: scope) (exps: exp gen_node list) =
  let olist =
    exps
    |> List.map (fun x -> typecheck_exp_opt scope x)
    |> List.filter is_some in
  if List.length olist <> List.length exps then None
  else
    olist
    |> List.map Option.get
    |> some

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

(* given the arguments of a function (i.e (name, gotype)), typecheck all the type references *)
let typecheck_args_opt scope args =
  let typed_args =
    args
    |> List.map (fun (_, typ) -> scopedtype_of_gotype scope typ)
    |> List.filter is_some in
  if List.length args <> List.length typed_args then None
  else
    typed_args
    |> List.map Option.get
    |> some

let double_helper (e: simpleStm node) current target isplus =
  typecheck_exp_opt current target
  |> bind (fun typed_exp ->
    resolve_to_reducedtype_opt current typed_exp.typ
    |> bind(fun resolved_type ->
      if not (resolved_type = basetype BInt
        || resolved_type = basetype BFloat64
        || resolved_type = basetype BRune) then None
      else
        { position = e.position;
          scope = current;
          value =
            if isplus then
              DoublePlus (Typed typed_exp)
            else
              DoubleMinus (Typed typed_exp)
        }
        |> some
    )
  )

(* given a simple statement node, typecheck it *)
let rec typecheck_simple_opt current s: simpleStm snode option =
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
          |> List.map (fun x -> typecheck_exp_opt current x)
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
              |> List.map2 (fun exp kind -> are_types_equal kind.typ exp.typ) typed_exps
              |> List.exists (fun x -> x = false) in
            if test then None
            else
              let gen_nodes = List.map (fun x -> Typed x) typed_exps in
              { position = e.position; scope = current; value = Assign (assign_type, (kinds, gen_nodes)) } |> some
      | Some l -> None)
    | Empty -> { position = e.position; scope = current; value = Empty } |> some
    | DoublePlus kind -> double_helper e current kind true
    | DoubleMinus kind -> double_helper e current kind false
    | ShortDeclaration (kinds, exps) ->
      (* Match the each kind with its associated exp *)
      List.map2 (fun k e -> (k, e)) kinds exps
      (* Accumulate the new scope (for new declaration) and the typed exps *)
      |> List.fold_left (fun scope_and_exps (k, e) ->
        scope_and_exps
        |> bind (fun (scope, l) ->
          (* Make sure the expression typechecks *)
          (typecheck_exp_opt scope e)
          |> bind (fun typed_exp ->
            (* This block of code checks if a new variable needs to be added to the current scope. *)
            match k with
            (* Extract the kind expression *)
            | Position { value = kind } -> (match kind with
              (* If it's just an identifier, check if it's defined in the current scope only *)
              | Id s -> if (List.exists (fun (name, _) -> name = s) scope.bindings) then
                  (* If the identifier is defined, nothing to add to the scope *)
                  Some (k, scope)
                else
                  (* Add the variable to the scope if it's not defined *)
                  add_variable_to_scope_opt scope (s, typed_exp.typ)
                  (* Return the new scope with the added binding *)
                  |> bind (fun scope -> Some (k, scope))
              (* If the expression is not a simple identifier, nothing to add to the scope *)
              | _ -> Some (k, scope)
              )
              (* Typecheck the expression with the potentially altered scope *)
              |> bind (fun (k, scope) -> typecheck_exp_opt scope k
                (* If the kind is well typed, check the type equality *)
                |> bind (fun k ->
                  if (are_types_equal k.typ typed_exp.typ) then
                    Some (scope, (Typed typed_exp)::l)
                  else
                    None
                )
              )
            | _ -> failwith "IMPOSSIBLE"
          )
        )
      ) (Some (current, []))
      (* When all the exps have been typed, return the short declaration with the new scope. *)
      |> bind (fun (new_scope, l) ->
        (* If no new bindings were created, it means that no new variable were
          introduced in the short declaration. This is not allowed. *)
        if List.length new_scope.bindings = List.length current.bindings then None
        else
          { position = e.position; scope = new_scope; value = ShortDeclaration (kinds, l) } |> some
      )
    | _ -> None)
  | _ -> None

(* print helper function for typecheck print and println statements *)
let print_helper current (e: stmt node) l is_println =
  let tnodes =
    l
    |> List.map (fun x -> typecheck_exp_opt current x)
    |> List.filter is_some in
  if List.length l <> List.length tnodes then None
  else
    let tnodes = List.map Option.get tnodes in
    let check_types =
      tnodes
      |> List.map (fun x -> match x.typ with | { gotype = Basetype s; _} -> Some s | _ -> None)
      |> List.exists is_none in
    let lst = List.map (fun x -> Typed x) tnodes in
    if check_types = true then None
    else
      Some { position = e.position; scope = current; value = if is_println then Println lst else Print lst }

let verify_return_statements (l: (stmt snode) list) expected_type =
  l
  |> List.map (fun s ->
    match s.value with
    | Return exp ->
      (match exp with
      | None -> expected_type = Void
      | Some exp ->
        (match expected_type with
        | Void -> false
        | NonVoid t ->
          (match exp with
          | Typed e -> e.typ = t
          | _ -> false)))
    | _ -> true
  )
  |> List.exists (fun x -> x = false)

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
    | If (simple, oexp, ifs, oelses) ->
      (* Typechecking the simple statement using a new scope *)
      typecheck_simple_opt (new_scope current) simple
      |> bind (fun typed_simple ->
        (* Keep the scope made for the simple statement *)
        let simple_scope = typed_simple.scope in
        (* Typechecking the if expression using the scope of the simple statement *)
        let typed_exp =
        typecheck_exp_opt simple_scope oexp
        |> bind (fun typed ->
          match typed.typ with
          | { gotype = Basetype s; _} -> if s = BBool then Typed typed |> some else None
          | _ -> None
        ) in
        (* Add the created simplestm's scope to the current scope list of children *)
        let current = { current with children = simple_scope::current.children } in
        let simple_scope = {simple_scope with parent = Some current} in
        typed_exp
        |> bind (fun typed_exp ->
          match oelses with
          | None -> if_helper simple_scope ifs e typed_simple typed_exp None
          | Some _ ->
            oelses
            |> bind (fun elses ->
              let else_scope = new_scope simple_scope in
              typecheck_stm_list_opt elses else_scope
              |> bind (fun (snodes, else_scope) ->
                let typed_elses_gen = snodes |> List.map (fun x -> Scoped x) in
                match (typed_elses_gen |> List.rev |> List.hd) with
                | Scoped typed_elses ->
                  if_helper simple_scope ifs e typed_simple typed_exp (Some typed_elses_gen)
                | _ -> None
              )
            )
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
          scopedtype_of_gotype current x
          |> bind (fun scopedx -> resolve_to_reducedtype_opt current scopedx)
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
          Some { position = e.position; scope = new_scope; value = TypeDeclaration decls }
        )
    | Simple simple ->
      typecheck_simple_opt current simple
      |> bind (fun simple ->
        Some { position = e.position; scope = simple.scope; value = Simple (Scoped simple) }
      )
    | Return exp ->
      (match exp with
      | None -> Some { position = e.position; scope = current; value = Return None }
      | Some exp ->
        typecheck_exp_opt current exp
        |> bind (fun x ->
          Some { position = e.position; scope = current; value = Return (Some (Typed x)) }
        ))
    | Loop loop ->
      let new_scope = new_scope current in
      loop_helper e new_scope loop
      |> bind (fun x ->
        let new_scope = { current with children = List.append current.children [x.scope] } in
        { x with scope = new_scope } |> some
      )
    | _ -> None)

  | Typed e -> None
  | Scoped e -> Some e

and if_helper simple_scope ifs (e: stmt node) typed_simple typed_exp typed_elses =
  let body_scope = new_scope simple_scope in
  typecheck_stm_list_opt ifs body_scope
  |> bind (fun (stms, body_scope) ->
    let body_scope = { body_scope with parent = Some simple_scope } in
    let else_scope =
        typed_elses
        |> bind (fun elses ->
          elses
          |> List.rev
          |> List.hd
          |> (fun x -> match x with Scoped s -> Some s.scope | _ -> None)
      ) in
    let scoped_stms = List.map (fun x -> Scoped x) stms in
    let typed_simple_gen = Scoped typed_simple in
    match typed_elses with
    | None ->
      let updated_scope = { simple_scope with children = [body_scope]} in
      Some { position = e.position; scope = BatOption.get updated_scope.parent; value = If (typed_simple_gen, typed_exp, scoped_stms, typed_elses) }
    | Some _ ->
      else_scope
      |> bind (fun else_scope ->
        let else_scope = { else_scope with parent = Some simple_scope } in
        let updated_scope = { simple_scope with children = [body_scope; else_scope] } in
        Some { position = e.position; scope = BatOption.get updated_scope.parent; value = If (typed_simple_gen, typed_exp, scoped_stms, typed_elses) }
      )
  )

and loop_helper e scope loop =
  match loop with
  | While (oexp, stmts) ->
    let otyped_exp =
      oexp
      |> bind (fun exp ->
        typecheck_exp_opt scope exp
      ) in
    typecheck_stm_list_opt stmts scope
    |> bind (fun (typed_stmts, new_scope) ->
      let gen_nodes = List.map (fun x -> Scoped x) typed_stmts in
      (match oexp with
      | None ->
        Some { position = e.position; scope = new_scope; value = Loop (While (None, gen_nodes)) }
      | _ ->
        otyped_exp
        |> bind (fun typed_exp ->
          if not (match typed_exp.typ with | { gotype = Basetype x } -> x = BBool | _ -> false) then None
          else
            let exp_gen = Typed typed_exp in
            Some { position = e.position; scope = new_scope; value = Loop (While (Some exp_gen, gen_nodes)) }
        )
      )
    )
  | For (init, oexp, inc, stmts) ->
    let otyped_exp =
      oexp
      |> bind (fun exp ->
        typecheck_exp_opt scope exp
      ) in
    typecheck_simple_opt scope init
    |> bind (fun init ->
      let scope = new_scope init.scope in
      typecheck_simple_opt scope inc
      |> bind (fun inc ->
        typecheck_stm_list_opt stmts scope
        |> bind (fun (typed_stmt, new_scope) ->
          let stmt_gen_nodes = List.map (fun x -> Scoped x) typed_stmt in
          let init_gen = Scoped init in
          let inc_gen = Scoped inc in
          (match oexp with
          | None ->
            Some { position = e.position; scope = new_scope; value = Loop (For (init_gen, None, inc_gen, stmt_gen_nodes)) }
          | Some _ ->
            otyped_exp
            |> bind (fun typed_exp ->
              if not (match typed_exp.typ with | { gotype = Basetype x } -> x = BBool | _ -> false) then None
              else
                let exp_gen = Typed typed_exp in
                Some { position = e.position; scope = new_scope; value = Loop (For (init_gen, Some exp_gen, inc_gen, stmt_gen_nodes)) }
            )
          )
        )
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
        scopedtype_of_gotype scope t
        |> bind (fun scopedt ->
          let exps_with_annotation =
            typed_exps
            |> List.filter (fun x -> x.typ = scopedt)
            |> List.map (fun x -> Typed x) in
          if List.length exps_with_annotation <> List.length typed_exps then None
          else
            (* Get back the type that wasn't reduced (needed for the binding). *)
            let new_bindings =
              vars
              |> List.map (fun name -> (name, scopedt)) in
            let new_scope = { empty_scope with bindings = new_bindings } in
            ((vars, Some t, exps), new_scope) |> some
        )

let check_invalid_main (name, args, otyp) =
  if name = "main" then
    List.length args > 0 || otyp <> Void
  else
    false

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
          scopedtype_of_gotype scope x
          |> bind (fun scopedx -> resolve_to_reducedtype_opt scope scopedx)
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
          (new_scope, Type decls) |> some
        )
    | Fct (name, args, return_type, stmts) ->
      let empty_function_scope = new_scope scope in
      if check_invalid_main (name, args, return_type) then None
      else
        args
        |> typecheck_args_opt scope
        |> bind (fun typed_args ->
          (* Add the type we resolved to all the arguments. *)
          let arguments_scope = List.map2 (fun (name, _) typ -> (name, typ)) args typed_args in
          let initial_function_scope = { empty_function_scope with bindings = arguments_scope } in
          typecheck_stm_list_opt stmts initial_function_scope
          |> bind (fun (typed_stmts, new_scope) ->
            (* Add the scope to each statement *)
            let scoped_typed_stmts = List.map (fun x -> Scoped x) typed_stmts in
            (* Typecheck the return type. *)
            (match return_type with
            | Void -> Some Void
            | NonVoid t -> scopedtype_of_gotype scope t |> bind (fun t -> NonVoid t |> some))
            |> bind (fun scoped_return_type ->
              if verify_return_statements typed_stmts scoped_return_type = true then None
              else
                let signature = { arguments = typed_args; returnt = scoped_return_type } in
                let function_binding = (name, signature) in
                let scope =
                  { scope with
                    children = List.append scope.children [new_scope];
                    functions = List.append scope.functions [function_binding]
                  } in
                (scope, Fct (name, args, return_type, scoped_typed_stmts)) |> some
            )
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
    Some ((package, decls), top_level)
  )
