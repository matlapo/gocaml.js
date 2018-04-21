open Ast
open BatOption
open Utils
open Scopeprinter
module Option = BatOption

(* function to display an error, ensures that at most 1 error can be printed *)
let error =
  let already_an_error_printed = ref false in
  (fun x -> if not !already_an_error_printed then (already_an_error_printed := true; Printf.eprintf "%s\n" x) else ())

let wrong_type x expected received =
  Printf.sprintf "Error: expected type %s but received type %s at line: %d" (Scopeprinter.string_of_gotype expected) (Scopeprinter.string_of_gotype received) x
let not_a_base_type x received = Printf.sprintf "Error: %s is not a base type at line: %d" (Scopeprinter.string_of_gotype received) x
let unknown_binding x name = Printf.sprintf "Error: '%s' used before declared at line: %d" name x
let name_redeclared x name = Printf.sprintf "Error: the name '%s' is already declared in the current scope at line: %d" name x
let variables_not_all_same_type x expected = Printf.sprintf "Error: not all variables are of type %s in this declaration at line: %d" (Scopeprinter.string_of_gotype expected) x
let cannot_find_gotype x typ = Printf.sprintf "Error: cannot find type definition for the type %s at line: %d" (Scopeprinter.string_of_gotype typ) x
let cannot_find_gotype_string x typ = Printf.sprintf "Error: cannot find type definition for the type %s at line: %d" typ x
let type_not_indexable x typ = Printf.sprintf "Error: type %s is not indexable at line %d" (Scopeprinter.string_of_gotype typ) x
let not_a_struct x typ = Printf.sprintf "Error: type %s is not a struct at line %d" (Scopeprinter.string_of_gotype typ) x
let invalid_type_cast x = Printf.sprintf "Error: invalid type cast operation at line %d" x
let types_not_comparable x left right = Printf.sprintf "Error: %s and %s are not comparable at line %d" (Scopeprinter.string_of_gotype left) (Scopeprinter.string_of_gotype right) x
let invalid_function_call x name = Printf.sprintf "Error: arguments types do not match the signature of function '%s' at line %d" name x
let too_many_exps_type_cast x = Printf.sprintf "Error: too many arguments for type cast at line %d" x
let invalid_type_for_op x typ = Printf.sprintf "Error: type %s is not a valid type for this operation at line %d" (Scopeprinter.string_of_gotype typ) x
let invalid_assignment_error x = Printf.sprintf "Error: invalid assignment at line %d" x
let invalid_short_declaration x = Printf.sprintf "Error: invalid short declaration, must declare at least one new variable at line %d" x
let invalid_main x = Printf.sprintf "Error: main function must have no argument and void return type at line %d" x
let redeclared_error x name = Printf.sprintf "Error: %s redeclared in this block at line: %d" name x

let base_int = "int"
let base_float = "float64"
let base_string = "string"
let base_bool = "bool"
let base_rune = "rune"

let try_base_type (s: string) =
  if s = base_int then BInt |> some
  else if s = base_float then BFloat64 |> some
  else if s = base_string then BString |> some
  else if s = base_bool then BBool |> some
  else if s = base_rune then BRune |> some
  else None

let is_numeric (t: basetype) =
  match t with
    | BInt
    | BFloat64
    | BRune -> true
    | _ -> false

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

(* Finds the most recent parent scope (including current scope) where a certain variable is defined. *)
let rec find_scope_of_varname_opt (s: scope) (varname: string): scope option =
  if List.exists (fun (name, _) -> name = varname) s.bindings then Some s
  else
    s.parent
    |> bind (fun p -> find_scope_of_varname_opt p varname)

(** Finds the type associated with a variable name *)
let rec scopedtype_of_varname_opt lineno (s: scope) (varname: string): scopedtype option =
  match List.find_opt (fun (id, _) -> id = varname) s.bindings with
  | Some (_, t) -> Some t
  | None -> match s.parent with
    | Some p -> scopedtype_of_varname_opt lineno p varname
    | None -> unknown_binding lineno varname |> error; None

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


(* Find the first scope where the given gotype is defined *)
let rec scopedtype_of_gotype lineno (s: scope) (t: gotype) : scopedtype option =
  match t with
  | Defined typename ->
    (match List.find_opt (fun (id, _) -> id = typename) s.types with
    | Some _ ->
      if s.scopeid <> 0 then
        Some { gotype = Defined typename; scopeid = s.scopeid }
      (* If the Defined goes back to the initial scope, it is a base type.*)
      else
        (basetype_of_string_opt typename) |> bind (fun t -> Some { gotype = t; scopeid = 0 })
    | None ->
      (match s.parent with
      | Some p -> scopedtype_of_gotype lineno p t
      | None -> cannot_find_gotype lineno t |> error; None))
  | _ -> Some { gotype = t; scopeid = s.scopeid }


(* Find the base type inside of an array. (needs to resolve gotype inside arrays scope) *)
let resolve_inside_type_of_indexable lineno (s: scope) (t: scopedtype) : scopedtype option =
  (* Find the original array type *)
  resolve_to_reducedtype_opt s t
  |> bind (fun reduced_type ->
    (* Get the gotype of type inside the array *)
    match reduced_type.gotype with
    | Array (insidetype, _)
    | Slice insidetype -> (
      (* Find the scope where the array type was defined *)
      find_scope_with_scopeid_opt s reduced_type.scopeid
      |> bind (fun indexablescope ->
        (* Find the scopedtype of the type inside the array *)
        scopedtype_of_gotype lineno indexablescope insidetype
      )
    )
    (* If the type is not indexable, error *)
    | _ -> type_not_indexable lineno t.gotype |> error; None
  )


(** Finds the type associated with a string *)
let rec scopedtype_of_typename_opt lineno (s: scope) (typename: string): scopedtype option =
  (* Look for a type in the current scope *)
  match List.find_opt (fun (id, _) -> id = typename) s.types with
  (* If found, create a scopedtype with the current scope id *)
  | Some (id, t) ->
    if t.scopeid = 0 && is_some (try_base_type id) then
      Some { gotype = Basetype (Option.get (try_base_type id)); scopeid = 0 }
    else Some { gotype = Defined id; scopeid = s.scopeid }
  | None -> match s.parent with
    (* Look in parent scope *)
    | Some p -> scopedtype_of_typename_opt lineno p typename
    | None -> None



(* Returns the type of a selection *)
let selection_type_opt lineno (s: scope) (t: scopedtype) (a: string) : scopedtype option =
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
          scopedtype_of_gotype lineno selectionscope member_type
        )
      | _ -> not_a_struct lineno t.gotype |> error; None
    )
  )

(* ### TYPE VERIFICATION ### *)

let is_indexable (s: scope) (t: scopedtype) : bool option =
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

let are_types_equal (t1: scopedtype) (t2: scopedtype): bool =
  match t1.gotype with
  (* Compare scope ids only when it's a defined type. *)
  | Defined _ -> t1.gotype = t2.gotype && t1.scopeid = t2.scopeid
  (* Null types can't be compared. *)
  | Null -> false
  (* Do not check scopeid otherwise. This is because, for example, two [3]int array
    are considered the same type even if they were not defined in the same scope. *)
  | _ ->
    t1.gotype = t2.gotype

(* ### SCOPE MANIPULATION ### *)

(** Add a variable binding to the current scope
    @param varname : the identifier of the variable
    @param t : the type associated with the variable
    @return the new scope or None if the variable is already declared
  *)
let add_variable_to_scope_opt lineno (s: scope) ((varname: string), (t: scopedtype)): scope option =
  if List.exists (fun (id, _) -> id = varname) s.bindings then (name_redeclared lineno varname |> error; None)
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

let tnode_of_node_and_value (e: exp node) t v = { position = e.position; typ = t; value = v }

let snode_of_node (s: stmt node) scope prev = { position = s.position; scope = scope; prevscope = prev; value = s.value }

let snode_of_node_and_value (s: stmt node) scope prev v = { position = s.position; scope = scope; prevscope = prev; value = v }

(* given the types of 2 arguments (for a binary operation) and a list of accepted types for the binary operation, return the resulting type *)
let check_ops_opt lineno (s: scope) (left: scopedtype) (right: scopedtype) (l: basetype list) comparable equality : scopedtype option =
  l
  |> List.map (fun t ->
    (resolve_to_reducedtype_opt s left, resolve_to_reducedtype_opt s right)
    |> bind2 (fun a b ->
      match a, b with
      | { gotype = Basetype x; _}, { gotype = Basetype y; _} ->
        if x = t && y = t then Some left
        else None
      | _ ->
        if equality then
          if are_types_equal left right then Some left else None
        else None
    )
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> if comparable then Some (basetype BBool) else  Some left
    | None -> wrong_type lineno left.gotype right.gotype |> error; None
  )

(* given type of an argument (for a unary operation) and a list of accepted types for the operation, return the resulting type *)
let check_op_opt lineno (s:scope) (a:scopedtype) (l: basetype list) : scopedtype option =
  l
  |> List.map (fun t ->
    resolve_to_reducedtype_opt s a
    |> bind (fun a ->
      match a with
      | { gotype = Basetype x; _} -> if x = t then Some t else None
      | _ -> None
    )
  )
  |> List.find_opt is_some
  |> (fun x ->
    match x with
    | Some x -> Some a
    | None -> invalid_type_for_op lineno a.gotype |> error; None
  )


(* checks if any variable name in a list is already declared in the current scope *)
let check_vars_declared line_no scope names =
    let onames =
      names
      |> List.map (fun name ->
        if List.mem_assoc name scope.bindings = true then (unknown_binding line_no name |> error; None) else Some name
      )
      |> List.filter is_some in
    if List.length onames <> List.length names then true else false

let position_to_tnode (e: 'a node) (value: 'a) (typ: scopedtype) =
  { position = e.position; value = value; typ = typ }

let unknown_function x name = Printf.sprintf "Error: unknown function name %s at line %d" name x

let rec find_function_signature_opt lineno (scope: scope) (name: string) =
  match List.find_opt (fun (id, sign) -> id = name) scope.functions with
  | Some (_, t) -> Some t
  | None -> match scope.parent with
    | Some p -> find_function_signature_opt lineno p name
    | None -> unknown_function lineno name |> error; None

let check_both_list_same_exps_types (a: scopedtype list) (b: scopedtype list) =
  if List.length a <> List.length b then false
  else
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
      scopedtype_of_varname_opt e.position.pos_lnum scope s
      |> bind (fun scoped ->
        tnode_of_node e scoped |> some
      )
    | Indexing (target, index) ->
      (typecheck_exp_opt scope target, typecheck_exp_opt scope index)
      |> bind2 (fun target index ->
        (is_indexable scope target.typ, resolve_to_reducedtype_opt scope index.typ)
        |> bind2 (fun indexable index_basetype ->
          if indexable && are_types_equal index_basetype (basetype BInt) then
            resolve_inside_type_of_indexable e.position.pos_lnum scope target.typ
            |> bind (fun t ->
              position_to_tnode e (Indexing (Typed target, Typed index)) t |> some
            )
          else
            None (*TODO: error message*)
        )
      )
    | Selection (target, selection) ->
      typecheck_exp_opt scope target
      |> bind (fun target ->
        selection_type_opt e.position.pos_lnum scope target.typ selection
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
        |> bind (fun b ->
          let types, comparable, equality =
             match bin with
            | Plus -> [BInt; BFloat64; BString; BRune], false, false
            | Minus
            | Times
            | Div -> [BInt; BFloat64; BRune], false, false
            | Equals
            | NotEquals -> [BInt; BFloat64; BString; BBool; BRune], true, true
            | And
            | Or -> [BBool], false, false
            | Smaller
            | Greater
            | SmallerEq
            | GreaterEq -> [BInt; BFloat64; BString; BRune], true, false
            | DGreater
            | DSmaller
            | AndHat
            | BAnd
            | BOr
            | Caret
            | Mod -> [BInt; BRune], false, false in
          check_ops_opt e.position.pos_lnum scope a.typ b.typ types comparable equality
          |> bind (fun x ->
            tnode_of_node_and_value e x (BinaryOp (bin, (Typed a, Typed b))) |> some
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
        check_op_opt e.position.pos_lnum scope a.typ types
        |> bind (fun x ->
          tnode_of_node_and_value e x (Unaryexp (un, Typed a)) |> some
        )
      )
    | FuncCall (name, exps) ->
      (* first, check if the function call is actually a valid type cast *)
      (match check_if_type_cast e scope name exps with
      | Some t -> Some t
      | None ->
        (* if not, then it must a function call, make sure it type check and return the resulting type for this node *)
        find_function_signature_opt e.position.pos_lnum scope name
        |> bind (fun signature ->
          typecheck_exp_list_opt scope exps
          |> bind (fun typed_exps ->
            let scoped_exps = List.map (fun { typ = x; _ } -> x) typed_exps in
            if check_both_list_same_exps_types scoped_exps signature.arguments then
              let inner_value = FuncCall (name, List.map (fun x -> Typed x) typed_exps) in
              match signature.returnt with
              | Void -> tnode_of_node_and_value e { gotype = Null; scopeid = scope.scopeid } inner_value |> some
              | NonVoid t -> tnode_of_node_and_value e t inner_value |> some
            else
              (invalid_function_call e.position.pos_lnum name |> error; None)
          )
        ))
    | Append (array, element) ->
      (typecheck_exp_opt scope array, typecheck_exp_opt scope element)
      |> bind2 (fun typed_array typed_element ->
        (* Checks if the type is an array/slice and returns the inside type. *)
        resolve_inside_type_of_indexable e.position.pos_lnum scope typed_array.typ
        |> bind (fun type_inside_array ->
          (* Compared the type inside the array with the type of the element. *)
          if are_types_equal type_inside_array typed_element.typ then
            Some (tnode_of_node_and_value e typed_array.typ (Append (Typed typed_array, Typed typed_element)))
          else
            (invalid_function_call e.position.pos_lnum "append" |> error; None)
          )
        )
      )
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

(*
function that checks if a function call is a typecast of the form type(expr). We have to check:
1. expr must be a single expression
2. type is a type that exists (in this scope or previous ones)
3. reduce the type to a base type, if possible
4. typecheck the expr (i.e the argument)
5. reduce the type of expr to a base type, if possible
6. Apply the type rule associated with type casting in G


note: all these operations can fail, hence the serie of binds
*)
and check_if_type_cast (e: exp node) (scope: scope) (name: string) (exps: exp gen_node list) =
  if List.length exps <> 1 then None
  else
    (* we only allow for one expression in the type cast *)
    let exp = List.hd exps in
    (* find the type associated with the name of the function *)
    scopedtype_of_typename_opt e.position.pos_lnum scope name
    |> bind (fun scoped_type ->
      (* resolve it *)
      resolve_to_reducedtype_opt scope scoped_type
      |> bind (fun type_reduced ->
        (* typecheck the expression that is being type casted *)
        typecheck_exp_opt scope exp
        |> bind (fun tnode ->
          (* resolve the type of the expression that typechecked *)
          resolve_to_reducedtype_opt scope tnode.typ
          |> bind (fun exp_reduced ->
            (* only types that resolves to basetypes are allowed *)
            match (type_reduced.gotype, exp_reduced.gotype) with
            | (Basetype a, Basetype b) ->
              (* if both types are the same basetype OR both are numeric OR type(expr) where type resolves to string and expr to int or rune *)
              if a = b || (is_numeric a && is_numeric b) || (a = BString && (b = BInt || b = BRune)) then
                tnode_of_node_and_value e scoped_type (FuncCall (name ,[Typed tnode])) |> some
              else (invalid_type_cast e.position.pos_lnum |> error; None)
            | _ -> not_a_base_type e.position.pos_lnum type_reduced.gotype |> error; None
          )
        )
      )
    )

(* given two scopes, merge them together so that all their bindings are all at the same level. If the resulting bindings contain duplicates, it returns None *)
let merge_scope_opt old_scope new_scope =
  let rec helper old_scope new_bindings =
    match new_bindings with
    | [] -> Some old_scope
    | (name, typ)::xs ->
      if List.mem_assoc name old_scope = true then (name_redeclared 0 name |> error; None)
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
let typecheck_args_opt lineno scope args =
  let typed_args =
    args
    |> List.map (fun (_, typ) -> scopedtype_of_gotype lineno scope typ)
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
        || resolved_type = basetype BRune) then
        (invalid_type_for_op e.position.pos_lnum resolved_type.gotype |> error; None)
      else
        { position = e.position;
          scope = current;
          prevscope = current;
          value =
            if isplus then
              DoublePlus (Typed typed_exp)
            else
              DoubleMinus (Typed typed_exp)
        }
        |> some
    )
  )

let extract_position (node: 'a gen_node) =
  match node with
  | Position t -> t
  | _ -> failwith "cannot extract node"

let is_underscore node =
  match (extract_position node).value with
   | Id name ->
      if name = "_" then true else false
   | _ -> false

let null_scopeid = { gotype = Null; scopeid = 0 }


(* given a simple statement node, typecheck it *)
let rec typecheck_simple_opt current s: simpleStm snode option =
  match s with
  | Position e ->
    (match e.value with
    | Assign (kinds, exps) ->
      (* match each id with its expression and typecheck the expression *)
      let typecheck_rhs =
        exps
        |> List.map2 (fun id exp ->
          (id, typecheck_exp_opt current exp)
        ) kinds
        |> List.filter (fun (_, exp) -> is_some exp) in
      (* make sure that all the expressions on the RHS typecheck *)
      if List.length typecheck_rhs <> List.length exps then None
      else
        (* ids are also expressions, so typecheck them too and unwrap the expressions *)
        let typecheck_lhs =
          typecheck_rhs
          |> List.map (fun (id, oexp) ->
            (* if the id is an underscore, then use the special type Null for its type *)
            if is_underscore id then (tnode_of_node (extract_position id) null_scopeid |> some, Option.get oexp)
            else (typecheck_exp_opt current id, Option.get oexp)
          )
          |> List.filter (fun (oid, _) -> is_some oid) in
        (* make sure each id typchecked *)
        if List.length typecheck_lhs <> List.length typecheck_rhs then None
        else
          (* unwrap the ids *)
          let typed_assignments =
            typecheck_lhs
            |> List.map (fun (id, exp) -> (Option.get id, exp)) in
          (* now the real logic, check if all id * expressions pairs match *)
          let invalid_assignment =
            typed_assignments
            |> List.exists (fun (id, exp) ->
              (* if id's type is Null, it means that it is an underscore, so need for comparaison *)
              match id.typ.gotype with
              | Null -> false
              | _ ->
                (* only those cases are assignable *)
                match id.value with
                | Id _
                | Indexing _
                | Selection _ ->
                  (* otherwise check that either the types are equal OR both types are valid types based on the operation used *)
                  are_types_equal id.typ exp.typ |> not
                | _ -> true
            ) in
          if invalid_assignment then (invalid_assignment_error e.position.pos_lnum |> error; None)
          else
            let kinds = List.map extract_position kinds in
            let exps = List.map extract_position exps in
            let gen_node_kinds =
              List.map2 (fun (id, _) old -> tnode_of_node_and_value old id.typ id.value) typed_assignments kinds
              |> List.map (fun x -> Typed x) in
            let gen_node_exps =
              List.map2 (fun (_, exp) old -> tnode_of_node_and_value old exp.typ exp.value) typed_assignments exps
              |> List.map (fun x -> Typed x) in
            { position = e.position; scope = current; prevscope = current; value = Assign (gen_node_kinds, gen_node_exps) } |> some
    | Empty -> { position = e.position; scope = current; prevscope = current; value = Empty } |> some
    | DoublePlus kind -> double_helper e current kind true
    | DoubleMinus kind -> double_helper e current kind false
    | ExpStatement exp ->
      typecheck_exp_opt current exp
      |> bind (fun x ->
        { position = e.position; scope = current; prevscope = current; value = ExpStatement (Typed x) } |> some
      )
    | ShortDeclaration (kinds, exps) ->
      (* Match the each kind with its associated exp *)
      List.map2 (fun k n -> (k, n)) kinds exps
      (* Accumulate the new scope (for new declaration) and the typed exps *)
      |> List.fold_left (fun scope_and_exps (k, n) ->
        scope_and_exps
        |> bind (fun (scope, l) ->
          (* Make sure the expression typechecks *)
          (typecheck_exp_opt scope n)
          |> bind (fun typed_exp ->
            (* This block of code checks if a new variable needs to be added to the current scope. *)
            match k with
            (* Extract the kind expression *)
            | Position { value = kind } -> (match kind with
              (* If it's just an identifier, check if it's defined in the current scope only *)
              | Id s -> if (List.exists (fun (name, _) -> name = s) scope.bindings) || s = "_" then
                  (* If the identifier is defined, nothing to add to the scope *)
                  Some (k, scope)
                else
                  (* Add the variable to the scope if it's not defined *)
                  add_variable_to_scope_opt e.position.pos_lnum scope (s, typed_exp.typ)
                  (* Return the new scope with the added binding *)
                  |> bind (fun scope -> Some (k, scope))
              (* If the expression is not a simple identifier, nothing to add to the scope *)
              | _ -> Some (k, scope)
              )
              (* Typecheck the expression with the potentially altered scope *)
              |> bind (fun (k, scope) ->
                match (extract_position k) with
                | {value = Id "_"} -> Some (scope, List.append l [(Typed typed_exp)])
                | _ -> typecheck_exp_opt scope k
                (* If the kind is well typed, check the type equality *)
                |> bind (fun k ->
                  if (are_types_equal k.typ typed_exp.typ) then
                    Some (scope, List.append l [(Typed typed_exp)])
                  else
                    (wrong_type e.position.pos_lnum k.typ.gotype typed_exp.typ.gotype |> error; None)
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
        if List.length new_scope.bindings = List.length current.bindings then
          (invalid_short_declaration e.position.pos_lnum |> error; None)
        else
          { position = e.position; scope = new_scope; prevscope = current; value = ShortDeclaration (kinds, l) } |> some
      )
    )
  | _ -> None

(* print helper function for typecheck print and println statements *)
let print_helper current (e: stmt node) l is_println =
  let tnodes =
    l
    |> List.map (fun x -> typecheck_exp_opt current x)
    |> List.filter is_some in
  if List.length tnodes <> List.length l then None
  else
    let basetypes_only =
      tnodes
      |> List.map (fun x ->
        let typ = Option.get x in
        resolve_to_reducedtype_opt current typ.typ
      )
      |> List.filter is_some
      |> List.filter (fun x -> match (Option.get x).gotype with | Basetype _ -> true | _ -> not_a_base_type e.position.pos_lnum (Option.get x).gotype |> error; false) in
    let not_all_base_types = List.length l <> List.length basetypes_only in
    if not_all_base_types = true then None
    else
      let tnodes = List.map Option.get tnodes in
      let lst = List.map (fun x -> Typed x) tnodes in
      Some { position = e.position; scope = current; prevscope = current; value = if is_println then Println lst else Print lst }


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
          | Typed e -> if are_types_equal e.typ t then true else (wrong_type s.position.pos_lnum t.gotype e.typ.gotype |> error; false)
          | _ -> false)))
    | _ -> true
  )
  |> List.exists (fun x -> x = false)

let rec typecheck_case_list_opt lineno (scope: scope) (cases: case list) (match_type: scopedtype): (case list) option =
  (* Verify each case individualy *)
  cases
  |> List.map (fun (exps, stmts) ->
    let case_scope = new_scope scope in
    (* Typecheck the statements *)
    (* TODO: case_scope is not passed back to the parent scope as a children *)
    typecheck_stm_list_opt stmts case_scope
    |> bind (fun (typed_stmts, accumulated_scope) ->
      let typed_stmts_gen = List.map (fun s -> Scoped s) typed_stmts in
      match exps with
      (* If it is a default case *)
      | None -> Some (None, typed_stmts_gen)
      (* If it is a normal case *)
      | Some exps ->
        (* Typecheck the case expressions *)
        typecheck_exp_list_opt scope exps
        |> bind (fun typed_exps ->
          let typed_exps_gen = List.map (fun e -> Typed e) typed_exps in
          (* Check if all expressions have the same type as the switch matching type. *)
          if not (List.for_all (fun e -> if are_types_equal e.typ match_type then true else (wrong_type lineno match_type.gotype e.typ.gotype |> error; false)) typed_exps) then None
          else Some (Some typed_exps_gen, typed_stmts_gen)
        )
    )
  )
  |> (fun typed_cases ->
    (* Check if no case failed typechecking *)
    if List.exists is_none typed_cases then None
    else Some (List.map BatOption.get typed_cases)
    )

(* given a statement node, typecheck it *)
and typecheck_stm_opt current s =
  match s with
  | Position e ->
    (match e.value with
    | Block l ->
      let new_scope = new_scope current in
      typecheck_stm_list_opt l new_scope
      |> bind (fun (stms, new_scope) ->
        let new_scope = { current with children = [new_scope] } in
        Some { position = e.position; scope = new_scope;  prevscope = current; value = Block (List.map (fun x -> Scoped x) stms) }
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
          resolve_to_reducedtype_opt simple_scope typed.typ
          |> bind (fun reduced_type ->
            match reduced_type with
            | { gotype = Basetype s; _} ->
              if s = BBool then Typed typed |> some
              else (wrong_type typed.position.pos_lnum (Basetype BBool) typed.typ.gotype |> error; None)
            | _ -> wrong_type typed.position.pos_lnum (Basetype BBool) typed.typ.gotype |> error; None
          )
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
      typecheck_decl_list_opt l (typecheck_var_decl_opt e.position.pos_lnum) current
      |> bind (fun (scope, ol) ->
          Some { position = e.position; scope = scope; prevscope = current; value = Declaration ol }
      )
    | TypeDeclaration decls ->
      let typed_decls =
        decls
        |> List.map (fun (name, x) ->
          scopedtype_of_gotype e.position.pos_lnum current x
          |> bind (fun scopedx -> resolve_to_reducedtype_opt current scopedx)
          |> bind (fun typ -> Some (name, typ)))
        |> List.filter is_some in
      if List.length typed_decls <> List.length decls then None
      else
        let new_types =
          typed_decls
          |> List.map Option.get
          |> List.filter (fun (name, _) -> name <> "_") in
        { empty_scope with types = new_types }
        |> merge_scope_opt current
        |> bind (fun new_scope ->
          Some { position = e.position; scope = new_scope; prevscope = current; value = TypeDeclaration decls }
        )
    | Simple simple ->
      typecheck_simple_opt current simple
      |> bind (fun simple ->
        Some { position = e.position; scope = simple.scope; prevscope = current; value = Simple (Scoped simple) }
      )
    | Return exp ->
      (match exp with
      | None -> Some { position = e.position; scope = current; prevscope = current; value = Return None }
      | Some exp ->
        typecheck_exp_opt current exp
        |> bind (fun x ->
          Some { position = e.position; scope = current; prevscope = current; value = Return (Some (Typed x)) }
        ))
    | Loop loop ->
      let new_scope = new_scope current in
      loop_helper e new_scope loop
      |> bind (fun x ->
        let new_scope = { current with children = List.append current.children [x.scope] } in
        { x with scope = new_scope } |> some
      )
    | Switch (simple, exp, cases) ->
      let simple_scope = new_scope current in
      (* Typecheck the simple statement *)
      typecheck_simple_opt simple_scope simple
      |> bind (fun typed_simple ->
        (* Get the scope after the simple statement executed *)
        let simple_scope = typed_simple.scope in
        (match exp with
        (* If there is an exp, cases expression must match the switch expression. *)
        | Some exp -> typecheck_exp_opt simple_scope exp
          |> bind (fun typed_exp -> typecheck_case_list_opt e.position.pos_lnum simple_scope cases typed_exp.typ)
        (* If there is no exp, cases expression must be of type bool. *)
        | None -> typecheck_case_list_opt e.position.pos_lnum simple_scope cases (basetype BBool))
        |> bind (fun typed_cases ->
          (* Add the scope of the switch to the current scope *)
          let current = { current with children = simple_scope::current.children } in
          let s = extract_position s in
          (* TODO: Here I am passing the untyped exp. *)
          snode_of_node_and_value s current current (Switch (Scoped typed_simple, exp, typed_cases)) |> some
          )
      )
    | Break
    | Continue -> snode_of_node e current current |> some
    )

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
      Some { position = e.position; scope = BatOption.get updated_scope.parent; prevscope = simple_scope; value = If (typed_simple_gen, typed_exp, scoped_stms, typed_elses) }
    | Some _ ->
      else_scope
      |> bind (fun else_scope ->
        let else_scope = { else_scope with parent = Some simple_scope } in
        let updated_scope = { simple_scope with children = [body_scope; else_scope] } in
        Some { position = e.position; scope = BatOption.get updated_scope.parent; prevscope = simple_scope; value = If (typed_simple_gen, typed_exp, scoped_stms, typed_elses) }
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
        Some { position = e.position; scope = new_scope; prevscope = scope; value = Loop (While (None, gen_nodes)) }
      | _ ->
        otyped_exp
        |> bind (fun typed_exp ->
          resolve_to_reducedtype_opt scope typed_exp.typ
          |> bind (fun reduced_type ->
            if not (match reduced_type with | { gotype = Basetype x } -> x = BBool | _ -> false) then
              (wrong_type e.position.pos_lnum (Basetype BBool) reduced_type.gotype |> error; None)
            else
              let exp_gen = Typed typed_exp in
              Some { position = e.position; scope = new_scope; prevscope = scope; value = Loop (While (Some exp_gen, gen_nodes)) }
          )
        )
      )
    )
  | For (init, oexp, inc, stmts) ->
    typecheck_simple_opt scope init
    |> bind (fun init ->
      let short_scope = init.scope in
      let otyped_exp =
        oexp
        |> bind (fun exp ->
          typecheck_exp_opt short_scope exp
        ) in
      typecheck_simple_opt short_scope inc
      |> bind (fun inc ->
        let body_scope = new_scope short_scope in
        typecheck_stm_list_opt stmts body_scope
        |> bind (fun (typed_stmt, new_scope) ->
          let stmt_gen_nodes = List.map (fun x -> Scoped x) typed_stmt in
          let init_gen = Scoped init in
          let inc_gen = Scoped inc in
          (match oexp with
          | None ->
            Some { position = e.position; scope = new_scope; prevscope = scope; value = Loop (For (init_gen, None, inc_gen, stmt_gen_nodes)) }
          | Some _ ->
            otyped_exp
            |> bind (fun typed_exp ->
              resolve_to_reducedtype_opt scope typed_exp.typ
              |> bind (fun reduced_type ->
                if not (match reduced_type with | { gotype = Basetype x } -> x = BBool | _ -> false) then
                  (wrong_type e.position.pos_lnum (Basetype BBool) reduced_type.gotype |> error; None)
                else
                  let exp_gen = Typed typed_exp in
                  Some { position = e.position; scope = new_scope; prevscope = scope; value = Loop (For (init_gen, Some exp_gen, inc_gen, stmt_gen_nodes)) }
              )
            )
          )
        )
      )
    )

(* given a list of statement nodes, typecheck_opt them *)
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
and typecheck_var_decl_opt lineno scope (vars, t, exps) =
  let otyped_exps =
    exps
    |> List.map (fun exp -> typecheck_exp_opt scope exp)
    |> List.filter is_some in
  if List.length otyped_exps <> List.length exps then None
  else
    let vars_without_underscore = List.filter (fun x -> x <> "_") vars in
    if check_vars_declared lineno scope vars_without_underscore then None
    else if contains_duplicate vars_without_underscore (fun name -> name_redeclared lineno name |> error) then None
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
          |> List.map2 (fun name node -> if name <> "_" then (name, node.typ) |> some else None) vars in
        let new_bindings_wihout_underscores =
          new_bindings
          |> List.filter is_some
          |> List.map Option.get in
        let new_scope = { empty_scope with bindings = new_bindings_wihout_underscores } in
        ((vars, t, exps), new_scope) |> some
      | Some t ->
        let exps =
          typed_exps
          |> List.map (fun x -> Typed x) in
        scopedtype_of_gotype lineno scope t
        |> bind (fun scopedt ->
          let exps_with_annotation =
            typed_exps
            |> List.filter (fun x -> x.typ = scopedt)
            |> List.map (fun x -> Typed x) in
          if List.length exps_with_annotation <> List.length typed_exps then (variables_not_all_same_type lineno scopedt.gotype |> error; None)
          else
            (* Get back the type that wasn't reduced (needed for the binding). *)
            let new_bindings =
              vars
              |> List.map (fun name -> if name <> "_" then (name, scopedt) |> some else None) in
            let new_bindings_wihout_underscores =
              new_bindings
              |> List.filter is_some
              |> List.map Option.get in
            let new_scope = { empty_scope with bindings = new_bindings_wihout_underscores } in
            ((vars, Some t, exps), new_scope) |> some
        )

let check_invalid_main (name, args, otyp) =
  if name = "main" then
    List.length args > 0 || otyp <> Void
  else
    false

let check_if_already_declared scope name =
  let f (x, _) = x = name in
  let bindings = List.exists f scope.bindings in
  let functions = List.exists f scope.functions in
  let types = List.exists f scope.types in
  (bindings || functions || types)


(*
 Depending on the type of declaration (i.e Var, Type or Fct) this function will
 add the new bindings to the given scope and return the updated scope along with
 the same declaration but in a different type of node.
*)
let typecheck_decl_opt scope decl =
  match decl with
  | Position n ->
    (match n.value with
    | Var l ->
      typecheck_decl_list_opt l (typecheck_var_decl_opt n.position.pos_lnum) scope
      |> bind (fun (scope, ol) ->
          (scope, Var ol) |> some
      )
    | Type decls ->
      let typed_decls =
        decls
        |> List.map (fun (name, x) ->
          scopedtype_of_gotype n.position.pos_lnum scope x
          |> bind (fun scopedx -> resolve_to_reducedtype_opt scope scopedx)
          |> bind (fun typ -> Some (name, typ)))
        |> List.filter is_some in
      if List.length typed_decls <> List.length decls then None
      else
        let redeclared =
          decls
          |> List.map (fun (x, _) -> x)
          |> List.find_opt (fun name -> check_if_already_declared scope name) in
        if is_some redeclared then (redeclared_error n.position.pos_lnum (Option.get redeclared) |> error; None)
        else
          let new_types =
            typed_decls
            |> List.map Option.get
            |> List.filter (fun (name, _) -> name <> "_") in
          { empty_scope with types = new_types }
          |> merge_scope_opt scope
          |> bind (fun new_scope ->
            (new_scope, Type decls) |> some
          )
    | Fct (name, args, return_type, stmts) ->
      let empty_function_scope = new_scope scope in
      if check_invalid_main (name, args, return_type) then (invalid_main n.position.pos_lnum |> error; None)
      else
        args
        |> typecheck_args_opt n.position.pos_lnum scope
        |> bind (fun typed_args ->
          (* Add the type we resolved to all the arguments. *)
          let arguments_scope =
            List.map2 (fun (name, _) typ -> (name, typ)) args typed_args
            |> List.filter (fun (name, _) -> name <> "_") in
          (match return_type with
          | Void -> Some Void
          | NonVoid t -> scopedtype_of_gotype n.position.pos_lnum scope t |> bind (fun t -> NonVoid t |> some))
          |> bind (fun scoped_return_type ->
            let signature = { arguments = typed_args; returnt = scoped_return_type } in
            let function_binding = (name, signature) in
            let initial_function_scope = { empty_function_scope with bindings = arguments_scope; functions = [function_binding] } in
            typecheck_stm_list_opt stmts initial_function_scope
            |> bind (fun (typed_stmts, new_scope) ->
              (* Add the scope to each statement *)
              let scoped_typed_stmts = List.map (fun x -> Scoped x) typed_stmts in
              (* Typecheck the return type. *)
              if verify_return_statements typed_stmts scoped_return_type = true
              then None
              else
                if check_if_already_declared scope name then (redeclared_error n.position.pos_lnum name |> error; None)
                else
                  let scope =
                    { scope with
                      children = List.append scope.children [new_scope];
                      functions = List.append scope.functions (if name <> "_" then [function_binding] else [])
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
