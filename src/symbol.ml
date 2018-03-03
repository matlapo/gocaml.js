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

(* a = (name, type) list list b = decl *)

module Option = BatOption
let bind x f = Option.bind f x
let id x = x

(* let def_to_ref x name =
  match x with
  | StructT s -> TypeR s
  | S *)

let table_var x =
  let helper (names, otype, _) =
    names
    |> List.map (fun x -> (x, otype)) in
  x
  |> List.map helper
  |> List.flatten

(* do we need this? *)
let table_func (name, args, ret, body) =
  let rec helper_list l =
    l
    |> List.map helper
    |> List.flatten
  and helper s =
    match s.value with
    | Block b -> helper_list b
    | Declaration x -> table_var x
    | If (_, _, s, e) ->
      e
      |> Option.map helper_list
      |> Option.default []
      |> List.append (helper_list s)
    | Loop l ->
      (match l with
      | While (_, l) -> helper_list l
      | For (_, _, _, l) -> helper_list l)
    | Switch (_, _, l) ->
      l
      |> List.map (fun (_, x) -> helper_list x)
      |> List.flatten
    | _ -> []
  in
  (name, ret)::(List.append args (List.map helper body |> List.flatten))

let build_table (prog: program) =
  let _, decl = prog in
  decl
  |> List.fold_left (fun table d ->
    (match d.value with
    | Var x -> table_var x
    | Type x -> []
    | Fct x -> table_func x)::table
  ) []
