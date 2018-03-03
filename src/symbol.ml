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
*)

(* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

(* a = (name, type) list list b = decl *)

module Option = BatOption
let bind x f = Option.bind f x
let id x = x

let table_var x = []
let table_type x = []
let table_func x = []

let build_table (prog: program) =
  let _, decl = prog in
  decl
  |> List.fold_left (fun table d ->
    (match d.value with
    | Var x -> table_var x
    | Type x -> table_type x
    | Fct x -> table_func x)::table
  ) []
