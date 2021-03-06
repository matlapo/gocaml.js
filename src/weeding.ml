open Ast
open Utils
open Batteries
open Lexing

let bind x f = Option.bind f x
let id x = x

(*
used everywhere in this file, applies a function
to a list and flattens the result.
*)
let map_flat f l =
  l
  |> List.map f
  |> List.flatten

(*
also used everywhere, applies a function to a list
and if value is Some v then evaluates to v else evals to []
*)
let map_default f o =
  o
  |> Option.map f
  |> Option.default []

let contains_duplicate_struct l =
  l
  |> List.map (fun x ->
    l
    |> List.find_all (fun name -> if x <> "_" then x = name else false)
  )
  |> List.exists (fun x -> List.length x > 1)

let contains_duplicate l =
  l
  |> List.map (fun x ->
    l
    |> List.find_all (fun name -> x = name)
  )
  |> List.exists (fun x -> List.length x > 1)

(*
########################### WEEDING ###########################
Most functions take an AST node and return a string list
which represents a list of errors found. The top level function
found at the bottom of this file will collect all these
strings and send to stderr the first one in the list
##############################################################
*)

(* Helpers for finding invalid blank (i.e '_') usage *)
let blank_s = "_"
let blank_error x = Printf.sprintf "Error: _ not allowed in this context: line %d" x
let helper n s = if s = blank_s then [ blank_error n ] else []

(* Helpers for finding missing default cases in switch statements *)
let default_many_error x = Printf.sprintf "Error: multiple defaults in switch statement: line %d" x

(* Helpers for finding invalid use of continue/break *)
let continue_error x = Printf.sprintf "Error: invalid usage of keyword 'continue': line %d" x
let break_error x = Printf.sprintf "Error: invalid usage of keyword 'break': line %d" x

(* Helpers for detecting if LHS != RHS for variable declaration *)
let variable_decl_error x = Printf.sprintf "Error: number of variables does not match number of expressions: line %d" x

(* Helpers for function call expression *)
let function_call_error x = Printf.sprintf "Error: only function call are allowed as expression statement: line %d" x

(* Helpers post loop *)
let loop_error x = Printf.sprintf "Error: cannot declare in post statement of for loop: line %d" x

let duplicate_args x = Printf.sprintf "Error: duplicate argument name for this function: line %d" x

let duplicate_member x = Printf.sprintf "Error: duplicate member name for this struct: line %d" x

let type_redeclaration x = Printf.sprintf "Error: cannot redeclare this type: line %d" x

let missing_return_statement x = Printf.sprintf "Error: not all paths in the function return a value: line %d" x

(* finds an invalid blank id in a type definition *)
let rec blank_type line t =
  match t with
  | Defined s -> helper line s
  | Array (t, _) -> blank_type line t
  | Slice t -> blank_type line t
  | Struct l ->
    l
    |> List.map (fun (s, t) -> blank_type line t)
    |> List.flatten
  | Basetype _ -> []
  | Null -> []

let duplicate_member_struct line t =
  match t with
  | Struct l ->
    let all_strings =
      l
      |> List.map (fun (s, _) -> s) in
    if contains_duplicate_struct all_strings then [duplicate_member line] else []
  | _ -> []

let type_redeclaration_check line t name =
  match t with
  | Defined s -> if s = name then [type_redeclaration line] else []
  | _ -> []

(* finds an invalid blank id in an expression node *)
let rec blank_exp (e: exp gen_node) =
    match e with
    | Position e ->
      (match e.value with
      | Id s -> if s = blank_s then [blank_error e.position.pos_lnum] else []
      | BinaryOp (_, (a, b)) ->
        blank_exp a
        |> List.append (blank_exp b)
      | Unaryexp (_, a) -> blank_exp a
      | FuncCall (n, l) ->
        l
        |> map_flat blank_exp
        |> List.append (helper e.position.pos_lnum n)
      | Append (a, b) ->
        blank_exp a
        |> List.append (blank_exp b)
      | Indexing (a,b) -> List.append (blank_exp a) (blank_exp b)
      | Selection (a, s) -> List.append (blank_exp a) (if s = blank_s then [blank_error e.position.pos_lnum] else [])
      | _ -> [])
    | _ -> []

(* finds an invalid blank id in a simple statement node *)
let blank_simple (simp: simpleStm gen_node) =
  match simp with
  | Position simp ->
    (match simp.value with
    | Assign (l, e) ->
      map_flat blank_exp e
    | ExpStatement e -> blank_exp e
    | ShortDeclaration (l, e) ->
      map_flat blank_exp e
      (* |> List.append (List.map blank_exp l |> List.flatten) *)
    | DoubleMinus s -> blank_exp s
    | DoublePlus s -> blank_exp s
    | Empty -> [])
  | _ -> []

(* finds an invalid blank id in a statement node *)
let rec blank_stm (stm: stmt gen_node) =
  match stm with
  | Position stm ->
    (match stm.value with
    | Block l -> map_flat blank_stm l
    | Print l -> map_flat blank_exp l
    | Println l -> map_flat blank_exp l
    | Declaration l ->
      l
      |> map_flat (fun (s, d, es) ->
        let d = map_default (blank_type stm.position.pos_lnum) d in
        map_flat blank_exp es
        |> List.append d
      )
    | TypeDeclaration l ->
      l
      |> map_flat (fun (s, ts) ->
        blank_type stm.position.pos_lnum ts
        |> List.append (duplicate_member_struct stm.position.pos_lnum ts)
        |> List.append (type_redeclaration_check stm.position.pos_lnum ts s)
      )
    | If (s, e, l, el) ->
      let s = blank_simple s in
      let e = map_default blank_exp (Some e) in
      let l = map_flat blank_stm l in
      let el =
        el
        |> map_default (fun x -> List.map blank_stm x)
        |> List.flatten in
      s
      |> List.append e
      |> List.append l
      |> List.append el
    | Loop l ->
      (match l with
      | While (e, l) -> map_default blank_exp e
      | For (s1, e, s2, l) ->
        let a =
          blank_simple s1
          |> List.append (blank_simple s2) in
        let b = map_default blank_exp e in
        let c = map_flat blank_stm l in
        a
        |> List.append b
        |> List.append c)
    | Simple s -> blank_simple s
    | Return e -> map_default blank_exp e
    | Switch (s, e, cs) ->
      let s = blank_simple s in
      let e = map_default blank_exp e in
      cs
      |> map_flat (fun x ->
        let (exps, stms) = x in
        let exps = map_default (map_flat blank_exp) exps in
        stms
        |> map_flat blank_stm
        |> List.append exps
      )
      |> List.append e
      |> List.append s
    | _ -> [])
  | _ -> []

let is_empty n =
  match n with
  | Position n ->
    (match n.value with
    | Empty -> true
    | _ -> false)
  | _ -> false

let rec check_return_inside_function (l: stmt gen_node list) seen_return =
  match l with
  | [] -> seen_return
  | s::ss ->
    match s with
    | Position s ->
      (match s.value with
      | Return _ -> true
      | Block block ->
        check_return_inside_function block seen_return
        || check_return_inside_function ss seen_return
      | If (_, _, ifs, oelses) ->
        let if_body = check_return_inside_function ifs seen_return in
        let rest = check_return_inside_function ss seen_return in
        (match oelses with
        | Some elses ->
          (* if there are no return statements found after the if-else block,
          then each branch most contain a return statement *)
          if not rest then
            if_body
            && check_return_inside_function elses false
          else
            true (* if there are return statements after the if-block, then this block doesn't matter for us *)
        | None -> if not rest then if_body else true)
      | Loop loop ->
        let special_case =
          match loop with
          | While (e, _) -> Option.is_none e
          | For (a, b, c, _) ->
            is_empty a && Option.is_none b && is_empty c in
        if special_case then true
        else
          let body =
            match loop with
            | While (e, x) -> x
            | For (_, _, _, x) -> x in
          check_return_inside_function body seen_return
          || check_return_inside_function ss seen_return
      | Switch (_, _, cases) ->
        (* exactly the same logic as the if-blocks *)
        let rest = check_return_inside_function ss seen_return in
        if not rest then
          (* all cases must have a return statement, which corresponds to a big logic AND *)
          cases
          |> List.fold_left (fun acc (_, stmts) -> acc && check_return_inside_function stmts false) true
        else
          true
      | _ -> check_return_inside_function ss seen_return)
    | _ -> false

(* makes sure that a switch statement has at most one default case *)
(* TODO not recursive *)
let check_default (s: stmt gen_node) =
  match s with
  | Position s ->
    (match s.value with
    | Switch (_, _, cs) ->
      let d =
        cs
        |> List.fold_left (fun acc (e, _) -> if Option.is_none e then acc + 1 else acc) 0 in
      if d > 1 then [default_many_error s.position.pos_lnum] else []
    | _ -> [])
  | _ -> []

(*
visits all the statement nodes and check if it contains a continue/break statement.
It also keeps track of if it encountered a loop-node or switch-node because continue/break
statements must be direct or indirect children of those 2 types of node
*)
let check_cont_break (s: stmt gen_node) =
  let rec helper (s: stmt gen_node) (seenLoop: bool) (seenSwitch: bool) =
    match s with
    | Position s ->
      (match s.value with
      | Continue -> if seenLoop = true then [] else [continue_error s.position.pos_lnum]
      | Break ->
        if seenLoop = true || seenSwitch = true then []
        else [break_error s.position.pos_lnum]
      | Loop loop ->
        (match loop with
        | While (_, s) -> s
        | For (_, _, _, s) -> s)
        |> map_flat (fun x -> helper x true true)
      | Block s -> map_flat (fun x -> helper x seenLoop seenSwitch) s
      | If (_, _, s, e) ->
        let e =
          e
          |> map_default (fun x ->
            map_flat (fun x -> helper x seenLoop seenSwitch) x
          ) in
        s
        |> map_flat (fun x -> helper x seenLoop seenSwitch)
        |> List.append e
      | Switch (_, _, cs) ->
        cs
        |> map_flat (fun (e, s) ->
          map_flat (fun x -> helper x seenLoop true) s
        )
      | _ -> [])
    | _ -> []
  in helper s false false

let decl_var_check line s e =
  if List.length e = 0 then []
  else if List.length s = List.length e then []
  else [variable_decl_error line]

let rec assign_check (s: stmt gen_node) =
  match s with
  | Position s ->
    (match s.value with
    | Simple simp ->
      (match simp with
      | Position simp ->
        (match simp.value with
        | Assign (a, b) ->
          if List.length a = List.length b then [] else [variable_decl_error s.position.pos_lnum]
        | ShortDeclaration (a, b) ->
          if List.length a = List.length b then [] else [variable_decl_error s.position.pos_lnum]
        | _ -> [])
      | _ -> [])
    | Block l -> map_flat assign_check l
    | If (_, _, s, e) ->
      let e = map_default (map_flat assign_check) e in
      s
      |> map_flat assign_check
      |> List.append e
    | Loop loop ->
      (match loop with
      | While (_, s) -> s
      | For (_, _, _, s) -> s)
      |> map_flat assign_check
    | Switch (_, _, cs) ->
      cs
      |> map_flat (fun (_, s) -> (map_flat assign_check s))
    | Declaration l ->
        l
        |> map_flat (fun (sl, o, e) ->
          decl_var_check s.position.pos_lnum sl e)
    | _ -> [])
  | _ -> []

let rec check_fcn_call (s: stmt gen_node) =
  match s with
  | Position s ->
    (match s.value with
    | Simple simp ->
      (match simp with
      | Position simp ->
        (match simp.value with
        | ExpStatement e ->
          (match e with
          | Position e ->
            (match e.value with
            | FuncCall _ -> []
            | _ -> [function_call_error e.position.pos_lnum])
          | _ -> [])
        | _ -> [])
      | _ -> [])
    | Block l -> map_flat check_fcn_call l
    | If (_, _, s, e) ->
      let e = map_default (map_flat check_fcn_call) e in
      s
      |> map_flat check_fcn_call
      |> List.append e
    | Loop loop ->
      (match loop with
      | While (_, s) -> s
      | For (_, _, _, s) -> s)
      |> map_flat check_fcn_call
    | Switch (_, _, cs) ->
      cs
      |> map_flat (fun (_, s) -> (map_flat check_fcn_call s))
    | _ -> [])
  | _ -> []

let rec check_post_loop (s: stmt gen_node) =
  match s with
  | Position s ->
    (match s.value with
    | Block l -> map_flat check_post_loop l
    | If (_, _, s, e) ->
      let e = map_default (map_flat check_post_loop) e in
      s
      |> map_flat check_post_loop
      |> List.append e
    | Loop loop ->
      (match loop with
      | While (_, s) -> map_flat check_post_loop s
      | For (_, _, p, s) ->
        let p =
          match p with
          | Position p ->
            (match p.value with
            | ShortDeclaration _ -> [loop_error p.position.pos_lnum]
            | _ -> [])
          | _ -> [] in
        s
        |> map_flat check_post_loop
        |> List.append p
      )
    | Switch (_, _, cs) ->
      cs
      |> map_flat (fun (_, s) -> (map_flat check_post_loop s))
    | _ -> [])
  | _ -> []

(*
this is the parent weeding function, it uses all the function defined
above to collect their error messages (if any!) and output the first one
in the resulting merged list
*)
let weed (p, d) =
  d
  |> map_flat (fun x ->
    match x with
    | Position x ->
      (match x.value with
      | Var l ->
        l
        |> map_flat (fun (v, r, exps) ->
          let exp = map_flat blank_exp exps in
          r
          |> map_default (blank_type x.position.pos_lnum)
          |> List.append exp
          |> List.append (decl_var_check x.position.pos_lnum v exps)
        )
      | Fct (name, args, ret, stmts) ->
        (* TODO make better filter structure *)
        let duplicate_args = if contains_duplicate args then [duplicate_args x.position.pos_lnum] else [] in
        let continue = map_flat check_cont_break stmts in
        let default = map_flat check_default stmts in
        let assign = map_flat assign_check stmts in
        let fct = map_flat check_fcn_call stmts in
        let post = map_flat check_post_loop stmts in
        let args =
          args
          |> map_flat (fun (_, r) ->
             blank_type x.position.pos_lnum r
          ) in
        let s = map_flat blank_stm stmts in
        let all_branch_return =
          match ret with
          | Void -> []
          | NonVoid _ ->
            if check_return_inside_function stmts false then [] else [ missing_return_statement x.position.pos_lnum ] in
        duplicate_args
        |> List.append s
        |> List.append args
        |> List.append continue
        |> List.append default
        |> List.append assign
        |> List.append fct
        |> List.append post
        |> List.append all_branch_return
    | Type l ->
      l
      |> map_flat (fun (s, ts) ->
        blank_type x.position.pos_lnum ts
        |> List.append (duplicate_member_struct x.position.pos_lnum ts)
        |> List.append (type_redeclaration_check x.position.pos_lnum ts s)))
    | _ -> []
  )
