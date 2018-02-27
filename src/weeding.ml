open Astwithposition
open Utils
open BatOption

module Option = BatOption
let bind x f = Option.bind f x
let id x = x

let blank_s = "_"
let blank_error x = Printf.sprintf "Error: _ not allowed in this context: line %d" x
let helper n s = if s = blank_s then [ blank_error n ] else []

let rec blank_def line (t: typesDef) =
  match t with
  | TypeT s -> helper line s
  | ArrayT (s, _) -> helper line s
  | SliceT (s, _) -> helper line s
  | StructT l ->
    l
    |> List.map (fun (ls, ts) ->
      List.map (helper 0) ls
      |> List.flatten
      |> List.append (blank_def line ts)
    )
    |> List.flatten

let rec blank_exp (e: exp node) : string list =
    match e.value with
    | Id l -> blank_kind e.position.pos_lnum l
    | BinaryOp (_, (a, b)) ->
      blank_exp a
      |> List.append (blank_exp b)
    | Unaryexp (_, a) -> blank_exp a
    | FuncCall (_, l) ->
      l
      |> List.map blank_exp
      |> List.flatten
    | Append (a, b) ->
      blank_exp a
      |> List.append (blank_exp b)
    | _ -> []
and blank_kind line (k: kind) : string list =
  k
  |> List.map (fun x ->
    match x with
    | Variable s -> helper line s
    | Array (s, l) ->
      l
      |> List.map blank_exp
      |> List.append [helper line s]
      |> List.flatten
  )
  |> List.flatten

let blank_simple (simp: simpleStm node) =
  match simp.value with
  | Assign (a, (l, e)) ->
    let e =
      e
      |> List.map blank_exp
      |> List.flatten in
    let l =
      match a with
      | Regular -> []
      | _ -> l |> List.map (blank_kind simp.position.pos_lnum) |> List.flatten in
    List.append e l
  | ExpStatement e -> blank_exp e
  | ShortDeclaration (l, e) ->
    e
    |> List.map blank_exp
    |> List.flatten
    |> List.append (List.map (blank_kind simp.position.pos_lnum) l |> List.flatten)
  | DoubleMinus s -> helper simp.position.pos_lnum s
  | DoublePlus s -> helper simp.position.pos_lnum s
  | Empty -> []

let rec blank_stm (stm: stmt node) =
  match stm.value with
  | Block l ->
    l
    |> List.map blank_stm
    |> List.flatten
  | Print l ->
    l
    |> List.map blank_exp
    |> List.flatten
  | Println l ->
    l
    |> List.map blank_exp
    |> List.flatten
  | Declaration l ->
    l
    |> List.map (fun (s, _, es) ->
      List.map blank_exp es
      |> List.flatten
    )
    |> List.flatten
  | TypeDeclaration l ->
    l
    |> List.map (fun (s, ts) ->
      blank_def stm.position.pos_lnum ts
      |> List.append (helper stm.position.pos_lnum s)
    )
    |> List.flatten
  | If (s, e, l, el) ->
    let s =
      s
      |> Option.map blank_simple
      |> Option.default [] in
    let e =
      e
      |> Option.map blank_exp
      |> Option.default [] in
    let l =
      l
      |> List.map blank_stm
      |> List.flatten in
    let el =
      el
      |> Option.map (fun x -> List.map blank_stm x)
      |> Option.default []
      |> List.flatten in
    s
    |> List.append e
    |> List.append l
    |> List.append el
  | Loop l ->
    (match l with
    | While (e, l) ->
      e
      |> Option.map blank_exp
      |> Option.default []
    | For (s1, e, s2, l) ->
      let a =
        blank_simple s1
        |> List.append (blank_simple s2) in
      let b =
        e
        |> Option.map blank_exp
        |> Option.default [] in
      let c =
        l
        |> List.map blank_stm
        |> List.flatten in
      a
      |> List.append b
      |> List.append c)
  | Simple s -> blank_simple s
  | Return e -> e |> Option.map blank_exp |> Option.default []
  | _ -> []

let illegal_blanks (prog: program) =
  let p, d = prog in
  let blanks =
    d
    |> List.map (fun x ->
      match x.value with
      | Var l ->
        l
        |> List.map (fun (_, _, exps) ->
          exps
          |> List.map (fun x -> blank_exp x)
          |> List.flatten
        )
        |> List.flatten
      | Fct (name, args, _, s) ->
        let name = helper x.position.pos_lnum name in
        let args =
          args
          |> List.map (fun (arg, _) -> helper x.position.pos_lnum arg)
          |> List.flatten in
        let s =
          s
          |> List.map blank_stm
          |> List.flatten in
        name
        |> List.append s
        |> List.append args
      | _ -> []
    )
    |> List.flatten in
  match blanks with
  | [] -> ""
  | x::_ -> x
