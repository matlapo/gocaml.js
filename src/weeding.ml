open Astwithposition
open Utils
open BatOption

module Option = BatOption
let bind x f = Option.bind f x

let blank_s = "_"

let rec blank_def (t: typesDef) =
  match t with
  | TypeT s -> s = blank_s
  | StructT l ->
    l
    |> List.exists (fun (ls, ts) ->
      List.exists (fun x -> x = blank_s) ls
      || blank_def ts
    )
  | ArrayT (s, _) -> s = blank_s
  | SliceT (s, _) -> s = blank_s


let rec blank_exp (e: exp node) =
  match e.value with
  | Id l ->
    l
    |> List.exists (function
      | Variable v -> v = blank_s
      | _ -> false
    )
  | BinaryOp (_, (a, b)) ->
    blank_exp a || blank_exp b
  | Unaryexp (_, a) ->
    blank_exp a
  | FuncCall (_, l) ->
    List.exists (fun x -> blank_exp x) l
  | Append (a, b) ->
    blank_exp a || blank_exp b
  | _ -> false

let rec blank_kind (k: kind) =
  k
  |> List.exists (fun x ->
    match x with
    | Variable s -> s = blank_s
    | Array (s, l) ->
      s = blank_s ||
      l |> List.exists (fun x -> blank_exp x)
  )

let blank_simple (s: simpleStm node) =
  match s.value with
  | Assign (_, (l, e)) ->
    List.exists blank_kind l
    || List.exists blank_exp e
  | ExpStatement e -> blank_exp e
  | DoublePlus s -> s = blank_s
  | DoubleMinus s -> s = blank_s
  | ShortDeclaration (k, l) ->
    List.exists blank_kind k
    || List.exists blank_exp l
  | Empty -> false

let rec blank_stm (s: stmt node) =
  match s.value with
  | Block l ->
    List.exists (fun x -> blank_stm x) l
  | Print l ->
    List.exists (fun x -> blank_exp x) l
  | Println l ->
    List.exists (fun x -> blank_exp x) l
  | Declaration l ->
    l
    |> List.exists (fun (s, _, es) ->
      es
      |> List.exists (fun x -> blank_exp x)
    )
  | TypeDeclaration l ->
    l
    |> List.exists (fun (s, ts) ->
      s = blank_s || blank_def ts
    )
  | If (s, e, l, el) ->
    s
    |> bind (fun x -> Some (blank_simple x))
    |> Option.default false
    || e
      |> bind (fun x -> Some (blank_exp x))
      |> Option.default false
    || l
      |> List.exists blank_stm
    || el
      |> Option.map (fun x -> List.exists blank_stm x)
      |> Option.default false
  | Loop l ->
    (match l with
    | While (e, l) ->
      e |> Option.map blank_exp |> Option.default false
    | For (s1, e, s2, l) ->
      blank_simple s1
      || blank_simple s2
      || e |> Option.map blank_exp |> Option.default false
      || List.exists blank_stm l
    )
  | Simple s -> blank_simple s
  | _ -> false

let illegal_blanks (prog: program) =
  let p, d = prog in
  d
  |> List.map (fun x ->
    match x.value with
    | Var l ->
      l
      |> List.exists (fun (_, _, exps) ->
        exps
        |> List.exists (fun x -> blank_exp x)
      )
    | _ -> false
  )
