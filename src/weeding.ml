open Astwithposition
open Utils

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
  | SliceT s -> s = blank_s

let rec blank_exp (e: exp) =
  match e with
  | Id l ->
    l
    |> List.exists (function
      | Variable v -> v = blank_s
      | _ -> false
    )
  | BinaryOp (_, (a, b)) ->
    blank_exp a.value || blank_exp b.value
  | Unaryexp (_, a) ->
    blank_exp a.value
  | FuncCall (_, l) ->
    List.exists (fun x -> blank_exp x.value) l
  | Append (a, b) ->
    blank_exp a.value || blank_exp b.value
  | _ -> false

let rec blank_stm (s: stmt) =
  match s with
  | Print l ->
    List.exists (fun x -> blank_exp x.value) l
  | Println l ->
    List.exists (fun x -> blank_exp x.value) l
  | Declaration l ->
    l
    |> List.exists (fun (s, _, es) ->
      es
      |> List.exists (fun x -> blank_exp x.value)
    )
  | TypeDeclaration l ->
    l
    |> List.exists (fun (s, ts) ->
      s = blank_s || blank_def ts
    )
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
        |> List.map (fun x -> x.value)
        |> List.exists (fun x -> blank_exp x)
      )
    | _ -> false
  )
