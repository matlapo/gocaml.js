open Astwithposition
open Utils
open BatOption

module Option = BatOption
let bind x f = Option.bind f x
let id x = x

(* Helpers for finding invalid blank (i.e '_') usage *)
let blank_s = "_"
let blank_error x = Printf.sprintf "Error: _ not allowed in this context: line %d" x
let helper n s = if s = blank_s then [ blank_error n ] else []

(* Helpers for finding missing default cases in switch statements *)
let default_many_error x = Printf.sprintf "Error: multiple defaults in switch statement: line %d" x

(* Helpers for finding invalid use of continue/break *)
let continue_error x = Printf.sprintf "Error: invalid usage of keyword 'continue': line %d" x
let break_error x = Printf.sprintf "Error: invalid usage of keyword 'break': line %d" x

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

let blank_ref line (t: typesRef) =
  match t with
  | TypeR s -> helper line s
  | ArrayR (s, _) -> helper line s
  | SliceR (s, _) -> helper line s

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
  | DoubleMinus s -> blank_kind simp.position.pos_lnum s
  | DoublePlus s -> blank_kind simp.position.pos_lnum s
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
    |> List.map (fun (s, d, es) ->
      let d =
        d
        |> Option.map (blank_ref stm.position.pos_lnum)
        |> Option.default [] in
      List.map blank_exp es
      |> List.flatten
      |> List.append d
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
  | Switch (s, e, cs) ->
    let s =
      s
      |> Option.map blank_simple
      |> Option.default [] in
    let e =
      e
      |> Option.map blank_exp
      |> Option.default [] in
    cs
    |> List.map (fun x ->
      let (exps, stms) = x in
      let exps =
        exps
        |> Option.map (fun x ->
          x
          |> List.map blank_exp
          |> List.flatten
        )
        |> Option.default [] in
      stms
      |> List.map blank_stm
      |> List.flatten
      |> List.append exps
    )
    |> List.flatten
    |> List.append e
    |> List.append s
  | _ -> []

let check_default (s: stmt node) =
  match s.value with
  | Switch (_, _, cs) ->
    let d =
      cs
      |> List.fold_left (fun acc (e, _) -> if Option.is_none e then acc + 1 else acc) 0 in
    if d > 1 then [default_many_error s.position.pos_lnum]
    else []
  | _ -> []

let check_cont_break (s: stmt node) =
  let rec helper (s: stmt node) (seenLoop: bool) (seenSwitch: bool) =
    match s.value with
    | Continue -> if seenLoop = true then [] else [continue_error s.position.pos_lnum]
    | Break ->
      if seenLoop = true || seenSwitch = true then []
      else [break_error s.position.pos_lnum]
    | Loop loop ->
      (match loop with
      | While (_, s) ->
        s
        |> List.map (fun x -> helper x true true)
        |> List.flatten
      | For (_, _, _, s) ->
        s
        |> List.map (fun x -> helper x true true)
        |> List.flatten
      )
    | Block s ->
        s
        |> List.map (fun x -> helper x seenLoop seenSwitch)
        |> List.flatten
    | If (_, _, s, e) ->
      let e =
        e
        |> Option.map (fun x ->
          x
          |> List.map (fun x -> helper x seenLoop seenSwitch)
          |> List.flatten
        )
        |> Option.default [] in
      s
      |> List.map (fun x -> helper x seenLoop seenSwitch)
      |> List.flatten
      |> List.append e
    | Switch (_, _, cs) ->
      cs
      |> List.map (fun (e, s) ->
        s
        |> List.map (fun x -> helper x seenLoop true)
        |> List.flatten
      )
      |> List.flatten
    | _ -> []
  in helper s false false

let illegal_blanks (prog: program) =
  let p, d = prog in
  let blanks =
    d
    |> List.map (fun x ->
      match x.value with
      | Var l ->
        l
        |> List.map (fun (_, r, exps) ->
          let exps =
            exps
            |> List.map (fun x -> blank_exp x)
            |> List.flatten in
          r
          |> Option.map (blank_ref x.position.pos_lnum)
          |> Option.default []
          |> List.append exps
        )
        |> List.flatten
      | Fct (name, args, _, s) ->
        let continue =
          s
          |> List.map check_cont_break
          |> List.flatten in
        let default =
          s
          |> List.map check_default
          |> List.flatten in
        let name = helper x.position.pos_lnum name in
        let args =
          args
          |> List.map (fun (_, r) ->
            r
            |> Option.map (blank_ref x.position.pos_lnum)
            |> Option.default []
          )
          |> List.flatten in
        let s =
          s
          |> List.map blank_stm
          |> List.flatten in
        name
        |> List.append s
        |> List.append args
        |> List.append continue
        |> List.append default
      | _ -> []
    )
    |> List.flatten in
  match blanks with
  | [] -> ""
  | x::_ -> x