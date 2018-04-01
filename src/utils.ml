open BatOption

module Option = BatOption

let inline (|>) x f = f x
let inline (||>) (x1,x2) f = f x1 x2
let inline (|||>) (x1,x2,x3) f = f x1 x2 x3
let inline (<|) f x = f x
let inline (<||) f (x1,x2) = f x1 x2
let inline (<|||) f (x1,x2,x3) = f x1 x2 x3
let inline (>>) f g x = g(f x)
let inline (<<) f g x = f(g x)

let bytes_insert_byte bytes b p =
  let before = Bytes.sub bytes 0 p in
  let after = Bytes.sub bytes p (Bytes.length bytes - p) in
  Bytes.concat (Bytes.make 1 b) [before; after]

let string_of_char c = Printf.sprintf "%c" c

let num_of_char c = Int64.of_int (match c with
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a'
    | 'A' -> 10
    | 'b'
    | 'B' -> 11
    | 'c'
    | 'C' -> 12
    | 'd'
    | 'D' -> 13
    | 'e'
    | 'E' -> 14
    | 'f'
    | 'F' -> 15
    | _ -> 0)

let list_of_string s =
  let rec rs s a =
    let length = String.length s in
    if length > 0 then
      rs (String.sub s 1 (length - 1)) (String.get s 0 :: a)
    else
      a
  in
  rs s []

let int_of_hex h =
  let digits = list_of_string h in
  let rec process_digit p r t = match r with
    | d::l -> process_digit (p + 1) l (Int64.mul (Int64.add t (num_of_char d)) (16.0 ** (float_of_int p) |> Int64.of_float))
    | [] -> t
  in
  process_digit 0 digits Int64.zero

let int_of_oct o =
  let digits = list_of_string o in
  let rec process_digit p r t = match r with
    | d::l -> process_digit (p + 1) l (Int64.mul (Int64.add t (num_of_char d)) (8.0 ** (float_of_int p) |> Int64.of_float))
    | [] -> t
  in
  process_digit 0 digits Int64.zero

let bind x f = Option.bind f x
let bind2 f (x, y) = match x, y with
  | Some x, Some y -> f x y
  | _ -> None
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
