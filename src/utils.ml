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

let num_of_char c = match c with
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
    | _ -> 0

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
        | d::l -> process_digit (p + 1) l (t + (num_of_char d) * (16.0 ** (float_of_int p) |> int_of_float))
        | [] -> t
    in
        process_digit 0 digits 0

let int_of_oct h =
    let digits = list_of_string h in
    let rec process_digit p r t = match r with
        | d::l -> process_digit (p + 1) l (t + (num_of_char d) * (8.0 ** (float_of_int p) |> int_of_float))
        | [] -> t
    in
        process_digit 0 digits 0
