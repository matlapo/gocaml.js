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
