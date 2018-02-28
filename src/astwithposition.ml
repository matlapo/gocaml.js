type 'a node = {position: Lexing.position; value: 'a}

(* Represents the definition of a new type *)
type typesDef =
    | TypeT of string
    | StructT of (string list * typesDef) list
    | ArrayT of string * (int64 list) (* The list of int64 is the size of each dimension *)
    | SliceT of string * int64 (* The int64 is the number of dimensions *)

(* Represents the reference to an existing type *)
type typesRef =
    | TypeR of string
    | ArrayR of string * (int64 list) (* The list of int64 is the size of each dimension *)
    | SliceR of string * int64 (* The int64 is the number of dimensions *)

type binary =
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | Equals
    | NotEquals
    | And
    | Or
    | Smaller
    | Greater
    | SmallerEq
    | GreaterEq
    | DGreater
    | DSmaller
    | AndHat
    | BAnd
    | BOr
    | Caret

type unary =
    | Not
    | UMinus
    | UPlus
    | UCaret

type exp =
    | Id of kind
    | Int of int64
    | Float of float
    | String of string
    | RawStr of string
    | Rune of string
    | Bool of bool
    | BinaryOp of binary * (exp node * exp node)
    | Unaryexp of unary * exp node
    | FuncCall of string * exp node list (* can also represent a typecast operation *)
    | Append of exp node * exp node
(* Represents a reference to a variable *)
and kind = kind_elem list
and kind_elem =
    | Variable of string
    | Array of string * (exp node list)

type assign =
    | Regular
    | PlusEqual
    | MinusEqual
    | DivEqual
    | TimesEqual
    | AndEqual
    | OrEqual
    | HatEqual
    | PercentEqual
    | AndHatEqual
    | DoubleGreaterEqual
    | DoubleSmallerEqual

(* Exactly like GoLang simpleStm *)
type simpleStm =
    | Assign of assign * (kind list * exp node list)
    | ExpStatement of exp node
    | DoublePlus of kind
    | DoubleMinus of kind
    | ShortDeclaration of (kind list * (exp node) list)
    | Empty

(* Represents a case of a switch *)
type case = exp node list option * stmt node list
and loop =
    | While of exp node option * stmt node list
    | For of simpleStm node * exp node option * simpleStm node * stmt node list
and elseif = (stmt node) list
and stmt =
    (* Random block {} *)
    | Block of stmt node list
    | Print of exp node list
    | Println of exp node list
    | Declaration of (string list * typesRef option * (exp node) list) list
    | TypeDeclaration of (string * typesDef) list
    | If of (simpleStm node) option * exp node option * (stmt node) list * elseif option
    | Loop of loop
    | Return of exp node option
    | Switch of (simpleStm node) option * exp node option * case list
    | Simple of simpleStm node
    | Break
    | Continue
    | Default

type package = string

type argument = (string * typesRef option)

type decl =
    | Var of (string list * typesRef option * (exp node) list) list
    | Type of (string * typesDef) list
    | Fct of (string * argument list * typesRef option * stmt node list)

type program = package * decl node list
