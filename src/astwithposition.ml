type 'a node = {position: Lexing.position; value: 'a}

type typesDef =
    | TypeT of string
    | StructT of (string list * typesDef) list
    | ArrayT of string * int
    | SliceT of string

type typesRef =
    | TypeR of string
    | ArrayR of string * int
    | SliceR of string

type kind_elem =
    | Variable of string
    | Array of (string * int)

type kind = kind_elem list

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
    | Int of int
    | Float of float
    | String of string
    | RawStr of string
    | Rune of string
    | Bool of bool
    | Octal of string
    | Hex of string
    | BinaryOp of binary * (exp node * exp node)
    | Unaryexp of unary * exp node
    | FuncCall of string * exp node list (* can also represent a typecast operation *)
    | Append of exp node * exp node

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
    | DoublePlus
    | DoubleMinus

type simpleStm =
    | Assign of assign * (kind * exp node)
    | ExpStatement of exp node
    | DoublePlus of string
    | DoubleMinus of string
    | ShortDeclaration of (string list * (exp node) list)
    | Empty

type case = exp node option * stmt node list
and loop =
    | While of exp node option * stmt node list
    | For of simpleStm node * exp node * simpleStm node * stmt node list
and stmt =
    | Print of exp node list
    | Println of exp node list
    | Assign of assign * (kind * exp node)
    | Declaration of (string list * typesRef option * (exp node) list) list
    | TypeDeclaration of (string * typesDef) list
    | If of (simpleStm node) option * exp node option * (stmt node) list * (stmt node list) option
    | Loop of loop
    | LeftArrow of (string * string)
    | Break
    | Chan
    | Const
    | Continue
    | Default
    | Defer
    | Fallthrough
    | Func
    | Go
    | Goto
    | Import
    | Iface
    | Map
    | Range
    | Return of exp node option
    | Select
    | Struct
    | Switch of exp node * case list
    | Type
    | Simple of simpleStm node

type package = string

type argument = (string * typesRef option)

type decl =
    | Var of (string list * typesRef option * (exp node) list) list
    | Type of (string * typesDef) list
    | Fct of (string * argument list * typesRef option * stmt node list)

type program = package * decl node list
