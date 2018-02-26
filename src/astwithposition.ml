type 'a node = {position: Lexing.position; value: 'a}

type types =
    | TypeT of string
    | StructT of (string list * string) list (* var name * type *)

type kind =
    | Variable of string
    | Array of (string * int)
    | Struct of (string * string)

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
    | Append

type unary =
    | Not
    | UMinus
    | UPlus
    | UCaret

type exp =
    | Id of string
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
    | FuncCall of exp node list (* can also represent a typecast operation *)

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

type case = exp node * stmt list
and loop =
    | While of exp node option * stmt node list
    | For of stmt node * exp node * stmt node * stmt node list
and stmt =
    | Print of exp node list
    | Println of exp node list
    | Declaration of (string list * string option * (exp node) list) list
    | TypeDeclaration of (string * types) list
    | If of simpleStm option * exp node option * (stmt node) list * (stmt node list) option
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
    | Switch of case list
    | Type
    | Simple of simpleStm

type package = string

type argument = (string * string option)

type decl =
    | Var of (string list * string option * (exp node) list) list
    | Type of (string * types) list (* type new old OR type new struct { (string * string) list } *)
    | Fct of (string * argument list * string option * stmt node list)

type program = package * decl node list
