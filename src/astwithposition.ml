type 'a node = {position: Lexing.position; value: 'a}

type types =
    | IntT
    | FloatT
    | StringT
    | RunesT
    | BoolT
    | HexT
    | OctalT
    | StructT of struct_item list
and struct_item = (string * types)

type binary =
    | Plus
    | Minus
    | Times
    | Div
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

type unary =
    | Not
    | UMinus

type exp =
    | Id of string
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Octal of string
    | Hex of string
    | BinaryOp of binary * (exp node * exp node)
    | Unaryexp of unary * exp node

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

type case = exp node * stmt list

and loop =
    | While of exp node * stmt list
    | For of exp node * exp node * exp node * stmt list
and stmt =
    | Print of exp node list
    | Println of exp node list
    | Append of exp node * exp node
    | Assign of assign * (string * exp node)
    | Declaration of (string list * types option * (exp node) list) list
    | If of exp node * (stmt node) list * (stmt node list) option
    | Loop of loop
    | LeftArrow of (string * string)
    | DoublePlus of string
    | DoubleMinus of string
    | ColonEqual of (string * exp node)
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
    | Return of exp node
    | Select
    | Struct
    | Switch of case list
    | Type

type package = string

type argument = (string * types option)

type decl =
    | Var of (string list * types option * (exp node) list) list
    | Type of (string * types)
    | Fct of (string * argument list * stmt node list)

type program = package * decl node list
