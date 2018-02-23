type types =
    | Int
    | Float
    | String 
    | Bool
    | Hex
    | Octal

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
    | Minus

type exp =
    | Id of string 
    | Int of int 
    | Float of float 
    | String of string 
    | Bool of bool
    | BinaryOp of binary * (exp * exp)
    | Unaryexp of unary * exp

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

type loop =
    | While of exp * node list
    | For of exp * exp * exp
and stmt =
    | Print of exp 
    | Println of exp
    | Append of exp * exp
    | Assign of assign * (string * exp)
    | Declaration of (string * types * exp)
    | If of exp * node list * (node list) option
    | Loop of loop
    | LeftArrow of (string * string)
    | DoublePlus of string
    | DoubleMinus of string
    | ColonEqual of (string * exp)
    | Break
    | Case of exp 
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
    | Return 
    | Select
    | Struct
    | Switch
    | Type
and value =
    | E of exp
    | S of stmt
and node = 
    {
        position: Lexing.position;
        value: value
    }

type program = node list 
