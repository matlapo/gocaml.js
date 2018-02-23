module type Ast =
    sig
        type 'a node

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

        type loop =
            | While of exp node * stmt list
            | For of exp node * exp node * exp node
        and stmt =
            | Print of exp node
            | Println of exp node
            | Append of exp node * exp node
            | Assign of assign * (string * exp node)
            | Declaration of (string * types * exp node)
            | If of exp node * (stmt node) list * ((stmt node) list) option
            | Loop of loop
            | LeftArrow of (string * string)
            | DoublePlus of string
            | DoubleMinus of string
            | ColonEqual of (string * exp node)
            | Break
            | Case of exp node
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

        type program = stmt list
    end 
