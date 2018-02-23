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
            | Struct of struct_item list
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

        type case = exp node * stmt list
        
        and loop =
            | While of exp node * stmt list
            | For of exp node * exp node * exp node * stmt list
        and stmt =
            | Print of exp node
            | Println of exp node
            | Append of exp node * exp node
            | Assign of assign * (string * exp node)
            | Declaration of (string * types * exp node option)
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
        type import = string

        type argument = (string * types)

        type decl =
            | Var of (string * types option * exp node option)
            | Type of (string * types)
            | Fct of (string * argument list * stmt node list)

        type program = package * import list * decl list
    end
