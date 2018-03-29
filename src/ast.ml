type gotype =
  | Defined of string * int (* the int is a unique id / level *)
  | Array of gotype * Int64.t
  | Slice of gotype
  | Struct of (string * gotype) list

type 'a fct_return =
  | NonVoid of 'a
  | Void

type scope =
  {
    bindings: (string * gotype) list;
    types: (string * gotype) list;
    functions: (string * gotype list * gotype fct_return) list; (* function name - argument types - return type *)
    parent: scope option; (* top level scope doesn't have a parent *)
    children: scope list
  }

type 'a node = { position: Lexing.position; value: 'a }
type 'a tnode = { position: Lexing.position; typ: gotype; value: 'a }
type 'a snode = { position: Lexing.position; scope: scope; value: 'a }

type 'a gen_node =
  | Position of 'a node
  | Typed of 'a tnode
  | Scoped of 'a snode

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
  | BinaryOp of binary * (exp gen_node * exp gen_node)
  | Unaryexp of unary * exp gen_node
  | FuncCall of string * exp gen_node list (* can also represent a typecast operation *)
  | Append of exp gen_node * exp gen_node
  (* Represents a reference to a variable *)
and kind = kind_elem list
and kind_elem =
  | Variable of string
  | Array of string * (exp gen_node list) (* name of array + indexes *)

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
  | Assign of assign * (kind list * exp gen_node list)
  | ExpStatement of exp gen_node
  | DoublePlus of kind
  | DoubleMinus of kind
  | ShortDeclaration of (kind list * (exp gen_node) list)
  | Empty

(* Represents a case of a switch *)
type case = exp gen_node list option * stmt gen_node list
and loop =
  | While of exp gen_node option * stmt gen_node list
  | For of simpleStm gen_node * exp gen_node option * simpleStm gen_node * stmt gen_node list
and elseif = (stmt gen_node) list
and stmt =
  (* Random block {} *)
  | Block of stmt gen_node list
  | Print of exp gen_node list
  | Println of exp gen_node list
  | Declaration of (string list * gotype option * (exp gen_node) list) list
  | TypeDeclaration of (string * gotype) list
  | If of simpleStm gen_node * exp gen_node * (stmt gen_node) list * elseif option
  | Loop of loop
  | Return of exp gen_node option
  | Switch of simpleStm gen_node * exp gen_node option * case list
  | Simple of simpleStm gen_node
  | Break
  | Continue
  | Default

type package = string

type argument = (string * gotype) (* arg name + type *)

type decl =
  | Var of (string list * gotype option * (exp gen_node) list) list
  | Type of (string * gotype) list (* name + type def *)
  | Fct of (string * argument list * gotype fct_return * stmt gen_node list)

type program = package * decl gen_node list
