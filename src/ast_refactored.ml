(* Represents the definition of a new type *)
type typesDef =
  | TypeT of string
  | StructT of (string * typesDef) list
  | ArrayT of typesDef * int64
  | SliceT of typesDef

(* Represents the reference to an existing type *)
type typesRef =
  | TypeR of string
  | ArrayR of typesRef * int64
  | SliceR of typesRef

type 'a return_type =
  | Void
  | Type of 'a

type function_signature =
  {
    name: string;
    args: typesDef list;
    return: typesDef return_type
  }

type scope =
  {
    bindings: (string * typesDef) list;
    types: (string * typesDef) list;
    functions: function_signature list;
    parent: scope option; (* top level scope doesn't have a parent *)
    children: scope list
  }

type 'a node = { position: Lexing.position; value: 'a }
type 'a tnode = { position: Lexing.position; typ: typesDef; value: 'a }
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
  | Id of string
  | Access of exp gen_node access
  | Int of int64
  | Float of float
  | String of string
  | RawStr of string
  | Rune of string
  | Bool of bool
  | BinaryOp of binary * (exp gen_node * exp gen_node)
  | Unaryexp of unary * exp gen_node
  | FuncCall of exp gen_node * exp gen_node list (* can also represent a typecast operation *)
  | Append of exp gen_node * exp gen_node
  (* Will by to the left of an assignation *)
and addressable_exp =
  | IdA of string
  | AccessA of addressable_exp gen_node access
and 'a access =
  | Member of string * 'a
  | Index of (exp gen_node) * 'a

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
  | Assign of assign * (addressable_exp list * exp gen_node list)
  | ExpStatement of exp gen_node
  | DoublePlus of addressable_exp
  | DoubleMinus of addressable_exp
  | ShortDeclaration of (addressable_exp list * (exp gen_node) list)
  | Empty

(* Represents a case of a switch *)
type case = exp gen_node list option * stmt gen_node list
and loop =
  | While of exp gen_node option * stmt gen_node list
  | For of simpleStm gen_node * exp gen_node option * simpleStm gen_node * stmt gen_node list
and elseif = (stmt gen_node) list
and stmt =
  (* Random block {} *)
  | Block of stmt gen_node list
  | Print of exp gen_node list
  | Println of exp gen_node list
  | Declaration of (string list * typesRef option * (exp gen_node) list) list
  | TypeDeclaration of (string * typesDef) list
  | If of simpleStm gen_node * exp gen_node * (stmt gen_node) list * elseif option
  | Loop of loop
  | Return of exp gen_node option
  | Switch of simpleStm gen_node option * exp gen_node option * case list
  | Simple of simpleStm gen_node
  | Break
  | Continue
  | Default

type package = string

type argument = (string * typesRef) (* arg name + type *)

type decl =
  | Var of (string list * typesRef return_type * (exp gen_node) list) list
  | Type of (string * typesDef) list (* name + type def *)
  | Fct of (string * argument list * typesRef option * stmt gen_node list)

type program = package * decl gen_node list
