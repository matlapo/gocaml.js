

(* type gotype =
  | Defined of string * int * gotype
  | Array of gotype * Int64.t
  | Slice of gotype
  | Struct of (string * gotype) list *)

(*
Will go up the scope.resolvedTypes to find the first float64.
Then will add int * (type it found) to currentScope.resolvedTypes
type int float64
{
  type myint int
  var a myint
}

type myint int //0
var b myint //0
{
  Will look in parents scope.resolvedTypes for 'myint', (find_scopedtype_of_string)
  Then will set a binding to c with scopeid being the scope where myint is defined
  var c myint //0
  {
    type myint int
    var a myint
    Will look at the scopedtype of c and b and see if they are the same.
    Then will look in parent scopes to find the definition of the types to get the resolved type.
    Then will see if they add. If they do add, create a tnode with the type being the scoped type of a or b
    a = c + b
  }
}
{

}

type a struct {
  x,y int
}
type a int;
{
  type b a;
}

var y a *)

type basetype =
  | BInt
  | BFloat64
  | BString
  | BRune
  | BBool

type gotype =
  | Basetype of basetype
  | Defined of string
  | Array of gotype * Int64.t
  | Slice of gotype
  | Struct of (string * gotype) list
  | Null

type scopeid = int
type scopedtype = { gotype: gotype; scopeid: scopeid }

type 'a fct_return =
  | NonVoid of 'a
  | Void

type signature =
  {
    arguments: scopedtype list;
    returnt: scopedtype fct_return
  }

type scope =
  {
    scopeid: scopeid;
    bindings: (string * scopedtype) list;
    types: (string * scopedtype) list; (* b : previous type *)
    functions: (string * signature) list; (* function name - argument types - return type *)
    parent: scope option; (* top level scope doesn't have a parent *)
    children: scope list
  }

type 'a node = { position: Lexing.position; value: 'a }
type 'a tnode = { position: Lexing.position; typ: scopedtype; value: 'a }
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
  | Indexing of exp gen_node * exp gen_node
  | Selection of exp gen_node * string
  | Int of int64
  | Float of float
  | String of string
  | RawStr of string
  | Rune of string
  | BinaryOp of binary * (exp gen_node * exp gen_node)
  | Unaryexp of unary * exp gen_node
  | FuncCall of string * exp gen_node list (* can also represent a typecast operation *)
  | Append of exp gen_node * exp gen_node

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
  | Assign of assign * (exp gen_node list * exp gen_node list)
  | ExpStatement of exp gen_node
  | DoublePlus of exp gen_node
  | DoubleMinus of exp gen_node
  | ShortDeclaration of (exp gen_node list * (exp gen_node) list)
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
