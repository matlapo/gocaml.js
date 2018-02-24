%{
  open Astwithposition
%}
%token TVAR
%token TIF
%token <string> TIDENTIFIER
%token TPRINT
%token TPRINTLN
%token TAPPEND
%token TELSE
%token <int> TINTVAL
%token <float> TFLOATVAL
%token <string> TSTRINGVAL
%token <string> TRAWSTRVAL
%token <bool> TBOOLVAL
%token <string> TOCTOVAL
%token <string> THEXVAL
%token TPLUS
%token TMINUS
%token TTIMES
%token TDIV
%token TEQUALS
%token TNOT
%token TASSIGN
%token TNOTEQUAL
%token TAND
%token TOR
%token TBITAND
%token TBITOR
%token TPLUSEQUAL
%token TMINUSEQUAL
%token TMULTEQUAL
%token TDIVEQUAL
%token TANDEQUAL
%token TOREQUAL
%token THATEQUAL
%token TPERCENTEQUAL
%token TGREATER
%token TSMALLER
%token TGREATEREQ
%token TSMALLEREQ
%token TDGREATER
%token TDSMALLER
%token TLEFTARROW
%token TDGEQUAL
%token TDSEQUAL
%token TDPLUS
%token TDMINUS
%token TCOLEQUAL
%token TANDHAT
%token TWTF
%token TCOLON
%token TSEMICOLON
%token TCOMMA
%token TPERIOD
%token TDOTS
%token TOPENINGBRACE
%token TCLOSINGBRACE
%token TOPENINGBRACKET
%token TCLOSINGBRACKET
%token TOPENINGSQUARE
%token TCLOSINGSQUARE
%token EOF
%token TBREAK
%token TCASE
%token TCHAN
%token TCONST
%token TCONTINUE
%token TDEFAULT
%token TDEFER
%token TFALL
%token TFOR
%token TFUNC
%token TGO
%token TGOTO
%token TIMPORT
%token TIFACE
%token TMAP
%token TPACKAGE
%token TRANGE
%token TRETURN
%token TSELECT
%token TSTRUCT
%token TSWITCH
%token TTYPE

%start <Astwithposition.program> prog
%%

prog:
  | p = package EOF { (p, []) }
  | p = package ds = decls EOF { (p, ds) }
  ;

package:
  | TPACKAGE id = TIDENTIFIER { id }
  ;

decls:
  | d1 = decl_node d2 = decls { d1::d2 }
  | d = decl_node { [d] }
  ;

decl_node:
  | vb = decl_type { { position = $symbolstartpos; value = vb } }
  ;

decl_type:
  | TVAR vars = var_decl { Var vars }
  | TFUNC name = TIDENTIFIER TOPENINGBRACKET args = fct_args TCLOSINGBRACKET TOPENINGBRACE body = stm_list TCLOSINGBRACE
    { Fct (name, args, body) }
  ;

var_decl:
  | d = var_format { [d] }
  | TOPENINGBRACKET ds = var_formats TCLOSINGBRACKET { ds }
  ;

var_formats:
  | v1 = var_format v2 = var_formats { v1::v2 }
  | v = var_format { [v] }
  ;

var_format:
  | vars = var_list t = types { (vars, Some t, []) }
  | vars = var_list TASSIGN exps = exp_list { (vars, None, exps) }
  | vars = var_list t = types TASSIGN exps = exp_list { (vars, Some t, exps) }
  ;

types:
  | TINT { IntT }
  | TFLOAT { FloatT }
  | TSTRING { StringT }
  | TBOOLEAN { BoolT }
  ;

var_list:
  | v1 = TIDENTIFIER TCOMMA v2 = var_list { v1::v2 }
  | v = TIDENTIFIER { [v] }
  ;

exp_list:
  | e = exp TCOMMA l = exp_list { e::l }
  | e = exp { [e] }
  ;

fct_args:
  | { [] }
  | args = args_list { args }

args_list:
  | var = TIDENTIFIER t = types { [(var, Some t)] }
  | vars = var_list t = types { List.map (fun x -> (x, Some t)) vars }
  | vars = var_list t = types TCOMMA l = args_list
    { List.append (List.map (fun x -> (x, Some t)) vars) l } //temporary
  | var = TIDENTIFIER t = types TCOMMA l = args_list { (var, Some t)::l }
  ;

stm_list:
  | TOPENINGBRACE s = stm_list TCLOSINGBRACE { s }
  | s = stm l = stm_list { s::l }
  | s = stm { [s] }
  | { [] }
  ;

  /* | Println of exp node
  | Append of exp node * exp node
  | Assign of assign * (string * exp node)
  | Declaration of (string list * types option * (exp node) list) list
  | If of exp node * (stmt node) list * (stmt node list) option
  | Loop of loop
  | LeftArrow of (string * string)
  | DoublePlus of string
  | DoubleMinus of string
  | ColonEqual of (string * exp node) */

stm:
  | TPRINT TOPENINGBRACKET e = exp_list TCLOSINGBRACKET { { position = $symbolstartpos; value = Print e } }
  | TPRINTLN TOPENINGBRACKET e = exp_list TCLOSINGBRACKET { { position = $symbolstartpos; value = Println e } }
  | var = TIDENTIFIER a = assign_type e = exp { { position = $symbolstartpos; value = Assign (a, (var, e)) } }
  | TVAR d = var_decl { { position = $symbolstartpos; value =  Declaration d } }
  ;

assign_type:
  | TASSIGN { Regular }
  | TPLUSEQUAL { PlusEqual }
  | TMINUSEQUAL { MinusEqual }
  | TMULTEQUAL { TimesEqual }
  | TDIVEQUAL { DivEqual }
  | TANDEQUAL { AndEqual }
  | TOREQUAL { OrEqual }
  | THATEQUAL { HatEqual }
  | TPERCENTEQUAL { PercentEqual }
  ;

exp:
  | TOPENINGBRACKET e = exp TCLOSINGBRACKET { e }
  | id = TIDENTIFIER { { position = $symbolstartpos; value = Id id } }
  | i = TINTVAL { { position = $symbolstartpos; value = Int i } }
  | f = TFLOATVAL { { position = $symbolstartpos; value = Float f } }
  | s = TSTRINGVAL { { position = $symbolstartpos; value = String s } }
  | b = TBOOLVAL { { position = $symbolstartpos; value = Bool b } }
  | h = THEXVAL { { position = $symbolstartpos; value = Hex h } }
  | o = TOCTOVAL { { position = $symbolstartpos; value = Octal o } }
  | e1 = exp TPLUS e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Plus, (e1, e2)) } }
  | e1 = exp TMINUS e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Minus, (e1, e2)) } }
  | e1 = exp TTIMES e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Times, (e1, e2)) } }
  | e1 = exp TDIV e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Div, (e1, e2)) } }
  | e1 = exp TEQUALS e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Equals, (e1, e2)) } }
  | e1 = exp TNOTEQUAL e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (NotEquals, (e1, e2)) } }
  | e1 = exp TAND e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (And, (e1, e2)) } }
  | e1 = exp TOR e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Or, (e1, e2)) } }
  | e1 = exp TSMALLER e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Smaller, (e1, e2)) } }
  | e1 = exp TGREATER e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Greater, (e1, e2)) } }
  | e1 = exp TSMALLEREQ e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (SmallerEq, (e1, e2)) } }
  | e1 = exp TGREATEREQ e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (GreaterEq, (e1, e2)) } }
  | e1 = exp TDSMALLER e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (DSmaller, (e1, e2)) } }
  | e1 = exp TDGREATER e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (DGreater, (e1, e2)) } }
  | TMINUS e = exp
    { { position = $symbolstartpos; value = Unaryexp (UMinus, e) } }
  | TNOT e = exp
    { { position = $symbolstartpos; value = Unaryexp (Not, e) } }
  ;
