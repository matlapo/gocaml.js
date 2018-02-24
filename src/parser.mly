%{
  open Astwithposition
%}
%token TVAR
%token TIF
%token <string> TIDENTIFIER
%token TINT
%token TFLOAT
%token TSTRING
%token TBOOLEAN
%token TPRINT
%token TPRINTLN
%token TAPPEND
%token TELSE
%token TTRUE
%token TFALSE
%token <int> TINTVAL
%token <float> TFLOATVAL
%token <string> TSTRINGVAL
%token <string> TRAWSTRVAL
%token <bool> TBOOLVAL
%token <int> TOCTOVAL
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
  | e = exp TCOMMA l = exp_list { { position = $symbolstartpos; value = e }::l }
  | e = exp { [{ position = $symbolstartpos; value = e }] }
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
  | s = stm l = stm_list { { position = $symbolstartpos; value = s }::l }
  | s = stm { [{ position = $symbolstartpos; value = s }] }
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
  | TPRINT TOPENINGBRACKET e = exp TCLOSINGBRACKET { Print { position = $symbolstartpos; value = e } }
  | TPRINTLN TOPENINGBRACKET e = exp TCLOSINGBRACKET { Println { position = $symbolstartpos; value = e } }
  | var = TIDENTIFIER a = assign_type e = exp { Assign (a, (var, { position = $symbolstartpos; value = e })) }
  | TVAR d = var_decl { Declaration d }
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
  | id = TIDENTIFIER { Id id }
  | i = TINTVAL { Int i }
  | f = TFLOATVAL { Float f }
  | s = TSTRINGVAL { String s }
  | b = TBOOLVAL { Bool b }
  | h = THEXVAL { Hex h }
  | o = TOCTOVAL { Octal o }
  | e1 = exp TPLUS e2 = exp
    { BinaryOp (Plus, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TMINUS e2 = exp
    { BinaryOp (Minus, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TTIMES e2 = exp
    { BinaryOp (Times, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TDIV e2 = exp
    { BinaryOp (Div, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TEQUALS e2 = exp
    { BinaryOp (Equals, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TNOTEQUAL e2 = exp
    { BinaryOp (NotEquals, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TAND e2 = exp
    { BinaryOp (And, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TOR e2 = exp
    { BinaryOp (Or, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TSMALLER e2 = exp
    { BinaryOp (Smaller, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TGREATER e2 = exp
    { BinaryOp (Greater, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TSMALLEREQ e2 = exp
    { BinaryOp (SmallerEq, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TGREATEREQ e2 = exp
    { BinaryOp (GreaterEq, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TDSMALLER e2 = exp
    { BinaryOp (DSmaller, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | e1 = exp TDGREATER e2 = exp
    { BinaryOp (DGreater, ({ position = $symbolstartpos; value = e1 }, { position = $symbolstartpos; value = e2 })) }
  | TMINUS e = exp
    { Unaryexp (UMinus, { position = $symbolstartpos; value = e }) }
  | TNOT e = exp
    { Unaryexp (Not, { position = $symbolstartpos; value = e }) }
  ;
