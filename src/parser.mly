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

%start <program> prog
%%

prog:
  | p = package    { p, [] }
  | p = package ds = decls { p, ds }
  ;

package:
  | TPACKAGE id = TIDENTIFIER { id }
  ;

decls:
  | d1 = dec d2 = decls { d1::d2 }
  | d = dec             { [d] }
  ;

dec:
  | vb = var_block { { position = $symbolstartpos; value = vb } }
  ;

var_block:
  | TVAR TOPENINGBRACKET vars = var_list TCLOSINGBRACKET { Var vars }
  ;

var_list:
  | v1 = var v2 = var_list { v1::v2 }
  | v = var                { [v] }
  ;

var:
  | ids = id_list t = types exps = exp_list
    { (ids, Some t, None) }
  | ids = id_list TASSIGN exps = exp_list
    { (ids, None, Some exps) }
  | ids = id_list t = types exps = exp_list
    { (ids, Some t, Some exps) }
  ;

types:
  | TOPENINGBRACKET t = types TCLOSINGBRACKET { t }
  | t = TIDENTIFIER { t }
  ;

id_list:
  | id = TIDENTIFIER TCOMMA ids = var_list { id::ids }
  | id = TIDENTIFIER                       { [id] }
  ;

exp_list:
  | e = exp TCOMMA l = exp_list { e::l }
  | e = exp                     { [e] }
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
    { Plus * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TMINUS e2 = exp
    { Minus * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TTIMES e2 = exp
    { Times * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TDIV e2 = exp
    { Div * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TEQUALS e2 = exp
    { Equals * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TNOTEQUAL e2 = exp
    { Equals * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TAND e2 = exp
    { And * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TOR e2 = exp
    { Or * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TSMALLER e2 = exp
    { Smaller * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TGREATER e2 = exp
    { Greater * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TSMALLEREQ e2 = exp
    { SmallerEq * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TGREATEREQ e2 = exp
    { GreaterEq * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TDSMALLER e2 = exp
    { DSmaller * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | e1 = exp TDGREATER e2 = exp
    { DGreater * ({ position = $symbolstartpos; value = e1 } * { position = $symbolstartpos; value = e2 }) }
  | TNOT e = exp { Not * { position = $symbolstartpos; value = e } }
  | TMINUS e = exp { Minus * { position = $symbolstartpos; value = e } }
  ;

stm_list:
  | s = stm TSEMICOLON l = stm_list { { position = $symbolstartpos; value = s::l } }
  | s = stm                         { { position = $symbolstartpos; value = [s] } }
  ;

stm:
  | TCONST { { position = $symbolstartpos; value = Id "test" } }
  ;
