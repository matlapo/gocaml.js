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
  | p = package    { p * [] }
  | p = package ds = decls { p * ds }
  ;

package:
  | TPACKAGE id = TIDENTIFIER { id }
  ;

decls:
  | d1 = dec d2 = decls { d1::d2 }
  | d = dec             { [d] }
  ;

dec:
  | vb = var_block { {position = $symbolstartpos; value = vb } }
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
    { (ids, None, Some { position = $symbolstartpos; value = exps }) }
  | ids = id_list t = types exps = exp_list
    { (ids, Some t, Some { position = $symbolstartpos; value = exps }) }
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
  | TCONST { { position = $symbolstartpos; value = Id "test" } }
  ;
