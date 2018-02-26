%{
  open Astwithposition
  open Utils
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
%token <string> TRUNEVAL
%token <bool> TBOOLVAL
%token <string> TOCTOVAL
%token <string> THEXVAL
%token TPLUS
%token TUPLUS
%token TMINUS
%token TUMINUS
%token TTIMES
%token TDIV
%token TMOD
%token TCARET
%token TUCARET
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
%token TOPENINGPAR
%token TCLOSINGPAR
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

%left TOR
%left TAND
%left TEQUALS TNOTEQUAL TSMALLER TSMALLEREQ TGREATER TGREATEREQ
%left TPLUS TMINUS TBITOR TCARET
%left TTIMES TDIV TMOD TDSMALLER TDGREATER TBITAND TANDHAT
%left TUPLUS TUMINUS TNOT TUCARET

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

//top-level declarations
decl_type:
  | TVAR vars = var_decls { Var vars }
  | TFUNC name = TIDENTIFIER TOPENINGPAR args = fct_args TCLOSINGPAR ret = fct_return TOPENINGBRACE body = stm_list TCLOSINGBRACE
    { Fct (name, args, ret, body) }
  | TTYPE t = type_decls { Type t }
  ;

//rules for variable declarations
var_decls:
  | d = var_format { [d] }
  | TOPENINGPAR ds = var_formats TCLOSINGPAR { ds }
  ;

var_formats:
  | v1 = var_format v2 = var_formats { v1::v2 }
  | v = var_format { [v] }
  ;

var_format:
  | vars = var_list t = TIDENTIFIER { (vars, Some t, []) }
  | vars = var_list TOPENINGSQUARE size = array_size TCLOSINGSQUARE t = TIDENTIFIER { (vars, Some (t ^ "[" ^ size ^ "]"), []) }
  | vars = var_list TASSIGN exps = exp_list { (vars, None, exps) }
  | vars = var_list t = TIDENTIFIER TASSIGN exps = exp_list { (vars, Some t, exps) }
  ;

array_size:
  | { "" }
  | i = TINTVAL { string_of_int i }
  ;

var_list:
  | v1 = TIDENTIFIER TCOMMA v2 = var_list { v1::v2 }
  | v = TIDENTIFIER { [v] }
  ;

exp_list:
  | e = exp TCOMMA l = exp_list { e::l }
  | e = exp { [e] }
  ;

//rules for function declarations
fct_args:
  | { [] }
  | args = args_list { args }
  ;

fct_return:
  | { None }
  | t = TIDENTIFIER { Some t }
  ;

args_list:
  | var = TIDENTIFIER t = TIDENTIFIER { [(var, Some t)] }
  | vars = var_list t = TIDENTIFIER { List.map (fun x -> (x, Some t)) vars }
  | vars = var_list t = TIDENTIFIER TCOMMA l = args_list
    { List.append (List.map (fun x -> (x, Some t)) vars) l } //temporary
  | var = TIDENTIFIER t = TIDENTIFIER TCOMMA l = args_list { (var, Some t)::l }
  ;

stm_list:
  | TOPENINGBRACE s = stm_list TCLOSINGBRACE { s }
  | s = stm l = stm_list { s::l }
  | s = stm { [s] }
  | { [] }
  ;

//rules for type declarations
type_decls:
  | t = type_format { [t] }
  | TOPENINGPAR ts = type_formats TCLOSINGPAR { ts }
  ;

type_formats:
  | v1 = type_format v2 = type_formats { v1::v2 }
  | v = type_format { [v] }
  ;

type_format:
  | name = TIDENTIFIER base = TIDENTIFIER { (name, TypeT base) }
  | name = TIDENTIFIER s = stuct_decl { (name, StructT s) }
  ;

stuct_decl:
  | TSTRUCT TOPENINGBRACE v = var_list_list TCLOSINGBRACE { v }
  ;

var_list_list:
  | v1 = var_list t = TIDENTIFIER v2 = var_list_list { (v1, t)::v2 }
  | v1 = var_list TOPENINGSQUARE size = array_size TCLOSINGSQUARE t = TIDENTIFIER v2 = var_list_list
    { (v1, t ^ "[" ^ size ^ "]")::v2 }
  | v = var_list t = TIDENTIFIER { [(v, t)] }
  | v = var_list TOPENINGSQUARE size = array_size TCLOSINGSQUARE t = TIDENTIFIER
    { [(v, t ^ "[" ^ size ^ "]")] }
  ;

//rules for statements and expressions
stm:
  | TPRINT TOPENINGPAR e = exp_list TCLOSINGPAR { { position = $symbolstartpos; value = Print e } }
  | TPRINTLN TOPENINGPAR e = exp_list TCLOSINGPAR { { position = $symbolstartpos; value = Println e } }
  | var = kind a = assign_type e = exp { { position = $symbolstartpos; value = Assign (a, (var, e)) } }
  | TVAR d = var_decls { { position = $symbolstartpos; value = Declaration d } }
  | TTYPE t = type_decls { { position = $symbolstartpos; value = TypeDeclaration t } }
  | v = var_list TCOLEQUAL e = exp_list { { position = $symbolstartpos; value = ShortDeclaration (v, e) } }
  | TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { { position = $symbolstartpos; value =  If (Some cond, s, Some l) } }
  | TFOR cond = exp TCLOSINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (While (Some cond, s)) } }
  | TFOR TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (While (None, s)) } }
  | TFOR init = stm TSEMICOLON cond = exp TSEMICOLON inc = stm TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (For (init, cond, inc, s)) } }
  | var = TIDENTIFIER TDPLUS { { position = $symbolstartpos; value = DoublePlus var } }
  | var = TIDENTIFIER TDMINUS { { position = $symbolstartpos; value = DoubleMinus var } }
  | e = exp { { position = $symbolstartpos; value = ExpStatement e } }
  ;

kind:
  | var = TIDENTIFIER { Variable var }
  | var = TIDENTIFIER TOPENINGSQUARE i = TINTVAL TCLOSINGSQUARE { Array (var, i) }
  | var = TIDENTIFIER TPERIOD member = TIDENTIFIER { Struct (var, member) }
  ;

else_ifs:
  | TELSE TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { [{ position = $symbolstartpos; value = If (Some cond, s, Some l) }] }
  | TELSE TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { [{ position = $symbolstartpos; value = If (None, s, None) }] }
  | { [] }
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
  | TOPENINGPAR e = exp TCLOSINGPAR { e }
  | TIDENTIFIER TOPENINGPAR e = exp_list TCLOSINGPAR { { position = $symbolstartpos; value = FuncCall e } }
  | id = TIDENTIFIER { { position = $symbolstartpos; value = Id id } }
  | i = TINTVAL { { position = $symbolstartpos; value = Int i } }
  | f = TFLOATVAL { { position = $symbolstartpos; value = Float f } }
  | s = TSTRINGVAL { { position = $symbolstartpos; value = String s } }
  | s = TRAWSTRVAL { { position = $symbolstartpos; value = RawStr s } }
  | s = TRUNEVAL { { position = $symbolstartpos; value = Rune s } }
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
  | e1 = exp TMOD e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Mod, (e1, e2)) } }
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
  | e1 = exp TANDHAT e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (AndHat, (e1, e2)) } }
  | e1 = exp TCARET e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Caret, (e1, e2)) } }
  | e1 = exp TBITAND e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (BAnd, (e1, e2)) } }
  | e1 = exp TBITOR e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (BOr, (e1, e2)) } }
  | e1 = exp TAPPEND e2 = exp
    { { position = $symbolstartpos; value = BinaryOp (Append, (e1, e2)) } }
  | TNOT e = exp
    { { position = $symbolstartpos; value = Unaryexp (Not, e) } }
  | TPLUS e = exp
    { { position = $symbolstartpos; value = Unaryexp (UPlus, e) } } %prec TUPLUS
  | TMINUS e = exp
    { { position = $symbolstartpos; value = Unaryexp (UMinus, e) } } %prec TUMINUS
  | TCARET e = exp
    { { position = $symbolstartpos; value = Unaryexp (UCaret, e) } } %prec TUCARET
  ;
