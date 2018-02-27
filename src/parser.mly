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

// Helpers
identifier_with_parenthesis: (* TODO: add this everywhere *)
  | t = TIDENTIFIER { t }
  | TOPENINGPAR t = identifier_with_parenthesis TCLOSINGPAR { t }
  ;

identifier_list:
  | v1 = TIDENTIFIER TCOMMA v2 = identifier_list { v1::v2 }
  | v = TIDENTIFIER { [v] }
  ;

// Start of program
prog:
  | p = package EOF { (p, []) }
  | p = package ds = decls EOF { (p, ds) }
  ;

package:
  | TPACKAGE id = TIDENTIFIER TSEMICOLON { id }
  ;

decls:
  | d1 = decl_node TSEMICOLON d2 = decls { d1::d2 }
  | d = decl_node TSEMICOLON { [d] }
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
  | TOPENINGPAR ds = var_formats TCLOSINGPAR { ds }
  | d = var_format { [d] }
  ;

var_formats:
  | v1 = var_format TSEMICOLON v2 = var_formats { v1::v2 }
  | { [] }
  ;

var_format:
  | vars = identifier_list t = type_ref { (vars, Some t, []) }
  | vars = identifier_list TASSIGN exps = exp_list { (vars, None, exps) }
  | vars = identifier_list t = type_ref TASSIGN exps = exp_list { (vars, Some t, exps) }
  ;

type_ref:
  | TOPENINGSQUARE i = TINTVAL TCLOSINGSQUARE base = identifier_with_parenthesis { ArrayR (base, i) }
  | TOPENINGSQUARE TCLOSINGSQUARE base = identifier_with_parenthesis { SliceR base }
  | base = identifier_with_parenthesis { TypeR base }
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
  | t = type_ref { Some t }
  ;

args_list:
  | var = TIDENTIFIER t = type_ref { [(var, Some t)] }
  | vars = identifier_list t = type_ref { List.map (fun x -> (x, Some t)) vars }
  | vars = identifier_list t = type_ref TCOMMA l = args_list
    { List.append (List.map (fun x -> (x, Some t)) vars) l } //temporary
  | var = TIDENTIFIER t = type_ref TCOMMA l = args_list { (var, Some t)::l }
  ;

stm_list:
  | TOPENINGBRACE s = stm_list TCLOSINGBRACE { s }
  | s = stm TSEMICOLON l = stm_list { s::l }
  | { [] }
  ;

//rules for type declarations
type_decls:
  | t = type_format { [t] }
  | TOPENINGPAR ts = type_formats TCLOSINGPAR { ts }
  ;

type_formats:
  | v1 = type_format TSEMICOLON v2 = type_formats { v1::v2 }
  | { [] }
  ;

type_format:
  | name = TIDENTIFIER t = type_def { (name, t) }
  ;

type_def:
  | TOPENINGSQUARE i = TINTVAL TCLOSINGSQUARE base = identifier_with_parenthesis { ArrayT (base, i) }
  | TOPENINGSQUARE TCLOSINGSQUARE base = identifier_with_parenthesis { SliceT base }
  | TSTRUCT TOPENINGBRACE s = type_def_list TCLOSINGBRACE { StructT s }
  | base = identifier_with_parenthesis { TypeT base }
  ;

type_def_list:
  | ids = identifier_list t = type_def TSEMICOLON tdl = type_def_list { (ids, t)::tdl }
  | ids = identifier_list t = type_def TSEMICOLON { [(ids, t)] }
  ;

//rules for statements and expressions
stm:
  | TPRINT TOPENINGPAR e = exp_list TCLOSINGPAR { { position = $symbolstartpos; value = Print e } }
  | TPRINTLN TOPENINGPAR e = exp_list TCLOSINGPAR { { position = $symbolstartpos; value = Println e } }
  | TVAR d = var_decls { { position = $symbolstartpos; value = Declaration d } }
  | TTYPE t = type_decls { { position = $symbolstartpos; value = TypeDeclaration t } }
  | TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { { position = $symbolstartpos; value =  If (None, Some cond, s, Some l) } }
  | TIF simp = simpleStm TSEMICOLON cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { { position = $symbolstartpos; value =  If (Some simp, Some cond, s, Some l) } }
  | TFOR cond = exp TCLOSINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (While (Some cond, s)) } }
  | TFOR TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (While (None, s)) } }
  | TFOR init = simpleStm TSEMICOLON cond = exp TSEMICOLON inc = simpleStm TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Loop (For (init, cond, inc, s)) } }
  | TRETURN e = exp { { position = $symbolstartpos; value = Return (Some e) } }
  | TRETURN { { position = $symbolstartpos; value = Return None } }
  | simple = simpleStm { { position = $symbolstartpos; value = Simple simple } }
  | TSWITCH e = exp TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Switch (None, Some e, cases) } }
  | TSWITCH TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Switch (None, None, cases) } }
  | TSWITCH s = simpleStm TSEMICOLON e = exp TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Switch (Some s, Some e, cases) } }
  | TSWITCH s = simpleStm TSEMICOLON TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { { position = $symbolstartpos; value = Switch (Some s, None, cases) } }
  | TBREAK { { position = $symbolstartpos; value = Break } }
  | TCONTINUE { { position = $symbolstartpos; value = Continue } }
  ;

case_list:
  | c1 = case c2 = case_list { c1::c2 }
  | c = case  { [c] }
  ;

case:
  | TCASE e = exp TCOLON s = stm_list { (Some e, s) }
  | TDEFAULT TCOLON s = stm_list { (None, s) }
  ;

simpleStm:
  | e = exp { { position = $symbolstartpos; value = ExpStatement e } }
  | var = TIDENTIFIER TDPLUS { { position = $symbolstartpos; value = DoublePlus var } }
  | var = TIDENTIFIER TDMINUS { { position = $symbolstartpos; value = DoubleMinus var } }
  | var = kind a = assign_type e = exp { { position = $symbolstartpos; value = Assign (a, (var, e)) } }
  | v = identifier_list TCOLEQUAL e = exp_list { { position = $symbolstartpos; value = ShortDeclaration (v, e) } }
  |  { { position = $symbolstartpos; value = Empty } }
  ;

kind:
  | var = kind_elem TPERIOD vars = kind { var::vars }
  | var = kind_elem { [var] }

kind_elem:
  | var = TIDENTIFIER { Variable var }
  | var = TIDENTIFIER TOPENINGSQUARE i = TINTVAL TCLOSINGSQUARE { Array (var, i) }
  ;

else_ifs:
  | TELSE TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { [{ position = $symbolstartpos; value = If (None, Some cond, s, Some l) }] }
  | TELSE TIF simp = simpleStm TSEMICOLON cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { [{ position = $symbolstartpos; value = If (Some simp, Some cond, s, Some l) }] }
  | TELSE TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { [{ position = $symbolstartpos; value = If (None, None, s, None) }] }
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
  | name = TIDENTIFIER TOPENINGPAR e = exp_list TCLOSINGPAR
    { { position = $symbolstartpos; value = FuncCall (name, e) } }
  | TAPPEND TOPENINGPAR e1 = exp TCOMMA e2 = exp TCLOSINGPAR
    { { position = $symbolstartpos; value = Append (e1, e2) } }
  | id = kind { { position = $symbolstartpos; value = Id id } }
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
  | TNOT e = exp
    { { position = $symbolstartpos; value = Unaryexp (Not, e) } }
  | TPLUS e = exp
    { { position = $symbolstartpos; value = Unaryexp (UPlus, e) } } %prec TUPLUS
  | TMINUS e = exp
    { { position = $symbolstartpos; value = Unaryexp (UMinus, e) } } %prec TUMINUS
  | TCARET e = exp
    { { position = $symbolstartpos; value = Unaryexp (UCaret, e) } } %prec TUCARET
  ;
