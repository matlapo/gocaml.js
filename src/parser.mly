%{
  open Ast
  open Utils
%}
%token TVAR
%token TIF
%token <string> TIDENTIFIER
%token TPRINT
%token TPRINTLN
%token TAPPEND
%token TELSE
%token <int64> TINTVAL
%token <float> TFLOATVAL
%token <string> TSTRINGVAL
%token <string> TRAWSTRVAL
%token <string> TRUNEVAL
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
%left TPERIOD TOPENINGPAR TOPENINGSQUARE

%start <Ast.program> prog
%%

// Reserved keyword (we don't use them anywhere)
keyword:
  | TIMPORT    {}
  | TSELECT    {}
  | TCHAN      {}
  | TLEFTARROW {}
  | TCONST     {}
  | TDEFER     {}
  | TFALL      {}
  | TGO        {}
  | TGOTO      {}
  | TIFACE     {}
  | TMAP       {}
  | TRANGE     {}
  | TDOTS      {}
  | TDEFAULT   {}
  ;

// Helpers
identifier_with_parenthesis:
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

// ########################################
// ### rules for top-level declarations ###
// ########################################

decls:
  | d1 = decl_type TSEMICOLON d2 = decls { Position { position = $symbolstartpos; value = d1 }::d2 }
  | d = decl_type TSEMICOLON { [ Position { position = $symbolstartpos; value = d }] }
  ;

//top-level declarations
decl_type:
  | TVAR vars = var_decls { Var vars }
  | TFUNC name = TIDENTIFIER TOPENINGPAR args = fct_args TCLOSINGPAR ret = fct_return TOPENINGBRACE body = stm_list TCLOSINGBRACE
    { Fct (name, args, ret, body) }
  | TTYPE t = type_decls { Type t }
  ;

// #######################################
// ### rules for variable declarations ###
// #######################################

// Support distributed declarations
var_decls:
  | TOPENINGPAR ds = var_formats TCLOSINGPAR { ds }
  | d = var_format { [d] }
  ;

// List of variables declaration in a distributed declaration
var_formats:
  | v1 = var_format TSEMICOLON v2 = var_formats { v1::v2 }
  | { [] }
  ;

// List of variables declaration in a distributed declaration
var_format:
  | vars = identifier_list t = gotype { (vars, Some t, []) }
  | vars = identifier_list TASSIGN exps = exp_list { (vars, None, exps) }
  | vars = identifier_list t = gotype TASSIGN exps = exp_list { (vars, Some t, exps) }
  ;

// #######################################
// ### rules for function declarations ###
// #######################################
fct_args:
  | { [] }
  | args = args_list { args }
  ;

// Matches the return type of a function
fct_return:
  | { Void }
  | t = gotype { NonVoid t }
  ;

args_list:
  | a = arg TCOMMA l = args_list { a@l }
  | a = arg { a }

// Takes a list of arguments having the same type (Ex: fun(a, b int)) and turning it into fun(a int, b int)
arg:
  | vars = identifier_list t = gotype { List.map (fun x -> (x, t)) vars }
  ;


// ###################################
// ### rules for type declarations ###
// ###################################

// Support distributed declarations
type_decls:
  | t = type_format { [t] }
  | TOPENINGPAR ts = type_formats TCLOSINGPAR { ts }
  ;

// Type declaration list in a distributed declaration
type_formats:
  | v1 = type_format TSEMICOLON v2 = type_formats { v1::v2 }
  | { [] }
  ;

// Type declaration in a distributed declaration
type_format:
  | name = TIDENTIFIER t = gotype { (name, t) }
  ;

// Definition of a type
gotype:
  | TOPENINGPAR t = gotype TCLOSINGPAR { t }
  | t = gotype_without_parenthesis { t }

gotype_without_parenthesis:
  | id = TIDENTIFIER { Defined id }
  | TOPENINGSQUARE i = TINTVAL TCLOSINGSQUARE t = gotype { Array (t, i) }
  | TOPENINGSQUARE TCLOSINGSQUARE t = gotype { Slice t }
  | TSTRUCT TOPENINGBRACE m = struct_member_def_list TCLOSINGBRACE { Struct m }
  ;

struct_member_def_list:
  | d1 = struct_member_def TSEMICOLON d2 = struct_member_def_list { d1@d2 }
  | { [] }

struct_member_def:
  | ids = identifier_list t = gotype { List.map (fun id -> (id, t)) ids }
  ;

// ############################
// ### rules for statements ###
// ############################
stm_list:
  | s = stm TSEMICOLON l = stm_list{ s::l }
  | s = stm { [s] }
  ;

stm:
  | TOPENINGBRACE ss = stm_list TCLOSINGBRACE { Position { position = $symbolstartpos; value = Block ss } }
  | TPRINT TOPENINGPAR e = exp_list TCLOSINGPAR { Position { position = $symbolstartpos; value = Print e } }
  | TPRINTLN TOPENINGPAR e = exp_list TCLOSINGPAR { Position { position = $symbolstartpos; value = Println e } }
  | TVAR d = var_decls { Position { position = $symbolstartpos; value = Declaration d } }
  | TTYPE t = type_decls { Position { position = $symbolstartpos; value = TypeDeclaration t } }
  | TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { Position { position = $symbolstartpos; value =  If (Position { position = $symbolstartpos; value = Empty}, cond, s, l) } }
  | TIF simp = simpleStm TSEMICOLON cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { Position { position = $symbolstartpos; value =  If (simp, cond, s, l) } }
  | TFOR cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Loop (While (Some cond, s)) } }
  | TFOR TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Loop (While (None, s)) } }
  | TFOR init = simpleStm TSEMICOLON cond = exp TSEMICOLON inc = simpleStm TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Loop (For (init, Some cond, inc, s)) } }
  | TFOR init = simpleStm TSEMICOLON TSEMICOLON inc = simpleStm TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Loop (For (init, None, inc, s)) } }
  | TRETURN e = exp { Position { position = $symbolstartpos; value = Return (Some e) } }
  | TRETURN { Position { position = $symbolstartpos; value = Return None } }
  | simple = simpleStm { Position { position = $symbolstartpos; value = Simple simple } }
  | TSWITCH e = exp TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Switch (Position { position = $symbolstartpos; value = Empty}, Some e, cases) } }
  | TSWITCH TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Switch (Position { position = $symbolstartpos; value = Empty}, None, cases) } }
  | TSWITCH s = simpleStm TSEMICOLON e = exp TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Switch (s, Some e, cases) } }
  | TSWITCH s = simpleStm TSEMICOLON TOPENINGBRACE cases = case_list TCLOSINGBRACE
    { Position { position = $symbolstartpos; value = Switch (s, None, cases) } }
  | TBREAK { Position { position = $symbolstartpos; value = Break } }
  | TCONTINUE { Position { position = $symbolstartpos; value = Continue } }
  ;

// Defines a list of cases in a switch
case_list:
  | l = case_list_nonempty { l }
  | { [] }
  ;

case_list_nonempty:
  | c1 = case c2 = case_list_nonempty { c1::c2 }
  | c = case  { [c] }
  ;

// Defines a switch case
case:
  | TCASE e = exp TCOLON s = stm_list { (Some [e], s) }
  | TCASE e = exp TCOMMA es = exp_list TCOLON s = stm_list { (Some (e::es), s) }
  | TDEFAULT TCOLON s = stm_list { (None, s) }
  ;

// Defines a golang simplestm (see https://golang.org/ref/spec#SimpleStmt)
simpleStm:
  | e = exp { Position { position = $symbolstartpos; value = ExpStatement e } }
  | e = exp TDPLUS { Position { position = $symbolstartpos; value = DoublePlus e } }
  | e = exp TDMINUS { Position { position = $symbolstartpos; value = DoubleMinus e } }
  | var = exp_list TASSIGN e = exp_list { Position { position = $symbolstartpos; value = Assign (var, e) } }
  | var = exp a = assign_type e = exp
    { Position { position = $symbolstartpos; value = Assign ([var], [ var ]) } }
  | v = exp_list TCOLEQUAL e = exp_list { Position { position = $symbolstartpos; value = ShortDeclaration (v, e) } }
  | { Position { position = $symbolstartpos; value = Empty } }
  ;

else_ifs:
  | TELSE TIF cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { Some [ Position { position = $symbolstartpos; value = If (Position { position = $symbolstartpos; value = Empty}, cond, s, l) }] }
  | TELSE TIF simp = simpleStm TSEMICOLON cond = exp TOPENINGBRACE s = stm_list TCLOSINGBRACE l = else_ifs
    { Some [ Position { position = $symbolstartpos; value = If (simp, cond, s, l) }] }
  | TELSE TOPENINGBRACE s = stm_list TCLOSINGBRACE
    { Some s }
  | { None }
  ;

assign_type:
  | TPLUSEQUAL { Plus }
  | TMINUSEQUAL { Minus }
  | TMULTEQUAL { Times }
  | TDIVEQUAL { Div }
  | TANDEQUAL { BAnd }
  | TOREQUAL { BOr }
  | THATEQUAL { Caret }
  | TPERCENTEQUAL { Mod }
  | TWTF { AndHat }
  | TDGEQUAL { DGreater }
  | TDSEQUAL { DSmaller }
  ;

// #############################
// ### rules for expressions ###
// #############################
exp_list:
  | l = exp_list_nonempty { l }
  | { [] }
  ;

exp_list_nonempty:
  | e = exp TCOMMA l = exp_list_nonempty { e::l }
  | e = exp { [e] }
  ;

exp:
  | TOPENINGPAR e = exp TCLOSINGPAR { e }
  | name = TIDENTIFIER TOPENINGPAR e = exp_list TCLOSINGPAR
    { Position { position = $symbolstartpos; value = FuncCall (name, e) } }
  | TAPPEND TOPENINGPAR e1 = exp TCOMMA e2 = exp TCLOSINGPAR
    { Position { position = $symbolstartpos; value = Append (e1, e2) } }
  | id = TIDENTIFIER { Position { position = $symbolstartpos; value = Id id } }
  | e = exp TOPENINGSQUARE i = exp TCLOSINGSQUARE { Position { position = $symbolstartpos; value = Indexing (e, i) } }
  | e = exp TPERIOD s = TIDENTIFIER  { Position { position = $symbolstartpos; value = Selection (e, s) } }
  | i = TINTVAL { Position { position = $symbolstartpos; value = Int i } }
  | f = TFLOATVAL { Position { position = $symbolstartpos; value = Float f } }
  | s = TSTRINGVAL { Position { position = $symbolstartpos; value = String s } }
  | s = TRAWSTRVAL { Position { position = $symbolstartpos; value = RawStr s } }
  | s = TRUNEVAL { Position { position = $symbolstartpos; value = Rune s } }
  | e1 = exp TPLUS e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Plus, (e1, e2)) } }
  | e1 = exp TMINUS e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Minus, (e1, e2)) } }
  | e1 = exp TTIMES e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Times, (e1, e2)) } }
  | e1 = exp TDIV e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Div, (e1, e2)) } }
  | e1 = exp TMOD e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Mod, (e1, e2)) } }
  | e1 = exp TEQUALS e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Equals, (e1, e2)) } }
  | e1 = exp TNOTEQUAL e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (NotEquals, (e1, e2)) } }
  | e1 = exp TAND e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (And, (e1, e2)) } }
  | e1 = exp TOR e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Or, (e1, e2)) } }
  | e1 = exp TSMALLER e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Smaller, (e1, e2)) } }
  | e1 = exp TGREATER e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Greater, (e1, e2)) } }
  | e1 = exp TSMALLEREQ e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (SmallerEq, (e1, e2)) } }
  | e1 = exp TGREATEREQ e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (GreaterEq, (e1, e2)) } }
  | e1 = exp TDSMALLER e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (DSmaller, (e1, e2)) } }
  | e1 = exp TDGREATER e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (DGreater, (e1, e2)) } }
  | e1 = exp TANDHAT e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (AndHat, (e1, e2)) } }
  | e1 = exp TCARET e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (Caret, (e1, e2)) } }
  | e1 = exp TBITAND e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (BAnd, (e1, e2)) } }
  | e1 = exp TBITOR e2 = exp
    { Position { position = $symbolstartpos; value = BinaryOp (BOr, (e1, e2)) } }
  | TNOT e = exp
    { Position { position = $symbolstartpos; value = Unaryexp (Not, e) } }
  | TPLUS e = exp
    { Position { position = $symbolstartpos; value = Unaryexp (UPlus, e) } } %prec TUPLUS
  | TMINUS e = exp
    { Position { position = $symbolstartpos; value = Unaryexp (UMinus, e) } } %prec TUMINUS
  | TCARET e = exp
    { Position { position = $symbolstartpos; value = Unaryexp (UCaret, e) } } %prec TUCARET
  ;
