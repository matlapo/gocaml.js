%{
%}
%token TVAR
%token TWHILE
%token TIF
%token <string> TIDENTIFIER
%token TINT
%token TFLOAT
%token TSTRING
%token TBOOLEAN
%token TREAD
%token TPRINT
%token TELSE
%token TTRUE
%token TFALSE
%token <int> TINTVAL
%token <float> TFLOATVAL
%token <string> TSTRINGVAL
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
%token TCOLON
%token TSEMICOLON
%token TOPENINGBRACE
%token TCLOSINGBRACE
%token TOPENINGBRACKET
%token TCLOSINGBRACKET
%token EOF

%start <string> prog
%%

prog:
  | EOF         { "" }
  | v = stmt    { v }
  ;

stmt:
  | v1 = value v2 = stmt  { v1 ^ "\n" ^ v2 }
  | v = value EOF         { v }

value:
  | TPRINT            { "TPRINT" }
  | TREAD             { "TREAD" }
  | TVAR              { "TVAR" }
  | TWHILE            { "TWHILE" }
  | TIF               { "TIF" }
  | TELSE             { "TELSE" }
  | TTRUE             { "TTRUE" }
  | TFALSE            { "TFALSE" }
  | i = TINTVAL       { "TINTVAL(" ^ string_of_int i ^ ")"  }
  | f = TFLOATVAL     { "TFLOATVAL(" ^ string_of_float f ^ ")" }
  | s = TSTRINGVAL    { "TSTRINGVAL(" ^ s ^ ")" }
  | TPLUS             { "TPLUS" }
  | TMINUS            { "TMINUS" }
  | TTIMES            { "TTIMES" }
  | TDIV              { "TDIV" }
  | TNOT              { "TNOT" }
  | TEQUALS           { "TEQUALS" }
  | TASSIGN           { "TASSIGN" }
  | TNOTEQUAL         { "TNOTEQUAL" }
  | TAND              { "TAND" }
  | TOR               { "TOR" }
  | TCOLON            { "TCOLON" }
  | TSEMICOLON        { "TSEMICOLON" }
  | TOPENINGBRACE     { "TOPENINGBRACE" }
  | TCLOSINGBRACE     { "TCLOSINGBRACE" }
  | TOPENINGBRACKET   { "TOPENINGBRACKET" }
  | TCLOSINGBRACKET   { "TCLOSINGBRACKET" }
  | TINT              { "TINT" }
  | TFLOAT            { "TFLOAT" }
  | TSTRING           { "TSTRING" }
  | TBOOLEAN          { "TBOOLEAN" }
  | id = TIDENTIFIER  { "TIDENTIFIER(" ^ id ^ ")" }
  ;
