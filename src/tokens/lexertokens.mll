{
  open Lexing
  open Parsertokens

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let rec next_line_count c lexbuf = match c with
    | 0 -> ()
    | _ -> next_line lexbuf; next_line_count (c - 1) lexbuf

  let inject_semicolon lexbuf =
    lexbuf.lex_buffer <- Utils.bytes_insert_byte lexbuf.lex_buffer ';' lexbuf.lex_curr_pos;
    lexbuf.lex_buffer_len <- (lexbuf.lex_buffer_len + 1)

  let count_new_lines str = (String.split_on_char '\n' str |> List.length) - 1
}

(* helper regex *)
let digit     = ['0'-'9']

(* invisiable characters *)
let ws        = [' ' '\t']
let nl        = ['\n']

(* keywords *)
let var       = "var"
let if        = "if"
let else      = "else"
let break     = "break"
let case      = "case"
let chan      = "chan"
let const     = "const"
let continue  = "continue"
let default   = "default"
let defer     = "defer"
let fall      = "fallthrough"
let for       = "for"
let func      = "func"
let go        = "go"
let goto      = "goto"
let import    = "import"
let iface     = "interface"
let map       = "map"
let package   = "package"
let range     = "range"
let return    = "return"
let select    = "select"
let struct    = "struct"
let switch    = "switch"
let type      = "type"

(* identifiers *)
let ident     = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

(* functions *)
let print     = "print"
let println   = "println"
let append    = "append"

(* literals *)
let intval       = '0' | ['1'-'9'] digit*
let octoval      = '0' digit*
let hexval       = '0''x' (digit | ['a'-'f'])*
let floatval     = intval '.' digit+ | '.' digit+ | digit+ '.'
let stringval    = '"' (ws | ['a'-'z''A'-'Z''0'-'9''~''@''#''$''%''^''&''*''-''+''/''\'''`''<''>''=''|''\'''.'','';'':''!''?''{''}''['']''('')'] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\'" | "\\\"" | "\\\\")* '"'
let runeval      = ''' (ws | ['a'-'z''A'-'Z''0'-'9''~''@''#''$''%''^''&''*''-''+''/''`''<''>''=''|''.'','';'':''!''?''{''}''['']''('')'] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\'" | "\\\\") '''
let rawstrval    = '`' [^'`']* '`'

(* operations *)
let plus      = "+"
let minus     = "-"
let times     = "*"
let div       = "/"
let not       = "!"
let caret     = "^"
let percent   = "%"
let dequal    = "=="
let sequal    = "="
let nequal    = "!="
let land      = "&&"
let lor       = "||"
let band      = "&"
let bor       = "|"
let pequal    = "+="
let mequal    = "-="
let divequal  = "/="
let multequal = "*="
let aequal    = "&="
let oequal    = "|="
let hequal    = "ˆ="
let perequal  = "%="
let smaller   = "<"
let greater   = ">"
let smalleq   = "<="
let greateq   = ">="
let dsmaller  = "<<"
let dgreater  = ">>"
let larrow    = "<-"
let dsequal   = "<<="
let dgequal   = ">>="
let dplus     = "++"
let dminus    = "--"
let colequal  = ":="
let andh      = "&ˆ"
let wtf       = "&ˆ="

(* others *)
let comment   = "//" [^'\n']*
let mcomment  = "/*" ([^'*']*('*'[^'/'])?)* "*/"
let colon     = ":"
let semicolon = ";"
let comma     = ","
let period    = "."
let dots      = "..."
let opar      = "{"
let cpar      = "}"
let oparent   = "("
let cparent   = ")"
let osquare   = "["
let csquare   = "]"

let scnl = ws* (comment|mcomment)* nl

rule read =
  parse
  (* Semicolon injection rules *)
  | (ident as id) (scnl as cnl) { inject_semicolon lexbuf; next_line_count (count_new_lines cnl) lexbuf; TIDENTIFIER id }
  | (intval as i) (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TINTVAL (int_of_string (i))}
  | (floatval as f) (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TFLOATVAL (float_of_string f)}
  | (runeval as r) (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TRUNEVAL r}
  | (stringval as s) (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TSTRINGVAL s}
  | (rawstrval as s) (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TRAWSTRVAL s}
  | break (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TBREAK }
  | continue (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TCONTINUE }
  | fall (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TFALL }
  | return (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TRETURN }
  | dplus (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TDPLUS }
  | dminus (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TDMINUS }
  | cparent (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TCLOSINGPAR }
  | csquare (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TCLOSINGSQUARE }
  | cpar (scnl as cnl) { next_line_count (count_new_lines cnl) lexbuf; inject_semicolon lexbuf; TCLOSINGBRACE }
  (* Normal rules *)
  | comment nl { next_line lexbuf; read lexbuf }
  | (mcomment nl) as cnl  { next_line_count (count_new_lines cnl) lexbuf; read lexbuf }
  | ws        { read lexbuf }
  | nl        { next_line lexbuf; read lexbuf }
  | print     { TPRINT }
  | println   { TPRINTLN }
  | append    { TAPPEND }
  | var       { TVAR }
  | if        { TIF }
  | else      { TELSE }
  | break     { TBREAK }
  | case      { TCASE }
  | chan      { TCHAN }
  | const     { TCONST }
  | continue  { TCONTINUE }
  | default   { TDEFAULT }
  | defer     { TDEFER }
  | fall      { TFALL }
  | for       { TFOR }
  | func      { TFUNC }
  | go        { TGO }
  | goto      { TGOTO }
  | import    { TIMPORT }
  | iface     { TIFACE }
  | map       { TMAP }
  | package   { TPACKAGE }
  | range     { TRANGE }
  | return    { TRETURN }
  | select    { TSELECT }
  | struct    { TSTRUCT }
  | switch    { TSWITCH }
  | type      { TTYPE }
  | intval    { TINTVAL (int_of_string (Lexing.lexeme lexbuf)) }
  | floatval  { TFLOATVAL (float_of_string (Lexing.lexeme lexbuf)) }
  | stringval { TSTRINGVAL (Lexing.lexeme lexbuf) }
  | rawstrval { TRAWSTRVAL (Lexing.lexeme lexbuf) }
  | runeval   { TRUNEVAL (Lexing.lexeme lexbuf) }
  | hexval    { THEXVAL (Lexing.lexeme lexbuf) }
  | octoval   { TOCTOVAL (Lexing.lexeme lexbuf) }
  | plus      { TPLUS }
  | minus     { TMINUS }
  | times     { TTIMES }
  | div       { TDIV }
  | not       { TNOT }
  | percent   { TMOD }
  | caret     { TCARET }
  | dequal    { TEQUALS }
  | sequal    { TASSIGN }
  | nequal    { TNOTEQUAL }
  | land      { TAND }
  | lor       { TOR }
  | band      { TBITAND }
  | bor       { TBITOR }
  | pequal    { TPLUSEQUAL }
  | mequal    { TMINUSEQUAL }
  | multequal { TMULTEQUAL }
  | divequal  { TDIVEQUAL }
  | aequal    { TANDEQUAL }
  | oequal    { TOREQUAL }
  | hequal    { THATEQUAL }
  | perequal  { TPERCENTEQUAL }
  | greater   { TGREATER }
  | smaller   { TSMALLER }
  | greateq   { TGREATEREQ }
  | smalleq   { TSMALLEREQ }
  | dsmaller  { TDSMALLER }
  | dgreater  { TDGREATER }
  | larrow    { TLEFTARROW }
  | dgequal   { TDGEQUAL }
  | dsequal   { TDSEQUAL }
  | dplus     { TDPLUS }
  | dminus    { TDMINUS }
  | colequal  { TCOLEQUAL }
  | andh      { TANDHAT }
  | wtf       { TWTF }
  | colon     { TCOLON }
  | semicolon { TSEMICOLON }
  | comma     { TCOMMA }
  | period    { TPERIOD }
  | dots      { TDOTS }
  | osquare   { TOPENINGSQUARE }
  | csquare   { TCLOSINGSQUARE }
  | opar      { TOPENINGBRACE }
  | cpar      { TCLOSINGBRACE }
  | oparent   { TOPENINGPAR }
  | cparent   { TCLOSINGPAR }
  | ident     { TIDENTIFIER (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
