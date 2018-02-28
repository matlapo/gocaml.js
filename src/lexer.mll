{
  open Lexing
  open Parser
  open Utils

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

  let possible_semicolon = ref false
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
let intval       = '0' | (['1'-'9'] digit*)
let octoval      = '0' ['0'-'7']*
let hexval       = '0'('x'|'X') ['0'-'9''A'-'F''a'-'f']*
let floatval     = (digit+ '.' digit+) | ('.' digit+) | (intval '.')
let stringval    = '"' (ws | [^'\\''"'] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\\"" | "\\\\")* '"'
let runeval      = ''' (ws | [^'\\''''] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\'" | "\\\\") '''
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
let hequal    = "^="
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
let andh      = "&^"
let wtf       = "&^="

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
  (* Normal rules *)
  | comment { read lexbuf }
  | mcomment as cnl {
      (* Increase the line count by the number of new lines in the comment *)
      next_line_count (count_new_lines cnl) lexbuf;
      (* If there is at least one newline in the comment and there should be a
         newline, insert the new line *)
      if (count_new_lines cnl) > 0 && !possible_semicolon then
        (possible_semicolon := false; TSEMICOLON)
      else
        (* Otherwise, continue reading the buffer *)
        read lexbuf
    }
  | ws        { read lexbuf }
  | nl        { next_line lexbuf; if !possible_semicolon then (possible_semicolon := false; TSEMICOLON) else read lexbuf }
  | wtf       { possible_semicolon := false; TWTF }
  | print     { possible_semicolon := false; TPRINT }
  | println   { possible_semicolon := false; TPRINTLN }
  | append    { possible_semicolon := false; TAPPEND }
  | var       { possible_semicolon := false; TVAR }
  | percent   { possible_semicolon := false; TMOD }
  | andh      { possible_semicolon := false; TANDHAT }
  | band      { possible_semicolon := false; TBITAND }
  | dgequal   { possible_semicolon := false; TDGEQUAL }
  | smalleq   { possible_semicolon := false; TSMALLEREQ }
  | dsmaller  { possible_semicolon := false; TDSMALLER }
  | dsequal   { possible_semicolon := false; TDSEQUAL }
  | dequal    { possible_semicolon := false; TEQUALS }
  | nequal    { possible_semicolon := false; TNOTEQUAL }
  | land      { possible_semicolon := false; TAND }
  | lor       { possible_semicolon := false; TOR }
  | bor       { possible_semicolon := false; TBITOR }
  | greater   { possible_semicolon := false; TGREATER }
  | smaller   { possible_semicolon := false; TSMALLER }
  | greateq   { possible_semicolon := false; TGREATEREQ }
  | dgreater  { possible_semicolon := false; TDGREATER }
  | pequal    { possible_semicolon := false; TPLUSEQUAL }
  | mequal    { possible_semicolon := false; TMINUSEQUAL }
  | multequal { possible_semicolon := false; TMULTEQUAL }
  | divequal  { possible_semicolon := false; TDIVEQUAL }
  | aequal    { possible_semicolon := false; TANDEQUAL }
  | oequal    { possible_semicolon := false; TOREQUAL }
  | hequal    { possible_semicolon := false; THATEQUAL }
  | perequal  { possible_semicolon := false; TPERCENTEQUAL }
  | if        { possible_semicolon := false; TIF }
  | else      { possible_semicolon := false; TELSE }
  | break     { possible_semicolon := true; TBREAK }
  | case      { possible_semicolon := false; TCASE }
  | chan      { possible_semicolon := false; TCHAN }
  | const     { possible_semicolon := false; TCONST }
  | continue  { possible_semicolon := true; TCONTINUE }
  | default   { possible_semicolon := false; TDEFAULT }
  | defer     { possible_semicolon := false; TDEFER }
  | fall      { possible_semicolon := true; TFALL }
  | for       { possible_semicolon := false; TFOR }
  | func      { possible_semicolon := false; TFUNC }
  | go        { possible_semicolon := false; TGO }
  | goto      { possible_semicolon := false; TGOTO }
  | import    { possible_semicolon := false; TIMPORT }
  | iface     { possible_semicolon := false; TIFACE }
  | map       { possible_semicolon := false; TMAP }
  | package   { possible_semicolon := false; TPACKAGE }
  | range     { possible_semicolon := false; TRANGE }
  | return    { possible_semicolon := true; TRETURN }
  | select    { possible_semicolon := false; TSELECT }
  | struct    { possible_semicolon := false; TSTRUCT }
  | switch    { possible_semicolon := false; TSWITCH }
  | type      { possible_semicolon := false; TTYPE }
  | floatval  { possible_semicolon := true; TFLOATVAL (float_of_string (Lexing.lexeme lexbuf)) }
  | intval    { possible_semicolon := true; TINTVAL (Int64.of_string (Lexing.lexeme lexbuf)) }
  | stringval { possible_semicolon := true; TSTRINGVAL (Lexing.lexeme lexbuf) }
  | rawstrval { possible_semicolon := true; TRAWSTRVAL (Lexing.lexeme lexbuf) }
  | runeval   { possible_semicolon := true; TRUNEVAL (Lexing.lexeme lexbuf) }
  | hexval    { possible_semicolon := true; TINTVAL (int_of_hex (Lexing.lexeme lexbuf)) }
  | octoval   { possible_semicolon := true; TINTVAL (int_of_oct (Lexing.lexeme lexbuf)) }
  | dplus     { possible_semicolon := true; TDPLUS }
  | dminus    { possible_semicolon := true; TDMINUS }
  | plus      { possible_semicolon := false; TPLUS }
  | minus     { possible_semicolon := false; TMINUS }
  | times     { possible_semicolon := false; TTIMES }
  | div       { possible_semicolon := false; TDIV }
  | not       { possible_semicolon := false; TNOT }
  | larrow    { possible_semicolon := false; TLEFTARROW }
  | colequal  { possible_semicolon := false; TCOLEQUAL }
  | colon     { possible_semicolon := false; TCOLON }
  | caret     { possible_semicolon := false; TCARET }
  | semicolon { possible_semicolon := false; TSEMICOLON }
  | comma     { possible_semicolon := false; TCOMMA }
  | period    { possible_semicolon := false; TPERIOD }
  | dots      { possible_semicolon := false; TDOTS }
  | osquare   { possible_semicolon := false; TOPENINGSQUARE }
  | csquare   { possible_semicolon := true; TCLOSINGSQUARE }
  | opar      { possible_semicolon := false; TOPENINGBRACE }
  | cpar      { possible_semicolon := true; TCLOSINGBRACE }
  | oparent   { possible_semicolon := false; TOPENINGPAR }
  | cparent   { possible_semicolon := true; TCLOSINGPAR }
  | sequal    { possible_semicolon := false; TASSIGN }
  | ident     { possible_semicolon := true; TIDENTIFIER (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
