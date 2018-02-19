{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* helper regex *)
let digit     = ['0'-'9']

(* invisiable characters *)
let ws        = [' ' '\t']
let nl        = ['\n']

(* keywords *)
let var       = "var"
let while     = "while"
let if        = "if"
let else      = "else"

(* identifiers *)
let ident     = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

(* types *)
let int       = "int"
let boolean   = "boolean"
let float     = "float"
let string    = "string"

(* functions *)
let print     = "print"
let read      = "read"

(* literals *)
let btrue        = "TRUE"
let bfalse       = "FALSE"
let intval       = '0' | ['1'-'9'] digit*
let floatval     = intval '.' digit digit*
let stringval    = '"' (ws | ['a'-'z''A'-'Z''0'-'9''~''@''#''$''%''^''&''*''-''+''/''\'''`''<''>''=''|''\'''.'','';'':''!''?''{''}''['']''('')'] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\\"" | "\\\\" )* '"'

(* operations *)
let plus      = "+"
let minus     = "-"
let times     = "*"
let div       = "/"
let not       = "!"
let dequal    = "=="
let sequal    = "="
let nequal    = "!="
let land      = "&&"
let lor       = "||"

(* others *)
let comment   = "//" [^'\n']* nl?
let colon     = ":"
let semicolon = ";"
let obracket  = "{"
let cbracket  = "}"
let oparent  = "("
let cparent  = ")"

rule read =
  parse
  | ws        { read lexbuf }
  | nl        { next_line lexbuf; read lexbuf }
  | print     { TPRINT }
  | read      { TREAD }
  | int       { TINT }
  | float     { TFLOAT }
  | string    { TSTRING }
  | boolean   { TBOOLEAN }
  | var       { TVAR }
  | while     { TWHILE }
  | if        { TIF }
  | else      { TELSE }
  | btrue     { TTRUE }
  | bfalse    { TFALSE }
  | intval    { TINTVAL (int_of_string (Lexing.lexeme lexbuf))}
  | floatval  { TFLOATVAL (float_of_string (Lexing.lexeme lexbuf))}
  | stringval { TSTRINGVAL (Lexing.lexeme lexbuf)}
  | plus      { TPLUS }
  | minus     { TMINUS }
  | times     { TTIMES }
  | div       { TDIV }
  | not       { TNOT }
  | dequal    { TEQUALS }
  | sequal    { TASSIGN }
  | nequal    { TNOTEQUAL }
  | land      { TAND }
  | lor       { TOR }
  | colon     { TCOLON }
  | semicolon { TSEMICOLON }
  | obracket  { TOPENINGBRACE }
  | cbracket  { TCLOSINGBRACE }
  | oparent   { TOPENINGBRACKET }
  | cparent   { TCLOSINGBRACKET }
  | ident     { TIDENTIFIER (Lexing.lexeme lexbuf) }
  | comment   { next_line lexbuf; read lexbuf }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
