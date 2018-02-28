open Printf
open Lexing

(* prints the error message msg with the associate lexer buffer *)
let print_error lb msg =
  let pos = lb.lex_curr_p in
  let line = pos.pos_lnum in
  let lcur = pos.pos_cnum - pos.pos_bol + 1 in
    fprintf stderr "%s (%i:%i)\n" msg line lcur

let scan input =
  let lexer_buffer = Lexing.from_channel input in
  try
    let _ = Parsertokens.prog Lexertokens.read lexer_buffer in
      print_string "OK\n";
      exit 0;
  with Lexertokens.SyntaxError msg ->
    print_error lexer_buffer ("Error: " ^ msg);
    exit 1

let tokens input =
  let lexer_buffer = Lexing.from_channel input in
  try
    let tokens = Parsertokens.prog Lexertokens.read lexer_buffer in
      print_string (tokens ^ "\n");
      exit 0;
  with
  | Lexertokens.SyntaxError msg ->
    print_error lexer_buffer ("Error: " ^ msg);
    exit 1
  | Parsertokens.Error ->
    let token = Lexing.lexeme lexer_buffer in
    print_error lexer_buffer ("Error: Unexpected " ^ token);
    exit 1

let parse input =
  let lexer_buffer = Lexing.from_channel input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.illegal_blanks ast with
      | "" ->
        print_string "OK\n";
        exit 0;
      | _ as w ->
        print_error lexer_buffer w;
        exit 1;
  with
  | Lexer.SyntaxError msg ->
    print_error lexer_buffer ("Error: " ^ msg);
    exit 1
  | Parser.Error ->
    let token = Lexing.lexeme lexer_buffer in
    print_error lexer_buffer ("Error: Unexpected " ^ token);
    exit 1

let pretty input =
  let lexer_buffer = Lexing.from_channel input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.illegal_blanks ast with
      | "" ->
        Pretty.pretty_print ast;
        print_newline ();
        exit 0;
      | _ as w ->
        print_error lexer_buffer w;
        exit 1;
  with
  | Lexer.SyntaxError msg ->
    print_error lexer_buffer ("Error: " ^ msg);
    exit 1
  | Parser.Error ->
    let token = Lexing.lexeme lexer_buffer in
    print_error lexer_buffer ("Error: Unexpected " ^ token);
    exit 1

let () =
  let argv = Sys.argv in
  if Array.length argv = 3 then
    let mode = argv.(1) in
    let filename = argv.(2) in
    let input_file = open_in filename in
    if mode = "scan" then scan input_file
    else if mode = "tokens" then tokens input_file
    else if mode = "parse" then parse input_file
    else if mode = "pretty" then pretty input_file
    else if mode = "typecheck" then parse input_file
    else printf "%s is not a valid compiler mode\n" mode; exit 1;
  else
    print_string "You must pass two argument: scan|tokens <source file path>\n";
    exit 1
