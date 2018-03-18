open Printf
open Lexing
open Weeding
open Symbol

(* prints the error message msg with the associate lexer buffer *)
let print_error lb msg =
  let pos = lb.lex_curr_p in
  let line = pos.pos_lnum in
  let lcur = pos.pos_cnum - pos.pos_bol + 1 in
    fprintf stderr "%s (%i:%i)\n" msg line lcur

let scan input =
  let lexer_buffer = Lexing.from_string input in
  try
    let _ = Parsertokens.prog Lexertokens.read lexer_buffer in
      print_string "OK\n";
      exit 0;
  with Lexertokens.SyntaxError msg ->
    print_error lexer_buffer ("Error: " ^ msg);
    exit 1

let tokens input =
  let lexer_buffer = Lexing.from_string input in
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
  let lexer_buffer = Lexing.from_string input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.weed ast with
      | [] ->
        print_string "OK\n";
        exit 0;
      | w::_ ->
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
  let lexer_buffer = Lexing.from_string input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.weed ast with
      | [] ->
        Pretty.pretty_print ast;
        print_newline ();
        exit 0;
      | w::_ ->
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

let typecheck input =
  let lexer_buffer = Lexing.from_string input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.weed ast with
      | [] ->
        (match Symbol.typecheck_opt ast with
        | Some _ ->
          print_string "OK\n";
          exit 0;
        | None ->
          print_error lexer_buffer "Error: Type check error";
          exit 1;)
      | w::_ ->
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

let symbol input =
  let lexer_buffer = Lexing.from_string input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.weed ast with
      | [] ->
        (match Symbol.typecheck_opt ast with
        | Some (_, symbols) ->
          print_string (Symbol.string_of_symbol_table symbols);
          exit 0;
        | None ->
          print_error lexer_buffer "Error: Type check error";
          exit 1;)
      | w::_ ->
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

let codegen input =
  let lexer_buffer = Lexing.from_string input in
  try
    let ast = Parser.prog Lexer.read lexer_buffer in
      match Weeding.weed ast with
      | [] ->
        (match Symbol.typecheck_opt ast with
        | Some (ast, _) ->
          print_string (Codegen.codegen ast);
          exit 0;
        | None ->
          print_error lexer_buffer "Error: Type check error";
          exit 1;)
      | w::_ ->
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

let in_to_string ic =
  let rec r ic o =
    try
      r ic (String.concat "" [o; (input_char ic |> Utils.string_of_char)])
    with End_of_file -> o
  in
    r ic ""

let () =
  let argv = Sys.argv in
  if Array.length argv = 3 then
    let mode = argv.(1) in
    let filename = argv.(2) in
    let input_file = open_in filename in
    let file_content = in_to_string input_file in
    let file_content = String.concat "" [file_content; "\n"] in
    if mode = "scan" then scan file_content
    else if mode = "tokens" then tokens file_content
    else if mode = "parse" then parse file_content
    else if mode = "pretty" then pretty file_content
    else if mode = "typecheck" then typecheck file_content
    else if mode = "symbol" then symbol file_content
    else if mode = "codegen" then codegen file_content
    else printf "%s is not a valid compiler mode\n" mode; exit 1;
  else
    print_string "You must pass two argument: scan|tokens <source file path>\n";
    exit 1
