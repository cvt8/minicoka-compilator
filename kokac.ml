open Lexing
open Ast
open Typing

let report_error file msg pos_start pos_end =
  Printf.printf "File \"%s\", line %d, characters %d-%d:\n%s\n" file
    pos_start.pos_lnum (pos_start.pos_cnum - pos_start.pos_bol)
    (pos_end.pos_cnum - pos_end.pos_bol) msg;
  exit 1

let () =
  let usage = "Usage: kokac [--parse-only | --type-only] <file.koka>" in
  let file = ref "" in
  let parse_only = ref false in
  let type_only = ref false in
  Arg.parse
    [("--parse-only", Arg.Set parse_only, "Stop after parsing");
     ("--type-only", Arg.Set type_only, "Stop after type checking")]
    (fun f -> if !file = "" then file := f else raise (Failure "multiple input files"))
    usage;
  if !file = "" then (print_endline usage; exit 1);
  if Filename.extension !file <> ".koka" then
    (print_endline "Input file must have .koka extension"; exit 1);
  try
    let ic = open_in !file in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !file };
    let ast = try Parser.file Lexer.token lexbuf
              with
              | Lexer.LexicalError (msg, p1, p2) -> report_error !file ("lexical error: " ^ msg) p1 p2
              | Parser.Error -> report_error !file "syntax error" lexbuf.lex_start_p lexbuf.lex_curr_p
    in
    close_in ic;
    if !parse_only then exit 0;
    try
      let _ = type_file ast in
      if !type_only then exit 0;
      (* Code generation would go here for Part 2 *)
      exit 0
    with TypeError (msg, p1, p2) ->
      report_error !file ("type error: " ^ msg) p1 p2
  with Sys_error msg ->
    Printf.printf "Error: %s\n" msg; exit 2