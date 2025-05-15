open Lexing
open Ast

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try
    let ast = Parser.file Lexer.next_token lexbuf in
    close_in ic;
    ast
  with
  | Lexer.Error (msg, pos) ->
      Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        (pos.pos_cnum - pos.pos_bol + 1) msg;
      exit 1
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\nsyntax error\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        (pos.pos_cnum - pos.pos_bol + 1);
      exit 1

let main () =
  let parse_only = ref false in
  let type_only = ref false in
  let filename = ref "" in
  let usage = "Usage: kokac [--parse-only | --type-only] <file.koka>" in
  Arg.parse
    [("--parse-only", Arg.Set parse_only, "Stop after parsing");
     ("--type-only", Arg.Set type_only, "Stop after type checking")]
    (fun f -> filename := f)
    usage;
  if !filename = "" || not (Filename.check_suffix !filename ".koka") then
    (prerr_endline usage; exit 2);
  try
    let ast = parse_file !filename in
    if !parse_only then exit 0;
    Typing.typecheck_file ast;
    if !type_only then exit 0;
    (* Code generation will be added here for Part 2 *)
    exit 0
  with
  | Typing.TypeError (msg, pos) ->
      Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        (pos.pos_cnum - pos.pos_bol + 1) msg;
      exit 1
  | e ->
      prerr_endline ("Internal compiler error: " ^ Printexc.to_string e);
      exit 2

let () = main ()