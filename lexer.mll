(* Analyseur lexical pour mini-Koka *)

{
  open Lexing
  open Parser

  exception Lexing_error of string

  (* tables des mots-clés *)
  let kwd_tbl =
    ["if", IF;
    "elif", ELIF;   
     "else", ELSE;
     "fn", FN;
     "fun", FUN;
     "return", RETURN;
     "then", THEN;
     "val", VAL;
     "var", VAR;
    ]

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      let s = String.lowercase_ascii s in (* la casse n'est pas significative *)
      try Hashtbl.find h s with _ -> IDENT s

}

let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let digit = ['0'-'9']
let other = lower | upper | digit | '_'
let ident = lower (other | "'")*
let integer = ['0'-'9']+
let space = [' ' '\t' '\r']

rule token = parse
  | "//" [^ '\n']* '\n'
  | '\n'    { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { BEGIN }
  | '}'     { END }
  | ','     { COMMA }
  | "(*"    { comment lexbuf }
  | integer as s { CST (int_of_string s) }
  | "//" [^ '\n']* eof
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* note : les commentaires ne sont pas imbriqués en Turtle *)
and comment = parse
  | '\n'    { new_line lexbuf; comment lexbuf }
  | "/*"       { comment lexbuf }
  | "*/"       { token lexbuf }
  | "//" [^ '\n']* '\n' { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
