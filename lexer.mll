(* Analyseur lexical pour Petit Koka *)

{
  open Lexing
  open Parser

  exception Lexing_error of string
  exception Indentation_error of string

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

  let lbracket = "{"
  let rbracket = "}"
  let semi = ";"
  

  (* Fonction pour obtenir le type d'un lexème *)

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      let s = String.lowercase_ascii s in (* la casse n'est pas significative *)
      try Hashtbl.find h s with _ -> IDENT s


(* Liste des lexèmes de fin de continuation *)
  let fin_continuation = [
  "+"; "-"; "*"; "/"; "%";
  "++"; "<"; "<="; ">" ; ">="; "=="; "!="; "&&"; "||"; "("; "{"; ","
  ]

(* Liste des lexèmes de début de continuation *)
  let debut_continuation = [
  "+"; "-"; "*"; "/"; "%";
  "++"; "<"; "<="; ">" ; ">="; "=="; "!="; "&&"; "||"; "then"; "else"; "elif";
  ")"; "}"; ","; "->"; "{"; "="; "."; ":="
  ]

  (* Fonction pour vérifier si un lexème est une fin de continuation *)
  let is_fin_continuation lexeme =
    List.mem lexeme fin_continuation

  (* Fonction pour vérifier si un lexème est un début de continuation *)
  let is_debut_continuation lexeme =
    List.mem lexeme debut_continuation

(* Fonction pour émettre un lexème *)
let emit token = token

(* Fonction pour gérer les retours à la ligne et l'indentation *)

let indentation_stack = Stack.create ()  (* Initialiser la pile d'indentation *)
let () = Stack.push 0 indentation_stack  (* Empiler 0 dans la pile d'indentation *)

(* Fonction pour calculer l'indentation d'une ligne *)
let rec count_spaces n lexbuf =
  if Lexing.lexeme_char lexbuf 0 = ' ' then (
    Lexing.new_line lexbuf;  (* Move to the next character *)
    count_spaces (n + 1) lexbuf
  ) else n


  (* Gestion de l'indentation significative *)
let handle_indentation current_indent =
    let previous_indent = Stack.top indentation_stack in
    if current_indent > previous_indent then (
      Stack.push current_indent indentation_stack;
      [INDENT]
    )
    else if current_indent < previous_indent then (
      let rec dedent_tokens acc =
        if Stack.is_empty indentation_stack then
          raise (Indentation_error "Mauvais niveau d'indentation")
        else if Stack.top indentation_stack > current_indent then (
          ignore (Stack.pop indentation_stack);
          dedent_tokens (DEDENT :: acc)
        )
        else acc
      in
      dedent_tokens []
    )
    else
      []

  (* Gestion des nouvelles lignes et de l'indentation *)
let newline_and_indent lexbuf =
    let current_indent = count_spaces 0 lexbuf in
    let indent_tokens = handle_indentation current_indent in
    NEWLINE :: indent_tokens

}

(* Fonction pour émettre un lexème *)
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let digit = ['0'-'9']
let other = lower | upper | digit | '-'
let ident = lower (other | "'")* ('-' (lower | digit) (other | "'")*)*
let space = [' ' '\t' '\r']
let integer = '-'? ('0' | ['1'-'9'] digit*)
let string = "\"" ([^ '"' '\\' '\n'] | "\\\"" | "\\\\" | "\\t" | "\\n")* "\""


(*let indentation_stack = Stack.create () *)

rule token = parse
  | "//" [^ '\n']* '\n' { new_line lexbuf; token lexbuf }
  | space+                { token lexbuf }
  | ident as id           { id_or_kwd id }
  | [' ' '\t']+   { token lexbuf } (* Ignorer les espaces/tabs entre les tokens *)
  | '\n' { let spaces = count_spaces 0 lexbuf in
           (* Use the space count for indentation logic *)
           token lexbuf }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { TIMES }
  | '.'                   { DOT }
  | '/'                   { DIV }
  | ':'                   { COLON }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { BEGIN }
  | '}'                   { END }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | ','                   { COMMA }
  | ';'                   { SEMI }
  | "++"                 { PLUSPLUS }
  | '%'                   { MOD }
  | "<="                 { LESSEQ }
  | ">="                 { GREATEREQ }
  | "=="                 { EQEQ }
  | ":="                 { ASSIGN }
  | "!="                 { NOTEQ }
  | "<"                  { LESS }
  | ">"                  { GREATER }
  | "&&"                 { ANDAND }
  | "||"                 { OROR }
  | '~'                  { TILDE }
  | '!'                  { BANG }
  | '='                  { EQ }
  | "->"                 { ARROW }
  | "(*"                  { comment lexbuf }
  | ['a'-'z' 'A'-'Z' '_' '0'-'9']+ as ident { IDENT ident }
  | integer as s          { CST (int_of_string s) }
  | string as s           { STRING s }
  | eof                   { EOF }
  | _ as c                { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)"                  { token lexbuf }
  | [^ '*']+              { comment lexbuf }
  | '*' [^ '/']*          { comment lexbuf }
  | eof                   { raise (Lexing_error "unterminated comment") }