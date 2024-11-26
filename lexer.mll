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
let space = [' ' '\t' '\r']
let ident = lower (other | "'")* ('-' (lower | digit) (other | "'")*)* (*un tiret (-) dans un identificateur doit être précédé d’une lettre ou d’un chiffre et
suivi d’une lettre s’il n’est pas en dernière position. *)

let integer = -?['0' | '1'-'9' + digit*]
let string = "\"" ([^ '"' '\\' '\n'] | "\\\"" | "\\\\" | "\\t" | "\\n")* "\"" 


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

(* note : les commentaires ne sont pas imbriqués en Coka *)
and comment = parse
  | '\n'    { new_line lexbuf; comment lexbuf }
  | "/*"       { comment lexbuf }
  | "*/"       { token lexbuf }
  | "//" [^ '\n']* '\n' { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }


(*Indentation significative *)
(*Algorithme de lecture des lexèmes*)
(* Définition de fin de continuation  *)

let fin_continuation = ['+'|'-'|'*'|'/'|'%'| '++' | '<' | '>' | '>=' | '==' | '!=' | '&&' | '||' | '<=' | '>' | '(' | '{' | ',']


(* Définition de début de continuation  *)

let debut_continuation = ['+'; '-'; '*'; '/'; '%'; "++"; '<'; "<="; '>'; ">="; "=="; "!="; "&&"; "||"; "then"; "else"; "elif"; ')'; '}'; ','; "->"; '{'; '='; '.'; ":="]

(*Algorithme déterminant si une action doit être effectuée lors d'un retour charoiot*)

let rec action_retour_chariot () = 
  let rec action_retour_chariot_aux () = 
      let next = token lexbuf in
      let c = lexeme_start_p lexbuf.pos_cnum in
      let m = Stack.top indentation_stack in
      if c > m then (
        if not (List.mem last fin_continuation) && not (List.mem next debut_continuation) then
          emit LBRACE;
        if last = LBRACE then
          Stack.push c indentation_stack;
        emit next
      ) else (
      while c < m do
        Stack.pop indentation_stack;
        let m = Stack.top indentation_stack in
        if next <> RBRACE then emit SEMI;
        emit RBRACE
      done;
      if c > m then failwith "Indentation error";
      if not (List.mem last fin_continuation) && not (List.mem next debut_continuation) then
        emit SEMI;
      emit next
      )