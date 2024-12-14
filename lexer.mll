(* Analyseur lexical pour Petit Koka *)

(* Analyseur lexical pour Petit Koka *)

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

(* Liste des lexèmes de fin de continuation *)
  let fin_continuation = [
  '+'; '-'; '*'; '/'; '%';
  "++"; "<"; "<="; ">" ; ">="; "=="; "!="; "&&"; "||"; '('; '{'; ','
  ]

(* Liste des lexèmes de début de continuation *)
  let debut_continuation = [
  '+'; '-'; '*'; '/'; '%';
  "++"; "<"; "<="; ">" ; ">="; "=="; "!="; "&&"; "||"; "then"; "else"; "elif";
  ')'; '}'; ','; "->"; '{'; '='; '.'; ":="
  ]

  (* Fonction pour vérifier si un lexème est une fin de continuation *)
  let is_fin_continuation lexeme =
    List.mem lexeme fin_continuation

  (* Fonction pour vérifier si un lexème est un début de continuation *)
  let is_debut_continuation lexeme =
    List.mem lexeme debut_continuation

  (* Fonction pour gérer les retours à la ligne et l'indentation *)

let rec action_retour_chariot lexbuf last indentation_stack =
  let next = token lexbuf in  (* Lire le prochain lexème *)
  let c = lexeme_start_p lexbuf in  (* Obtenir la colonne actuelle *)
  let m = Stack.top indentation_stack in  (* Récupérer la colonne au sommet de la pile *)

  (* Si la colonne c est plus grande que m (nouveau bloc d'indentation) *)
  if c > m then (
    (* Si le dernier lexème n'est pas une fin de continuation et que next n'est pas un début de continuation, on émet { ; *)
    if not (List.mem last fin_continuation) && not (List.mem next debut_continuation) then
      emit LBRACE;

    (* Si le dernier lexème émis est {, empiler la colonne c dans la pile d'indentation *)
    if last = LBRACE then
      Stack.push c indentation_stack;

    (* Émettre le lexème suivant *)
    emit next
  ) else (
    (* Si la colonne c est inférieure ou égale à m, on doit dépiler la pile d'indentation *)
    while c < m do
      Stack.pop indentation_stack;
      let m = Stack.top indentation_stack in  (* Mettre à jour m avec la nouvelle valeur du sommet de la pile *)

      (* Si next n'est pas RBRACE, on émet ; *)
      if next <> RBRACE then emit SEMI;

      (* Émettre } pour fermer le bloc *)
      emit RBRACE
    done;

    (* Si c est plus grand que m, échouer avec une erreur d'indentation *)
    if c > m then failwith "Indentation error";

    (* Si last n'est pas une fin de continuation et next n'est pas un début de continuation, émettre ; *)
    if not (List.mem last fin_continuation) && not (List.mem next debut_continuation) then
      emit SEMI;

    (* Émettre le lexème suivant *)
    emit next
  )
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
  | '\n'                 { action_retour_chariot (); token lexbuf }
  | ident as id           { id_or_kwd id }
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
  | "elif"               { ELIF }
  | "else"               { ELSE }
  | "fn"                 { FN }
  | "fun"                { FUN }
  | "(*"                  { comment lexbuf }
  | integer as s          { CST (int_of_string s) }
  | string as s           { STRING s }
  | lower as c            { LOWER (Char.escaped c) }
  | upper as c            { UPPER (Char.escaped c) }
  | digit as c            { DIGIT (Char.escaped c) }
  | other as c            { OTHER (Char.escaped c) }
  | eof                   { EOF }
  | _ as c                { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)"                  { token lexbuf }
  | [^ '*']+              { comment lexbuf }
  | '*' [^ '/']*          { comment lexbuf }
  | eof                   { raise (Lexing_error "unterminated comment") }