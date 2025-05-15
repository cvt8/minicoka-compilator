{
  open Parser
  open Lexing

  (* Stack to track indentation levels *)
  let indent_stack = ref [0]

  (* Last emitted token for continuation checks *)
  let last_token = ref NONE

  (* Queue for tokens to be emitted *)
  let token_queue = Queue.create ()

  (* Helper to check if a token is a continuation end *)
  let is_continuation_end = function
    | PLUS | MINUS | STAR | SLASH | PERCENT | PLUSPLUS | LT | LE | GT | GE | EQ | NEQ | AND | OR | LPAREN | COMMA -> true
    | _ -> false

  (* Helper to check if a token is a continuation start *)
  let is_continuation_start = function
    | PLUS | MINUS | STAR | SLASH | PERCENT | PLUSPLUS | LT | LE | GT | GE | EQ | NEQ | AND | OR | THEN | ELSE | ELIF | RPAREN | RBRACE | SEMI | ARROW | LBRACE | COLONEQ -> true
    | _ -> false

  (* Emit a token to the queue *)
  let emit_token tok = Queue.add tok token_queue

  (* Process a newline and adjust indentation *)
  let process_newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    let col = pos.pos_cnum - pos.pos_bol in
    let top = List.hd !indent_stack in
    if col > top then begin
      if not (is_continuation_end !last_token) && not (is_continuation_start (lookahead lexbuf)) then
        emit_token LBRACE;
      emit_token (lookahead lexbuf);
      indent_stack := col :: !indent_stack
    end else begin
      while col < top do
        indent_stack := List.tl !indent_stack;
        if (lookahead lexbuf) <> RBRACE then emit_token SEMI;
        emit_token RBRACE;
        let top = List.hd !indent_stack in
        if col > top then failwith "Invalid indentation"
      done;
      if col = top && not (is_continuation_end !last_token) && not (is_continuation_start (lookahead lexbuf)) then
        emit_token SEMI;
      emit_token (lookahead lexbuf)
    end

  (* Lookahead to the next non-whitespace, non-comment token *)
  let rec lookahead lexbuf =
    let tok = lex_token lexbuf in
    match tok with
    | SPACE | NEWLINE | COMMENT _ -> lookahead lexbuf
    | _ -> tok

  (* Main token reading function *)
  let next_token lexbuf =
    if not (Queue.is_empty token_queue) then
      let tok = Queue.take token_queue in
      last_token := tok;
      tok
    else
      let tok = lex_token lexbuf in
      last_token := tok;
      tok
}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '-'
let upper = ['A'-'Z']
let other = lower | upper | digit | '-'
let ident = lower other*

rule lex_token = parse
  | [' ' '\t']+ { SPACE }
  | '\n'        { process_newline lexbuf; next_token lexbuf }
  | "/*"        { comment lexbuf }
  | "//"        { line_comment lexbuf }
  | ident as id {
      match id with
      | "elif"   -> ELIF
      | "else"   -> ELSE
      | "fn"     -> FN
      | "fun"    -> FUN
      | "if"     -> IF
      | "return" -> RETURN
      | "then"   -> THEN
      | "val"    -> VAL
      | "var"    -> VAR
      | _        -> if String.contains id '-' then
                      check_hyphen id
                    else
                      ID id
    }
  | "-"? (['0'] | ['1'-'9'] digit*) as n { INT (int_of_string n) }
  | "\""        { string (Buffer.create 17) lexbuf }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { STAR }
  | "/"         { SLASH }
  | "%"         { PERCENT }
  | "++"        { PLUSPLUS }
  | "<"         { LT }
  | "<="        { LE }
  | ">"         { GT }
  | ">="        { GE }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "["         { LBRACK }
  | "]"         { RBRACK }
  | "{"         { LBRACE }
  | "}"         { RBRACE }
  | ","         { COMMA }
  | ";"         { SEMI }
  | ":"         { COLON }
  | ":="        { COLONEQ }
  | "->"        { ARROW }
  | "True"      { TRUE }
  | "False"     { FALSE }
  | "()"        { UNIT }
  | eof         { process_newline lexbuf; EOF }
  | _           { error lexbuf "Illegal character" }

and comment = parse
  | "*/"        { COMMENT "*/" }
  | _           { comment lexbuf }
  | eof         { error lexbuf "Unterminated comment" }

and line_comment = parse
  | '\n'        { NEWLINE }
  | _           { line_comment lexbuf }
  | eof         { EOF }

and string buf = parse
  | "\""        { STRING (Buffer.contents buf) }
  | "\\\""      { Buffer.add_char buf '"'; string buf lexbuf }
  | "\\\\"      { Buffer.add_char buf '\\'; string buf lexbuf }
  | "\\t"       { Buffer.add_char buf '\t'; string buf lexbuf }
  | "\\n"       { Buffer.add_char buf '\n'; string buf lexbuf }
  | _ as c      { Buffer.add_char buf c; string buf lexbuf }
  | eof         { error lexbuf "Unterminated string" }

{
  let check_hyphen id =
    let valid = ref true in
    for i = 1 to String.length id - 1 do
      if id.[i] = '-' then
        if i = String.length id - 1 || not (Char.is_letter id.[i-1] || Char.is_digit id.[i-1]) || not (Char.is_letter id.[i+1]) then
          valid := false
    done;
    if !valid then ID id else error lexbuf "Invalid hyphen in identifier"

  let error lexbuf msg =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (pos.pos_cnum - pos.pos_bol + 1) msg;
    exit 1
}