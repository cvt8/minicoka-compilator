/* Analyseur syntaxique pour mini-Koka */

%{
  open Ast 
  open Lexing
%}

%token <int> CST
%token <string> IDENT
%token <string> STRING
%token <string> UPPER
%token <string> DIGIT
%token <string> OTHER
%token <string> SPACE
%token COMMA BEGIN END
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token IF ELSE REPEAT ELIF THEN FN VAL RETURN VAR
%token EOF
%token COLON
%token TILDE BANG OROR ANDAND PLUSPLUS ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER DOT FUN
%token LBRACKET RBRACKET SEMI ARROW
%token EQ
%token TRUE FALSE

/* Les priorités et associativités des tokens */

(*)
%left OROR
%left ANDAND
%left MINUS PLUS PLUSPLUS
%left TIMES DIV
%nonassoc ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER
%nonassoc TILDE BANG
%nonassoc IF
%nonassoc DOT BEGIN END FUN *)

/* Point d'entrée de la grammaire */

%start file

%type <Ast.param_type> param_type
%type <Ast.file> file
%type <Ast.expr> expr
%type <Ast.bexpr> bexpr
%type <Ast.atom> atom
%type <Ast.stmt> stmt 
%type <Ast.block> block 
%type <Ast.binop> binop
%type <Ast.funbody> funbody 
%type <Ast.param> param
%type <Ast.annot> annot 
%type <Ast.result> result 
%type <Ast.atype> atype
%type <Ast.elif> elif
%%

(* Les règles de la grammaire *)

True:
    | TRUE
        { Econst true }

False:
    | FALSE
        { Econst false }

file:
    SEMI* LPAREN decl SEMI+ RPAREN* EOF
        { decl }
;

decl:
    | FUN IDENT funbody
        { Sfun (IDENT, funbody) }
;

funbody:
    | LPAREN param_list = separated_list(COMMA, param) RPAREN annot? expr
        { (param_list, annot, expr) }
;

param:
    | IDENT COLON param_type
        { (IDENT, param_type) }
;

annot:
    | COLON result
        { result }
; 

param_type:
    | atype
        { PBase($1) }
    | atype ARROW result
        { PArrow(atype, result) }
    | LPAREN param_type_list = separated_list(COMMA, param_type) RPAREN ARROW result
        { PArrow(PList(param_type_list), result) }
;

result:
    | LESS idents = separated_list(COMMA, IDENT) GREATER param_type
        { (idents, Some(param_type)) }
    | param_type
        { ([], Some(param_type)) }
;

atype:
    | IDENT LPAREN LESS param_type* COMMA GREATER RPAREN?
        { ATypeApp(IDENT, param_type) }
    | LPAREN atype RPAREN
        { ATypeParen atype }
    | LPAREN RPAREN
        { AUnit }
;

atom:
    | True
        { Econst true }
    | False
        { Econst false }
    | CST
        { Econst (int_of_string CST) }
    | STRING
        { Estring STRING }
    | IDENT
        { Evar IDENT }
    | LPAREN expr RPAREN
        { expr }
    | atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Ecall (atom, expr_list) }
    | atom FN funbody
        { Efun funbody } 
    | atom block
        { Eblock block } 
    | LBRACKET expr_list = separated_list(COMMA, expr) RBRACKET
        { Earray expr_list }
    | atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN FN funbody
        { Ecall (atom, expr_list @ [Efun funbody]) } 
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Ecall (atom, expr_list @ [Eblock block]) } 
;

expr:
    | block
        {Eblock(block) } 
    | bexpr
        {Eexpr(bexpr)}
    (*| IF bexpr THEN expr ELSE expr
        { Sif (bexpr, expr, expr, []) }
    | IF bexpr THEN expr elif_list = separated_list(ELIF, elif) ELSE expr
        { Sif (bexpr, expr, elif_list, Some expr) } 
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, [], None) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Ecall (atom, expr_list) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT
        { Ecall (Ecall (atom, expr_list), IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Ecall (atom, expr_list @ [Eblock block]) } 
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT block
        { Ecall (Ecall (atom, expr_list @ [Eblock block]), IDENT, []) }
    | atom block
        { Eblock block } 
    | atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN FN funbody
        { Ecall (atom, expr_list @ [Efun funbody]) }  *)
;

bexpr:
    | atom
        { atom }
    | TILDE bexpr
        { Eunop (Not, bexpr) }
    | BANG bexpr
        { Eunop (Neg, bexpr) }
    | bexpr binop bexpr
        { Ebinop (binop, bexpr, bexpr) }
    | IDENT ASSIGN bexpr
        { Sassign (IDENT, bexpr) }
    | IF bexpr THEN expr ELSE expr
        { Sif (bexpr, expr, expr, []) }
   | IF bexpr THEN expr LPAREN ELIF bexpr THEN expr RPAREN* LPAREN ELSE expr RPAREN?
        { Sif (bexpr, expr, [], None) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, [], None) }
    | FN funbody
        { Sfn funbody } 
    | RETURN expr
        { Sreturn expr }
;

block:
    | BEGIN SEMI* LPAREN stmt SEMI+ RPAREN* END
        { Sblock(stmt) }
;

stmt: 
    | bexpr
        { bexpr } 
   (*) | VAL IDENT EQ expr
        { Sval (expr) }
    | VAR IDENT ASSIGN expr
        { Svar (expr) } *)
    (*| IF bexpr THEN stmt
        { Sif (bexpr, stmt, [Sblock []]) }
    | IF bexpr THEN stmt ELSE stmt
        { Sif (bexpr, stmt, [stmt]) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, []) } *)
    (*| atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Scall (atom, expr_list) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT
        { Scall (Ecall (atom, expr_list), IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Scall (atom, expr_list @ [Eblock block]) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT block
        { Scall (Ecall (atom, expr_list @ [Eblock block]), IDENT, []) }
    | atom block
        { Sblock block }
    | atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN FN funbody
        { Scall (atom, expr_list @ [Efun funbody]) } *)
;

binop:
    | EQEQ | NOTEQ | LESSEQ | GREATEREQ | LESS | GREATER | PLUS | MINUS | TIMES | DIV | ANDAND | OROR | PLUSPLUS
        { binop }
;

elif:
    | ELSE IF bexpr THEN stmt
        { Sif (bexpr, stmt, [], None) }
    | ELSE IF bexpr THEN stmt ELSE stmt
        { Sif (bexpr, stmt, [stmt], None) }
;