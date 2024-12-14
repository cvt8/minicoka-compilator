/* Analyseur syntaxique pour mini-Koka */

%{
  open Ast

 (* let neg e = Ebinop (Sub, Econst 0, e)
    let not e = Eunop (Not, e) *)

  let block stmt = Sblock stmt
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

%left OROR
%left ANDAND
%left MINUS PLUS PLUSPLUS
%left TIMES DIV
%nonassoc ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER
%nonassoc TILDE BANG
%nonassoc IF
%nonassoc DOT BEGIN END FUN

/* Point d'entrée de la grammaire */

%start <Ast.file> file

%type <Ast.param_type> param_type
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

%%

True:
    | TRUE
        { true }

False:
    | FALSE
        { false }

ident:
    | IDENT
        { IDENT }

file:
    decls = decl* EOF
    { decls }
;

decl:
    | FUN IDENT funbody
        { Sfun (IDENT, funbody) }
;

funbody:
    | LPAREN param* COMMA RPAREN annot? expr
        { (param, annot, expr) }
;



block:
| BEGIN SEMI* LPAREN stmt SEMI+ RPAREN* END
        { stmt }
;

param:
| IDENT COLON param_type
        { (IDENT, param_type) }
;

annot:
| COLON result
        { result }
;

result:
| LPAREN LESS IDENT* COMMA GREATER RPAREN? param_type
        { (IDENT, param_type) }
;

param_type:
| atype
        { atype }
    | atype ARROW result
        { (atype, result) }
    | LPAREN param_type* COMMA RPAREN ARROW result
        { (param_type, result) }
;

atype:
| IDENT LPAREN LESS param_type = separated_list(COMMA, atype) GREATER RPAREN
        { ATypeApp (IDENT, param_type) }
    | LPAREN atype RPAREN
        { ATypeParen atype }
    | LPAREN RPAREN
        { AUnit }
;

atom:
    | True
        { Econst (true) }
    | False
        { Econst (false) }
    | CST
        { Econst (CST) }
    | STRING
        { Estring (STRING) }
    | IDENT
        { Evar (IDENT) }
    | LPAREN expr RPAREN
        { expr }
    | atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Ecall (atom, expr_list) }
    | atom FN funbody
        { Efun (funbody) }
    | atom block
        { Eblock (block) }
    | LBRACKET expr_list = separated_list(COMMA, expr) RBRACKET
        { Earray (expr_list) }
;


expr:
    | block
        { Eblock block }
    | bexpr
        { bexpr }
    | IF bexpr THEN expr ELSE expr
        { Sif (bexpr, expr, expr, []) }
    | IF bexpr THEN expr ELIF* ELSE expr
        { Sif (bexpr, expr, ELIF, expr) }
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
        { Eblock (block) }
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
    | IF bexpr THEN expr ELIF* ELSE expr
        { Sif (bexpr, expr, ELIF, expr) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, [], None) }
    | FUN funbody
        { Sfun funbody }
    | RETURN expr
        { Sreturn expr }
;


stmt:
    | bexpr
        { bexpr }
    | VAL IDENT EQ expr
        { Sval (IDENT, expr) }
    | VAL IDENT EQEQ expr
        { Svar (IDENT, expr) }
    | IF bexpr THEN stmt
        { Sif (bexpr, stmt, Sblock [], None) }
    | IF bexpr THEN stmt ELSE stmt
        { Sif (bexpr, stmt, stmt, None) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Ereturn expr, [], None) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Scall (atom, expr_list) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT
        { Scall (Ecall (atom, expr_list), IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Scall (atom, expr_list @ [Eblock block]) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT block
        { Scall (Ecall (atom, expr_list @ [Eblock block]), IDENT, []) }
    | atom block
        { Sblock block }
;

binop:
| EQEQ | NOTEQ | LESSEQ | GREATEREQ | LESS | GREATER | PLUS | MINUS | TIMES | DIV | ANDAND | OROR | PLUSPLUS
    { binop }
;