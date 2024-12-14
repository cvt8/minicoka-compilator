/* Analyseur syntaxique pour mini-Koka */

%{
  open Ast

  let neg e = Ebinop (Sub, Econst 0, e)
%}

%token <int> CST
%token <string> IDENT
%token <string> STRING
%token <string> LOWER
%token <string> UPPER
%token <string> DIGIT
%token <string> OTHER
%token <string> SPACE
%token COMMA BEGIN END DEF
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token IF ELSE REPEAT ELIF FORWARD TURNLEFT TURNRIGHT
%token EOF
%token COLON
%token TILDE BANG OROR ANDAND PLUSPLUS ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER DOT FUN
%token LBRACKET RBRACKET SEMI

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
    | LPAREN param* RPAREN annot = annot_opt expr = expr
    { (param, annot, expr) }


param:
| IDENT COLON param_type
        { (IDENT, param_type) }
;

annot:
| COLON result
        { result }
;

result:
| LPAREN LESS ident = separated_list(COMMA, IDENT) GREATER RPAREN param_type = type_opt
        { (ident, type) }
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
| IDENT LT atype GT
        { ATypeApp(IDENT, atype) }
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
    | IF bexpr THEN expr elif* ELSE expr
        { Sif (bexpr, expr, elif, expr) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, [], None) }
    | FUN funbody
        { Sfun funbody }
    | RETURN expr
        { Sreturn expr }
;

block:
| BEGIN stmt_list = stmt* END
        { Sblock stmt_list }
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
        { Sif (bexpr, Sreturn expr, [], None) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Scall (atom, expr_list) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT
        { Scall (Ecall (atom, IDENT, expr_list), []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Scall (atom, expr_list @ [Eblock block]) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT block
        { Scall (Ecall (atom, IDENT, expr_list @ [Eblock block]), []) }
    | atom block
        { Sblock (block) }
;

binop:
| EQEQ | NOTEQ | LESSEQ | GREATEREQ | LESS | GREATER | PLUS | MINUS | TIMES | DIV | ANDAND | OROR | PLUSPLUS
;

/* Sucre syntaxique */

atom:
| atom DOT IDENT
        { Ecall (atom, IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Ecall (atom, expr_list) }
    | atom FN funbody
        { Efun (funbody) }
    | atom block
        { Eblock (block) }
;

expr:
| IF bexpr THEN expr ELSE expr
        { Sif (bexpr, expr, expr, []) }
    | IF bexpr THEN expr elif* ELSE expr
        { Sif (bexpr, expr, elif, expr) }
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

stmt:
| IF bexpr THEN stmt
        { Sif (bexpr, stmt, Sblock [], None) }
    | IF bexpr THEN stmt ELSE stmt
        { Sif (bexpr, stmt, stmt, None) }
    | IF bexpr RETURN expr
        { Sif (bexpr, Sreturn expr, [], None) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { Scall (atom, expr_list) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT
        { Scall (Ecall (atom, expr_list), IDENT, []) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN block
        { Scall (atom, expr_list @ [Eblock block]) }
    | atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN DOT IDENT block
        { Scall (Ecall (atom, expr_list @ [Eblock block]), IDENT, []) }
    | atom block
        { Sblock (block) }
;

def:
| DEF IDENT LPAREN formals = separated_list(COMMA, IDENT) RPAREN body = stmt        { { name = IDENT;
            formals = formals;
            body = body } }
;