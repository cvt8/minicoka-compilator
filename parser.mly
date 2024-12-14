/* Analyseur syntaxique pour mini-Koka */

%{
  open Ast

  let neg e = Ebinop (Sub, Econst 0, e)
%}

%token <int> CST
%token <string> IDENT
%token COMMA BEGIN END DEF
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token IF ELSE REPEAT PENUP PENDOWN FORWARD TURNLEFT TURNRIGHT
%token EOF
%token COLON
%token TILDE BANG OROR ANDAND PLUSPLUS ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER DOT FUN


/* Les priorités et associativités des tokens */

%left OROR
%left ANDAND
%left MINUS PLUS PLUSPLUS
%left TIMES DIV MOD
%nonassoc ASSIGN EQEQ NOTEQ LESSEQ GREATEREQ LESS GREATER
%nonassoc TILDE BANG
%nonassoc IF
%nonassoc DOT BEGIN END FUN 

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

file:
    decls = decl* EOF
    { decls }
;

decl:
    | FUN ident funbody
        { Sfun (ident, funbody) }
;

funbody:
    | LPAREN param* COMMA RPAREN annot = annot_opt expr = expr
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
    | LPAREN LESS idents = separated_list(COMMA, IDENT) GREATER RPAREN param_type = type_opt
        { (idents, type) }
;


param_type:
    | atype 
    | atype ARROW result
    | LPAREN param_type* COMMA RPAREN ARROW result
;

atype:
    | IDENT LPAREN param_type RPAREN { TypeApp($1, $3) }  (* Type application *)
    | LPAREN param_type RPAREN { TypeParen($2) }  (* Parenthesized type *)
    | LPAREN RPAREN { TypeUnit }  (* Unit type or empty tuple *)
;

atom:
    | True | False | integer | string | RPAREN LPAREN 
    | ident
    | LPAREN expr RPAREN
    | atom LPAREN expr* COMMA RPAREN
    | atom DOT ident
    | atom FN funbody
    | atom block
    | LBRAK expr* COMMA RBRAK
;

expr:
    | block 
    | bexpr
;

bexpr:
    | atom
    | TILDE b = bexpr
        { Eunop (Not, b) }
    | BANG b = bexpr
        { Eunop (Neg, b) }
    | b1 = bexpr o = binop b2 = bexpr
        { Ebinop (o, b1, b2) }
    | id = IDENT ASSIGN b = bexpr
        { Sassign (id, b) }
    | IF b = bexpr THEN e = expr elifs = elif* else_expr = else_expr_opt
        { Sif (b, e, elifs, else_expr) }
    | IF b = bexpr RETURN e = expr
        { Sif (b, Sreturn e, [], None) }
    | FUN fb = funbody
        { Sfun fb }
    | RETURN e = expr
        { Sreturn e }
;

block:
    | BEGIN SEMI* LPAREN stmt SEMI+ RPAREN* END
        { Sblock stmt }
;

stmt:
    | bexpr
    | VAL ident = expr
    | VAL ident := expr
;

binop:
    | EQEQ | NOTEQ | LESSEQ | GREATEREQ | LESS | GREATER | PLUS | MINUS | TIMES | DIV | MOD | ANDAND | OROR | PLUSLUS
;

prog:
  defs = def*
  main = stmt*
  EOF
    { { defs = defs;
        main = Sblock main; } }
;

def:
| DEF f = IDENT
  LPAREN formals = separated_list(COMMA, IDENT) RPAREN body = stmt
    { { name = f;
        formals = formals;
        body = body } }
;

stmt:
| PENUP
    { Spenup }
| PENDOWN
    { Spendown }
| FORWARD e = expr
    { Sforward e }
| TURNLEFT e = expr
    { Sturn e }
| TURNRIGHT e = expr
    { Sturn (neg e) }
| id = IDENT LPAREN actuals = separated_list(COMMA, expr) RPAREN
    { Scall (id, actuals) }
| IF e = expr s = stmt
    { Sif (e, s, Sblock []) }
| IF e = expr s1 = stmt ELSE s2 = stmt
    { Sif (e, s1, s2) }
| REPEAT e = expr b = stmt
    { Srepeat (e, b) }
| BEGIN is = stmt* END
    { Sblock is }
;


%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;

/* Sucre syntaxique */

atom:
    | atom DOT ident
        { Ecall (atom, ident, []) }
    | atom LPAREN expr* COMMA RPAREN
        { Ecall (atom, "", expr) }
    | atom LPAREN expr* COMMA RPAREN DOT ident
        { Ecall (atom, ident, expr) }
    | atom FN funbody
        { Efun (funbody) }
    | atom block
        { Eblock (block) }
;

expr:
    | IF b = bexpr THEN e = expr elifs = elif* else_expr = else_expr_opt
        { Eif (b, e, elifs, else_expr) }
    | IF b = bexpr RETURN e = expr
        { Eif (b, Ereturn e, [], None) }
    | atom LPAREN expr* COMMA RPAREN
        { Ecall (atom, "", expr) }
    | atom LPAREN expr* COMMA RPAREN DOT ident
        { Ecall (atom, ident, expr) }
    | atom LPAREN expr* COMMA RPAREN block
        { Ecall (atom, "", expr @ [Eblock block]) }
    | atom LPAREN expr* COMMA RPAREN DOT ident block
        { Ecall (atom, ident, expr @ [Eblock block]) }
    | atom block
        { Eblock (block) }
;

stmt:
    | IF b = bexpr s = stmt
        { Sif (b, s, Sblock []) }
    | IF b = bexpr s1 = stmt ELSE s2 = stmt
        { Sif (b, s1, s2) }
    | IF b = bexpr RETURN e = expr
        { Sif (b, Sreturn e, [], None) }
    | IF b = bexpr THEN e = expr elifs = elif* else_expr = else_expr_opt
        { Sif (b, e, elifs, else_expr) }
    | atom LPAREN expr* COMMA RPAREN
        { Scall (atom, expr) }
    | atom LPAREN expr* COMMA RPAREN DOT ident
        { Scall (Ecall (atom, ident, []), expr) }
    | atom LPAREN expr* COMMA RPAREN block
        { Scall (Ecall (atom, "", expr @ [Eblock block]), []) }
    | atom LPAREN expr* COMMA RPAREN DOT ident block
        { Scall (Ecall (atom, ident, expr @ [Eblock block]), []) }
    | atom block
        { Sblock (block) }
;

def:
    | DEF f = IDENT LPAREN formals = separated_list(COMMA, IDENT) RPAREN body = stmt
        { { name = f;
            formals = formals;
            body = body } }
;