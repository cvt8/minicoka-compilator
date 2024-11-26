/* Analyseur syntaxique pour mini-Turtle */

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
%token COLOR BLACK WHITE RED GREEN BLUE
%token EOF

/* Les priorités et associativités des tokens */

%left MINUS PLUS
%left TIMES DIV
%nonassoc uminus
%nonassoc IF
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

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

expr:
| c = CST                        { Econst c }
| id = IDENT                     { Evar id }
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
| MINUS e = expr %prec uminus    { neg e }
| LPAREN e = expr RPAREN         { e }
;

%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;

color:
| BLACK { Turtle.black }
| WHITE { Turtle.white }
| RED   { Turtle.red   }
| GREEN { Turtle.green }
| BLUE  { Turtle.blue  }
;