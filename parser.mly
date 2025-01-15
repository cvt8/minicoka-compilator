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
(*%type <Ast.elif> elif*)
%%

(* Les règles de la grammaire *)

file:
    SEMI* LPAREN d=decl SEMI+ RPAREN* EOF
        { d }
;

decl:
    | FUN i=IDENT f=funbody
        { Dfun(i, f) }
;

funbody:
    | LPAREN p=param* COMMA RPAREN a=annot? e=expr
        { Fbody(p, a, e) }
;

param:
    | IDENT COLON p=param_type
        { (p) }
;

annot:
    | COLON r=result
        { r }
; 

param_type:
    | a=atype
        { PBase(a) }
    | a=atype ARROW r=result
        { PArrow(a, r) }
    | LPAREN p=param_type* COMMA RPAREN ARROW r=result
        { PArrowpar(p, r) }
;

result:
    | LESS idents = separated_list(COMMA, IDENT) GREATER p=param_type
        { (idents, Some(p)) }
    | p=param_type
        { ([], Some(p)) }
;

atype:
    | i=IDENT LPAREN LESS p=param_type* COMMA GREATER RPAREN?
        { ATypeApp(i, p) }
    | LPAREN a=atype RPAREN
        { ATypeParen a }
    | LPAREN RPAREN
        { AUnit }
;

atom:
    | i=IDENT
        { AIdent i }
    | c=CST
        { AIntConst (c) }
    | s=STRING
        { AStringConst s }
    | TRUE
        { ATrue }
    | FALSE
        { AFalse }
    | LPAREN e=expr RPAREN
        { AParen (Some e) }
    | LPAREN RPAREN
        { AUnit }
    | a=atom DOT i=IDENT
        { ADot (a, i) }
    | a=atom LPAREN expr_list = separated_list(COMMA, expr) RPAREN
        { ACall (a, expr_list) }
    | FN f=funbody
        { AFun f }
    | BEGIN b=block END
        { ABlock b }
    | LBRACKET expr_list = separated_list(COMMA, expr) RBRACKET
        { AArray expr_list }
;

expr:
    | b=block
        {Eblock(b) } 
    | b=bexpr
        {Eexpr(b)}
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
    | a=atom
        { BAtom(a) }
    | TILDE b=bexpr
        { BNot(b) }
    | BANG b=bexpr
        { BNeg(b) }
    | b=bexpr c=binop d=bexpr
        { BBinop (b, c, d) }
    | i=IDENT ASSIGN b=bexpr
        { BAssign (i, b) }
    | IF b=bexpr THEN e=expr ELSE f=expr
        { BIf (b, e, Some f) }
    | IF b=bexpr THEN e=expr LPAREN ELIF c=bexpr THEN f=expr RPAREN* LPAREN ELSE g=expr RPAREN?
        { BIfElse (b, e, [(c, f)], Some g) }
    | IF b=bexpr RETURN e=expr
        { BIfReturn (b, e) }
    | FN f=funbody
        { BFun f }
    | RETURN e=expr
        { BReturn e }
;

block:
    | BEGIN SEMI* LPAREN s=stmt SEMI+ RPAREN* END
        { Sblock(s) }
;

stmt: 
    | e=bexpr
        { Sbexpr e } 
    | VAL IDENT EQ e=expr
        { Sval (e) }
    | VAR IDENT ASSIGN e=expr
        { Svar (e) } 
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
    | EQEQ 
        { Eq }
    | NOTEQ 
        { Neq }
    | LESSEQ 
        { Le }
    | GREATEREQ 
        { Ge }
    | LESS 
        { Lt }
    | GREATER 
        { Gt }
    | PLUS 
        { Add }
    | MINUS 
        { Sub }
    | TIMES 
        { Mul }
    | DIV 
        { Div }
    | ANDAND 
        { And }
    | OROR 
        { Or }
    | PLUSPLUS
        { Add }
;
(*elif:
    | ELSE IF bexpr THEN stmt
        { Sif (bexpr, stmt, [], None) }
    | ELSE IF bexpr THEN stmt ELSE stmt
        { Sif (bexpr, stmt, [stmt], None) }
    
    expr 
        IF ifexpr
    ifexpr:
        cond = expr THEN e1=expr ELIF ifexpr
                                ELSE expr
;*)