/* Analyseur syntaxique pour mini-Koka */

%{
    open Ast 
    open Lexing
%}

/* Déclaration des tokens */
%token <int> CST
%token <string> IDENT
%token <string> STRING
%token <string> UPPER
%token <string> DIGIT
%token <string> OTHER
%token <string> SPACE
%token INDENT DEDENT NEWLINE
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
%token MOD

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
%start file

/* Déclaration des types de retour pour les non-terminaux */
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

/* Les règles de la grammaire */

/* Règle pour le fichier complet */
file:
        SEMI* LPAREN d=decl SEMI+ RPAREN* EOF
                { d }
;

/* Règle pour une déclaration */
decl:
        | FUN i=IDENT f=funbody
                { Dfun(i, f) }
;

/* Règle pour le corps d'une fonction */
funbody:
        | LPAREN p=param* COMMA RPAREN a=annot? e=expr
                { Fbody(p, a, e) }
;

/* Règle pour un paramètre de fonction */
param:
        | IDENT COLON p=param_type
                { (p) }
;

/* Règle pour une annotation */
annot:
        | COLON r=result
                { r }
; 

/* Règle pour le type de paramètre */
param_type:
        | a=atype
                { PBase(a) }
        | a=atype ARROW r=result
                { PArrow(a, r) }
        | LPAREN p=param_type* COMMA RPAREN ARROW r=result
                { PArrowpar(p, r) }
        | FN f=funbody
                { PFn(f) }
        | LESS idents = separated_list(COMMA, IDENT) GREATER p=param_type
            { PAnnot(idents, p) }
;

/* Règle pour le résultat d'une fonction */
result:
        | LESS idents = separated_list(COMMA, IDENT) GREATER p=param_type
                { (idents, Some(p)) }
        | p=param_type
                { ([], Some(p)) }
;

/* Règle pour les types */
atype:
        | i=IDENT LPAREN LESS p=param_type* COMMA GREATER RPAREN?
                { ATypeApp(i, p) }
        | LPAREN a=atype RPAREN
                { ATypeParen a }
        | LPAREN RPAREN
                { AUnit }
;

/* Règle pour les atomes */
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

/* Règle pour les expressions */
expr:
        | b=block
            {Eblock(b) } 
        | a=atom LPAREN RPAREN
            { AeCall(a, []) } (* Désucrage en x() *)
        | b=bexpr
            {Eexpr(b)}
        | e=expr DOT i=IDENT
            { ECall(AIdent(i), [e]) } (* Désucrage en x(e) *)
        | e=expr LPAREN a=param* RPAREN FN f=funbody
            { Call(e, a @ [PFn(f)]) } (* Désucrage en e(e1, ..., en, fn f) *)
        | e=expr FN f=expr
            { match e with
                | ECallb(_, _) -> ECallb(e, [Fn(f)]) (* Si c'est déjà un appel *)
                | _ -> ECallb(e, [Fn(f)]) (* Sinon, transformer en appel *)
            }
        | e=expr LPAREN a=param* RPAREN BEGIN b=funbody END
            { Call(e, a @ [PFn(b)]) } (* Désucrage en e(e1, ..., en, {b}) *)
        | e=expr LPAREN a=param* RPAREN
            { Call(e, a) } (* Désucrage en e(e1, ..., en) *)    
;

/* Règle pour les expressions booléennes */
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

/* Règle pour les blocs */
block:
    | BEGIN SEMI* LPAREN s=stmt SEMI+ RPAREN* END
        { Sblock(s) }
    | BEGIN s=stmt END
        { Sblock(s) }
;

/* Règle pour les instructions */
stmt: 
        | e=bexpr
                { Sbexpr e } 
        | VAL IDENT EQ e=expr
                { Sval (e) }
        | VAR IDENT ASSIGN e=expr
                { Svar (e) } 
;

/* Règle pour les opérateurs binaires */
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