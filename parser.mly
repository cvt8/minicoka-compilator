%{
  open Ast
%}

%token <string> ID
%token <int> INT
%token <string> STRING
%token TRUE FALSE UNIT
%token FUN FN VAL VAR IF THEN ELIF ELSE RETURN
%token PLUS MINUS STAR SLASH PERCENT PLUSPLUS
%token LT LE GT GE EQ NEQ AND OR
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COMMA SEMI COLON COLONEQ ARROW DOT
%token EOF

%start <file> file

%nonassoc IF
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS PLUSPLUS
%left STAR SLASH PERCENT
%nonassoc UMINUS FN_POSTFIX DOT_ID

%%

file:
  | SEMI* decls = separated_list(SEMI, decl) EOF { File decls }

decl:
  | FUN id = ID fb = funbody { FunDecl (id, fb) }

funbody:
  | LPAREN params = separated_list(COMMA, param) RPAREN annot = option(annot) e = expr
    { FunBody (params, annot, e) }

param:
  | id = ID COLON t = type_ { Param (id, t) }

annot:
  | COLON r = result { r }

result:
  | t = type_ { Result ([], t) }
  | LPAREN ids = separated_list(COMMA, ID) RPAREN t = type_ { Result (ids, t) }

type_:
  | t = atype { t }
  | t1 = atype ARROW r = result { FunType ([t1], r) }
  | LPAREN ts = separated_nonempty_list(COMMA, type_) RPAREN ARROW r = result
    { FunType (ts, r) }

atype:
  | id = ID { NamedType (id, []) }
  | id = ID LPAREN ts = separated_list(COMMA, type_) RPAREN { NamedType (id, ts) }
  | LPAREN t = type_ RPAREN { t }
  | LBRACK RBRACK { ListType }

atom:
  | TRUE { Bool true }
  | FALSE { Bool false }
  | n = INT { Int n }
  | s = STRING { String s }
  | UNIT { Unit }
  | id = ID { Var id }
  | LPAREN e = bexpr RPAREN 
  { Atom e }
  | a = atom LPAREN args = separated_list(COMMA, expr) RPAREN 
  { Call (a, args) }
  | a = atom DOT id = ID { Dot (a, id) } %prec DOT_ID
  | a = atom FN fb = funbody { Fun (a, fb) } %prec FN_POSTFIX
  | a = atom b = block { BlockApp (a, b) }
  | LBRACK es = separated_list(COMMA, expr) RBRACK { ListLit es }

expr:
  | b = block { Block b }
  | e = bexpr { e }

bexpr:
  | a = atom { Atom a }
  | MINUS e = bexpr %prec UMINUS { UnOp (Neg, e) }
  | e1 = bexpr op = binop e2 = bexpr { BinOp (e1, op, e2) }
  | id = ID COLONEQ e = bexpr { Assign (id, e) }
  | IF c = bexpr THEN t = expr elifs = list(elif_clause) el = option(else_clause)
    { If (c, t, elifs, el) }
  | IF c = bexpr RETURN e = expr { IfReturn (c, e) }
  | FN fb = funbody { Lambda fb }
  | RETURN e = expr { Return e }

elif_clause:
  | ELIF c = bexpr THEN e = expr { (c, e) }

else_clause:
  | ELSE e = expr { e }

block:
  | LBRACE SEMI* stmts = separated_nonempty_list(SEMI, stmt) RBRACE { stmts }

stmt:
  | e = bexpr { ExprStmt e }
  | VAL id = ID EQ e = expr { ValDecl (id, e) }
  | VAR id = ID COLONEQ e = expr { VarDecl (id, e) }

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | PERCENT { Mod }
  | PLUSPLUS { Concat }
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }

%inline
separated_nonempty_list(sep, X):
  | x = X { [x] }
  | x = X sep xs = separated_nonempty_list(sep, X) { x :: xs }