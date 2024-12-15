(* Syntaxe abstraite pour mini-Koka *)

type ident = string

type unop =
  | Not
  | Neg

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or

type atom =
  | AIdent of ident
  | AIntConst of int
  | AStringConst of string
  | ABoolConst of bool
  | AUnit
  | AParenExpr of expr
  | AApp of atom * expr list
  | ADot of atom * ident
  | AFn of atom * funbody
  | ABlock of block
  | AArray of expr list

and result = (ident list * param_type option)

and annot = result

 
and param_type =
  | AIdent of ident
  | ATypeApp of ident * param_type
  | ATypeParen of param_type
  | AUnit
  | AArrow of param_type * result list

and param = ident * param_type

and stmt =
  | Bbexpr of bexpr
  | Sval of ident * expr
  | Svar of ident * expr
  | Sif of expr * expr * expr list * expr option
  | Sreturn of expr
  | Sassign of ident * expr
  | Sfun of funbody
  | Sblock of stmt list
  | Scall of expr * expr list
  | Sbexpr of bexpr
  | Selif of bexpr * stmt * stmt list  (* ... elif ... *)
  | Sdot of expr * ident  (* e . x *)
  | Sfn of expr * funbody  (* e fn f *)
  | SfnCall of expr * expr list * funbody  (* e(e1, ..., en) fn f *)
  | SblockExpr of expr * block  (* e { b } *)

and block = stmt list


and bexpr =
    | Batom of atom
    | Bminus of bexpr
    | Bnot of bexpr
    | Bbinop of binop * bexpr * bexpr
    | Bassign of ident * bexpr
    | Bif of bexpr * expr * expr list * expr option
    | Breturn of expr
    | Bfn of funbody

and expr =
  | Econst of bool
  | Estring of string
  | Ebexpr of bexpr
  | Evar of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of expr * ident * expr list
  | Efun of funbody
  | Eblock of block
  | Earray of expr list
  | Eif of expr * expr * expr list * expr option
  | Ereturn of expr
  | Edot of expr * ident  (* e . x *)
  | Efn of expr * funbody  (* e fn f *)
  | EfnCall of expr * expr list * funbody  (* e(e1, ..., en) fn f *)
  | EblockExpr of expr * block  (* e { b } *)

and funbody = (param list * annot option * expr)

and decl = Sfun of ident * funbody

and file = decl list


and atype =
  | AInt
  | ABool
  | AString
  | AUnit
  | AFun of atype * atype
  | AArray of atype
  | AIdent of ident
  | ATypeApp of ident * param_type
  | ATypeParen of param_type