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

type expr =
  | Econst of bool
  | Estring of string
  | Evar of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of expr * ident * expr list
  | Efun of funbody
  | Eblock of block
  | Earray of expr list
  | Eif of expr * block * block option
  | Ereturn of expr
  | Edot of expr * ident
  | EblockExpr of expr * block

and atom =
  | AIdent of ident
  | AIntConst of int
  | AStringConst of string
  | ABoolConst of bool
  | AUnit

and param = ident * param_type

and param_type =
  | PBase of atype
  | PArrow of atype * atype

and atype =
  | AInt
  | ABool
  | AString
  | AUnit

and stmt =
  | Sval of ident * expr
  | Svar of ident * expr
  | Sassign of ident * expr
  | Sif of expr * block * block option
  | Swhile of expr * block
  | Sreturn of expr
  | Scall of ident * expr list
  | Sblock of block

and funbody = param list * stmt list

and block = stmt list

and decl =
  | Dfun of ident * funbody

and file = decl list