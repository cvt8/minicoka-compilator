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

and block =
    | Bstmt of stmt
    | Bbegin of stmt list

and expr =
  | Econst of bool
  | Estring of string
  | Evar of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of ident * expr list
  | Efun of funbody
  | Eblock of block
  | Earray of expr list
  | Eif of expr * block * block option
  | Ereturn of expr
  | Edot of expr * ident
  | EblockExpr of expr * block

and bexpr =
  | BAtom of atom
  | BNot of bexpr
  | BNeg of bexpr
  | BBinop of binop * bexpr * bexpr
  | BAssign of ident * bexpr
  | BIf of bexpr * expr * expr option
  | BIfElse of bexpr * expr * (bexpr * expr) list * expr option
  | BIfReturn of bexpr * expr
  | BFun of funbody
  | BReturn of expr

and atom =
  | AIdent of ident
  | AIntConst of int
  | AStringConst of string
  | ABoolConst of bool
  | AUnit
  | AParen of expr option
  | ACall of atom * expr list
  | ADot of atom * ident
  | AFun of funbody
  | ABlock of block
  | AArray of expr list
  

and funbody =
    | Fbody of param list * annot option * expr

and param = ident * param_type

and param_type =
  | PBase of atype
  | PArrow of param_type * result
  | PList of param_type list

and atype =
    | ATypeApp of ident * param_type list
    | ATypeParen of atype
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
  | Sparam of param  


and decl =
  | Dfun of ident * funbody
  | Dparam of param 

and file = decl list

and annot = result

and result = ident list * param_type option