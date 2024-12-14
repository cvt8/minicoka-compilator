(* Syntaxe abstraite pour mini-Coka
*)


type ident = string


type expr =
  | Econst of bool
  | Estring of string
  | Evar of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Ecall of expr * expr list
  | Efun of funbody
  | Eblock of block
  | Earray of expr list
  | Eif of expr * expr * expr list * expr option
  | Ereturn of expr

and unop =
    | Not
    | Neg
  
and binop =
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
  
and funbody = (param list * annot option * expr)
  
and param = ident * param_type
  
and annot = result
  
and result = (ident list * param_type option)
  
and param_type =
    | AIdent of ident
    | ATypeApp of ident * param_type
    | ATypeParen of param_type
    | AUnit
    | AArrow of param_type * result
  
and stmt =
    | Sexpr of expr
    | Sval of ident * expr
    | Svar of ident * expr
    | Sif of expr * expr * expr list * expr option
    | Sreturn of expr
    | Sassign of ident * expr
    | Sfun of funbody
    | Sblock of stmt list
    | Scall of expr * expr list
  
and block = stmt list
  
and file = stmt list

