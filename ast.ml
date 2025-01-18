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

and expr =
  | Eblock of block
  | Eexpr of bexpr
  | Call of expr * param list (* Appel d'une fonction, ex. e(e1, ..., en) *)
  | Fn of  expr      (* Fonction anonyme avec un argument, ex. fn f *)
  | Block of stmt list       (* Bloc de code, ex. { b } *)
  | ECall of atom * expr list (* Appel d'une fonction, ex. f(e1, ..., en) *)
  | ECallb of expr * expr list (* Appel d'une fonction, ex. e(e1, ..., en) *)

and block =
    | Sblock of stmt

and bexpr =
  | BAtom of atom
  | BNot of bexpr
  | BNeg of bexpr
  | BBinop of bexpr * binop * bexpr
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
  | ATrue
  | AFalse
  | AUnit
  | AParen of expr option
  | ACall of atom * expr list
  | ADot of atom * ident
  | AFun of funbody
  | ABlock of block
  | AArray of expr list
  

and funbody =
    | Fbody of param list * annot option * expr

and param = param_type

and param_type =
  | PBase of atype
  | PArrow of atype * result
  | PArrowpar of (param_type list) * result
  | PFn of funbody

and atype =
    | ATypeApp of ident * param_type list
    | ATypeParen of atype
    | AUnit

and stmt =
  | Sbexpr of bexpr
  | Sval of expr
  | Svar of expr
  | Sifo of bexpr * expr * stmt list * stmt option (* if expr then block else stmt option *)
  | Sif of bexpr * expr * stmt list (* if expr then block else stmt list *)
  | SIfElse of bexpr * expr * stmt list * stmt option (* if expr then block else if expr then block else stmt option *)
  | Sassign of string * expr  (* affectation *)
  | Sreturn of expr          (* return *)
  | Sblock of block          (* bloc de code *)


and decl =
  | Dfun of ident * funbody

and file = decl

and annot = result

and result = ident list * param_type option

and elif = 
  | Elif of bexpr * expr * block option
