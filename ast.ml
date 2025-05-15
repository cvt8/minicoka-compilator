type ident = string

type binop =
  | Add | Sub | Mul | Div | Mod | Concat
  | Lt | Le | Gt | Ge | Eq | Neq | And | Or

type unop = Neg

type expr =
  | Block of stmt list
  | Atom of atom
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | Assign of ident * expr
  | If of expr * expr * (expr * expr) list * expr option
  | IfReturn of expr * expr
  | Lambda of funbody
  | Return of expr

and atom =
  | Bool of bool
  | Int of int
  | String of string
  | Unit
  | Var of ident
  | Call of atom * expr list
  | Dot of atom * ident
  | Fun of atom * funbody
  | BlockApp of atom * stmt list
  | ListLit of expr list

and stmt =
  | ExprStmt of expr
  | ValDecl of ident * expr
  | VarDecl of ident * expr

and funbody = FunBody of param list * result option * expr

and param = Param of ident * type_

and result = Result of ident list * type_

and type_ =
  | NamedType of ident * type_ list
  | FunType of type_ list * result
  | ListType

type decl = FunDecl of ident * funbody

type file = File of decl list