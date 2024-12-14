(* Syntaxe abstraite pour mini-Coka
*)

(* expressions entières *)

(* ast.ml *)

type expr =
  | Econst of int
  | Evar of string
  | Ebool of bool
  | Ebinop of binop * expr * expr
  | Enot of expr
  | Ecall of string * expr list

and binop = 
  | Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | And | Or

type stmt =
  | Sval of string * expr
  | Svar of string * expr
  | Sexpr of expr
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Scall of string * expr list
  | Sblock of stmt list

type program = { defs: decl list }

and decl = { name: string; body: stmt }

