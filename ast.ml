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
  | AeCall of atom * expr list (* Appel d'une fonction, ex. f(e1, ..., en) *)

and block =
    | Sblock of stmt
    | Sblocks of stmt list

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

and param = 
  | Pdots of ident * param_type
  | PFn of funbody

and param_type =
  | PBase of atype
  | PArrow of atype * result
  | PArrowpar of (param_type list) * result
  | PAnnot of ident list * param_type
  | PAFn of funbody

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
  | Sexpr of expr


and decl =
  | Dfun of ident * funbody

and file = decl

and annot = result

and result = ident list * param_type option

(*
(* Pretty-printing *)
open Format

let rec pp_ident fmt i = fprintf fmt "%s" i
and pp_unop fmt = function
  | Not -> fprintf fmt "not"
  | Neg -> fprintf fmt "-"

and pp_binop fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Eq -> fprintf fmt "=="
  | Neq -> fprintf fmt "!="
  | Lt -> fprintf fmt "<"
  | Gt -> fprintf fmt ">"
  | Le -> fprintf fmt "<="
  | Ge -> fprintf fmt ">="
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"

and pp_expr fmt = function
  | Eblock b -> pp_block fmt b
  | Eexpr e -> pp_bexpr fmt e
  | Call (e, params) -> fprintf fmt "%a(%a)" pp_expr e (pp_list pp_expr) params
  | Fn e -> fprintf fmt "fn %a" pp_expr e
  | Block stmts -> fprintf fmt "{ %a }" pp_stmt_list stmts
  (*| ECall (a, exprs) -> fprintf fmt "%a(%a)" pp_atom a (pp_list pp_expr) exprs
  | ECallb (e, exprs) -> fprintf fmt "%a(%a)" pp_expr e (pp_list pp_expr) exprs *)

and pp_block fmt = function
  | Sblock s -> pp_stmt fmt s

and pp_bexpr fmt = function
  | BAtom a -> pp_atom fmt a
  | BNot e -> fprintf fmt "not %a" pp_bexpr e
  | BNeg e -> fprintf fmt "-%a" pp_bexpr e
  | BBinop (e1, op, e2) -> fprintf fmt "%a %a %a" pp_bexpr e1 pp_binop op pp_bexpr e2
  | BAssign (id, e) -> fprintf fmt "%s = %a" id pp_bexpr e
  | BIf (cond, e1, e2) -> fprintf fmt "if %a then %a%a" pp_bexpr cond pp_expr e1 (pp_option (fun fmt -> fprintf fmt " else %a" pp_expr)) e2
  (*| BIfElse (cond, e1, elifs, e2) -> fprintf fmt "if %a then %a%a%a" pp_bexpr cond pp_expr e1 (pp_list (fun fmt (c, e) -> fprintf fmt " elif %a then %a" pp_bexpr c pp_expr e)) elifs (pp_option (fun fmt -> fprintf fmt " else %a" pp_expr)) e2 *)
  | BIfReturn (cond, e) -> fprintf fmt "if %a then return %a" pp_bexpr cond pp_expr e
  | BFun f -> pp_funbody fmt f
  | BReturn e -> fprintf fmt "return %a" pp_expr e

and pp_atom fmt = function
  | AIdent id -> fprintf fmt "%s" id
  | AIntConst i -> fprintf fmt "%d" i
  | AStringConst s -> fprintf fmt "\"%s\"" s
  | ATrue -> fprintf fmt "true"
  | AFalse -> fprintf fmt "false"
  | AUnit -> fprintf fmt "()"
  | AParen e -> fprintf fmt "(%a)" (pp_option pp_expr) e
 (* | ACall (a, exprs) -> fprintf fmt "%a(%a)" pp_atom a (pp_list pp_expr) exprs *)
  | ADot (a, id) -> fprintf fmt "%a.%s" pp_atom a id
  | AFun f -> pp_funbody fmt f
  | ABlock b -> pp_block fmt b
  (*| AArray exprs -> fprintf fmt "[%a]" (pp_list pp_expr) exprs *)

and pp_funbody fmt = function
  | Fbody (params, annot, e) -> fprintf fmt "fun "

and pp_param fmt = function
  | PBase t -> pp_atype fmt t
  | PArrow (t, r) -> fprintf fmt "%a -> %a" pp_atype t pp_result r
 (* | PArrowpar (params, r) -> fprintf fmt "(%a) -> %a" (pp_list pp_param) params pp_result r *)
  | PFn f -> pp_funbody fmt f

and pp_atype fmt = function
  (*| ATypeApp (id, params) -> fprintf fmt "%s%a" id (pp_list pp_param) params *)
  | ATypeParen t -> fprintf fmt "(%a)" pp_atype t
  | AUnit -> fprintf fmt "unit"

and pp_stmt fmt = function
  | Sbexpr e -> pp_bexpr fmt e
  | Sval e -> fprintf fmt "val %a" pp_expr e
  | Svar e -> fprintf fmt "var %a" pp_expr e
  (*| Sifo (cond, e, stmts, opt_stmt) -> fprintf fmt "if %a then %a%a" pp_bexpr cond pp_expr e (pp_option (fun fmt -> fprintf fmt " else %a" pp_stmt)) opt_stmt *)
  | Sif (cond, e, stmts) -> fprintf fmt "if %a then %a" pp_bexpr cond pp_expr e
  (*| SIfElse (cond, e, stmts, opt_stmt) -> fprintf fmt "if %a then %a%a" pp_bexpr cond pp_expr e (pp_option (fun fmt -> fprintf fmt " else %a" pp_stmt)) opt_stmt *)
  | Sassign (id, e) -> fprintf fmt "%s = %a" id pp_expr e
  | Sreturn e -> fprintf fmt "return %a" pp_expr e
  | Sblock b -> pp_block fmt b

and pp_decl fmt = function
  | Dfun (id, f) -> fprintf fmt "fun %s %a" id pp_funbody f

and pp_file fmt f = pp_decl fmt f

and pp_annot fmt = function
  | (ids, opt_type) -> fprintf fmt "asa"

and pp_result fmt = function
  | (ids, opt_type) -> fprintf fmt "asa 94"

and pp_param_type fmt = function
  | PBase t -> pp_atype fmt t
  | PArrow (t, r) -> fprintf fmt "%a -> %a" pp_atype t pp_result r
  (*| PArrowpar (params, r) -> fprintf fmt "(%a) -> %a" (pp_list pp_param_type) params pp_result r *)
  | PFn f -> pp_funbody fmt f

and pp_list pp fmt lst = 
  let rec aux = function
    | [] -> ()
   (* | [x] -> pp fmt x *)
   (* | x :: xs -> fprintf fmt "%a, %a" pp x aux xs *)
  in aux lst

and pp_stmt_list fmt stmts = 
  let rec aux = function
    | [] -> ()
    | [x] -> pp_stmt fmt x
  (*  | x :: xs -> fprintf fmt "%a; %a" pp_stmt x aux xs *)
  in aux stmts

and pp_option pp fmt = function
  | None -> ()
  | Some x -> pp fmt x *)
