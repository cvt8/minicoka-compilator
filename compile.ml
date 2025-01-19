

open Format
open X86_64
open Ast
open Typing

(* compile.ml *)

open Ast
open Typing
open X86_64

let rec convert_ast_to_typing_expr = function
  | Eblock _ -> EUnit
  | Eexpr bexpr -> convert_bexpr_to_typing_expr bexpr
  | Call (e, params) -> EApp (convert_ast_to_typing_expr e, List.map convert_ast_to_typing_expr params)
  | Fn e -> convert_ast_to_typing_expr e
  | Block stmts -> convert_block_to_typing_expr (Sblock (List.hd stmts))
  | ECall (atom, exprs) -> EApp (convert_atom_to_typing_expr atom, List.map convert_ast_to_typing_expr exprs)
  | ECallb (e, exprs) -> EApp (convert_ast_to_typing_expr e, List.map convert_ast_to_typing_expr exprs)

and convert_bexpr_to_typing_expr = function
  | BAtom atom -> convert_atom_to_typing_expr atom
  | BNot bexpr -> EBinop ("not", convert_bexpr_to_typing_expr bexpr, EBool false)
  | BNeg bexpr -> EBinop ("-", EInt 0, convert_bexpr_to_typing_expr bexpr)
  | BBinop (bexpr1, op, bexpr2) -> EBinop (convert_binop_to_string op, convert_bexpr_to_typing_expr bexpr1, convert_bexpr_to_typing_expr bexpr2)
  | BAssign (ident, bexpr) -> ELet (ident, convert_bexpr_to_typing_expr bexpr, EVar ident)
  | BIf (cond, then_expr, else_expr_opt) ->
      EIf (convert_bexpr_to_typing_expr cond, convert_ast_to_typing_expr then_expr, Option.map convert_ast_to_typing_expr else_expr_opt |> Option.get |> convert_ast_to_typing_expr)
  | BIfElse (cond, then_expr, elif_list, else_expr_opt) ->
      let rec aux cond then_expr elif_list else_expr_opt =
        match elif_list with
        | [] -> EIf (cond, then_expr, else_expr_opt |> Option.map convert_ast_to_typing_expr |> Option.get |> convert_ast_to_typing_expr)
        | (elif_cond, elif_expr) :: rest -> EIf (cond, then_expr, aux elif_cond elif_expr rest else_expr_opt)
      in aux (convert_bexpr_to_typing_expr cond) (convert_ast_to_typing_expr then_expr) elif_list else_expr_opt
  | BIfReturn (cond, expr) -> EIf (convert_bexpr_to_typing_expr cond, expr, EUnit)
  | BFun funbody -> convert_funbody_to_typing_expr funbody
  | BReturn expr -> expr

and convert_atom_to_typing_expr = function
  | AIdent ident -> EVar ident
  | AIntConst i -> EInt i
  | AStringConst s -> EString s
  | ATrue -> EBool true
  | AFalse -> EBool false
  | AUnit -> EUnit
  | AParen expr_opt -> Option.map convert_ast_to_typing_expr expr_opt |> Option.get |> convert_ast_to_typing_expr
  | ACall (atom, exprs) -> EApp (convert_atom_to_typing_expr atom, List.map convert_ast_to_typing_expr exprs)
  | ADot (atom, ident) -> failwith "Dot notation not supported"
  | AFun funbody -> convert_funbody_to_typing_expr funbody
  | ABlock block -> convert_block_to_typing_expr block
  | AArray exprs -> EList (List.map convert_ast_to_typing_expr exprs)

and convert_funbody_to_typing_expr = function
  | Fbody (params, annot_opt, expr) ->
      let args = List.map (function PBase (ATypeApp (_, [PBase atype])) -> convert_atype_to_value_type atype | _ -> failwith "Unsupported param type") params
      in EFunc (List.map (function PBase (ATypeApp (_, [PBase atype])) -> convert_atype_to_value_type atype | _ -> failwith "Unsupported param type") params, args, convert_ast_to_typing_expr expr)

and convert_atype_to_value_type = function
  | ATypeApp (_, [PBase atype]) -> convert_atype_to_value_type atype
  | ATypeParen atype -> convert_atype_to_value_type atype
  | AUnit -> Tunit

and convert_binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

and convert_block_to_typing_expr = function
  | Sblock stmt -> convert_stmt_to_typing_expr stmt

and convert_stmt_to_typing_expr = function
  | Sbexpr bexpr -> convert_bexpr_to_typing_expr bexpr
  | Sval expr -> convert_ast_to_typing_expr expr
  | Svar expr -> convert_ast_to_typing_expr expr
  | Sifo (cond, then_expr, else_stmts, else_stmt_opt) ->
      EIf (convert_bexpr_to_typing_expr cond, convert_ast_to_typing_expr then_expr, else_stmt_opt |> Option.map convert_stmt_to_typing_expr |> Option.get |> convert_stmt_to_typing_expr)
  | Sif (cond, then_expr, else_stmts) ->
      EIf (convert_bexpr_to_typing_expr cond, convert_ast_to_typing_expr then_expr, else_stmts |> List.map convert_stmt_to_typing_expr |> List.hd |> convert_stmt_to_typing_expr)
  | SIfElse (cond, then_expr, else_stmts, else_stmt_opt) ->
      let rec aux cond then_expr else_stmts else_stmt_opt =
        match else_stmts with
        | [] -> EIf (cond, then_expr, else_stmt_opt |> Option.map convert_stmt_to_typing_expr |> Option.get |> convert_stmt_to_typing_expr)
        | else_stmt :: rest -> EIf (cond, then_expr, aux else_stmt rest else_stmt_opt)
      in aux (convert_bexpr_to_typing_expr cond) (convert_ast_to_typing_expr then_expr) else_stmts else_stmt_opt
  | Sassign (ident, expr) -> ELet (ident, convert_ast_to_typing_expr expr, EVar ident)
  | Sreturn expr -> convert_ast_to_typing_expr expr
  | Sblock block -> convert_block_to_typing_expr block

let type_check env file =
  match file with
  | Dfun (ident, funbody) ->
      let typing_expr = convert_funbody_to_typing_expr funbody in
      let _ = type_expr env typing_expr in ()

let initial_env = []

let type_check_file file =
  type_check initial_env file

(*Production de code*)

let rec compile_expr = function
  | Eblock _ -> nop
  | Eexpr bexpr -> compile_bexpr bexpr
  | Call (e, params) -> List.fold_left (fun acc param -> acc ++ compile_expr param) nop params
  | Fn e -> compile_expr e
  | Block stmts -> List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop stmts
  | ECall (atom, exprs) -> List.fold_left (fun acc expr -> acc ++ compile_expr expr) nop exprs
  | ECallb (e, exprs) -> List.fold_left (fun acc expr -> acc ++ compile_expr expr) nop exprs

and compile_bexpr = function
  | BAtom atom -> compile_atom atom
  | BNot bexpr -> compile_bexpr bexpr
  | BNeg bexpr -> compile_bexpr bexpr
  | BBinop (bexpr1, op, bexpr2) -> compile_bexpr bexpr1 ++ compile_bexpr bexpr2
  | BAssign (ident, bexpr) -> compile_bexpr bexpr
  | BIf (cond, then_expr, else_expr_opt) ->
      compile_bexpr cond ++ compile_expr then_expr ++ Option.fold ~none:nop ~some:compile_expr else_expr_opt
  | BIfElse (cond, then_expr, elif_list, else_expr_opt) ->
      compile_bexpr cond ++ compile_expr then_expr ++
      List.fold_left (fun acc (elif_cond, elif_expr) -> acc ++ compile_bexpr elif_cond ++ compile_expr elif_expr) nop elif_list ++
      Option.fold ~none:nop ~some:compile_expr else_expr_opt
  | BIfReturn (cond, expr) -> compile_bexpr cond ++ compile_expr expr
  | BFun funbody -> compile_funbody funbody
  | BReturn expr -> compile_expr expr

and compile_atom = function
  | AIdent ident -> nop
  | AIntConst i -> movq (imm i) (!%rax)
  | AStringConst s -> nop
  | ATrue -> movq (imm 1) (!%rax)
  | AFalse -> movq (imm 0) (!%rax)
  | AUnit -> movq (imm 0) (!%rax)
  | AParen expr_opt -> Option.fold ~none:nop ~some:compile_expr expr_opt
  | ACall (atom, exprs) -> List.fold_left (fun acc expr -> acc ++ compile_expr expr) nop exprs
  | ADot (atom, ident) -> nop
  | AFun funbody -> compile_funbody funbody
  | ABlock block -> compile_block block
  | AArray exprs -> List.fold_left (fun acc expr -> acc ++ compile_expr expr) nop exprs

  and compile_funbody = function
  | Fbody (params, annot_opt, expr) ->
      let allocate_closure =
        List.fold_left (fun acc param -> acc ++ pushq (!%rbp)) nop params ++
        movq (imm (List.length params * 8)) (!%rax) ++
        inline "subq %rax, %rsp" ++
        List.fold_left (fun acc param -> acc ++ popq (!%rbp) ++ movq (!%rbp) (ind ~ofs:0 (!%rsp))) nop params
      in
      let initialize_closure =
        List.fold_left (fun acc param -> acc ++ movq (!%rbp) (ind ~ofs:(List.length params * 8) (!%rsp))) nop params
      in
      let call_closure =
        movq (ind ~ofs:0 (!%rsp)) (!%rax) ++
        inline "call *%rax" ++
        inline "addq $8, %rsp"
      in
      allocate_closure ++ initialize_closure ++ compile_expr expr ++ call_closure

and compile_param = function
  | PBase atype -> compile_atype atype
  | PArrow (atype, result) -> compile_atype atype ++ compile_result result
  | PArrowpar (param_types, result) -> List.fold_left (fun acc param_type -> acc ++ compile_param param_type) nop param_types ++ compile_result result
  | PFn funbody -> compile_funbody funbody


and compile_atype = function
  | ATypeApp (ident, param_types) -> List.fold_left (fun acc param_type -> acc ++ compile_param param_type) nop param_types
  | ATypeParen atype -> compile_atype atype
  | AUnit -> nop

and compile_result = function
  | (idents, param_type_opt) ->
      List.fold_left (fun acc ident -> acc ++ inline ident) nop idents ++
      Option.fold ~none:nop ~some:compile_param param_type_opt

and compile_stmt = function
  | Sbexpr bexpr -> compile_bexpr bexpr
  | Sval expr -> compile_expr expr
  | Svar expr -> compile_expr expr
  | Sifo (cond, then_expr, else_stmts, else_stmt_opt) ->
      compile_bexpr cond ++ compile_expr then_expr ++
      List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop else_stmts ++
      Option.fold ~none:nop ~some:compile_stmt else_stmt_opt
  | Sif (cond, then_expr, else_stmts) ->
      compile_bexpr cond ++ compile_expr then_expr ++
      List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop else_stmts
  | SIfElse (cond, then_expr, else_stmts, else_stmt_opt) ->
      compile_bexpr cond ++ compile_expr then_expr ++
      List.fold_left (fun acc stmt -> acc ++ compile_stmt stmt) nop else_stmts ++
      Option.fold ~none:nop ~some:compile_stmt else_stmt_opt
  | Sassign (ident, expr) -> compile_expr expr
  | Sreturn expr -> compile_expr expr
  | Sblock block -> compile_block block

and compile_block = function
  | Sblock stmt -> compile_stmt stmt

let compile_to_x86 file =
  match file with
  | Dfun (ident, funbody) ->
      let text = compile_funbody funbody in
      let data = nop in
      { text; data }

