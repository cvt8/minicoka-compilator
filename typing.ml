open Ast

type eff_kind = Div | Console
type comp_type = eff_kind list * type_
type value_type = type_
type typing_env = (ident * value_type) list

exception TypeError of string * Lexing.position

let rec typecheck_expr env e pos =
  match e with
  | Block stmts -> typecheck_block env stmts pos
  | Atom a -> typecheck_atom env a pos
  | UnOp (Neg, e') ->
      let (eff, t) = typecheck_expr env e' pos in
      if t = NamedType ("int", []) then (eff, t)
      else raise (TypeError ("Negation expects int", pos))
  | BinOp (e1, op, e2) ->
      let (eff1, t1) = typecheck_expr env e1 pos in
      let (eff2, t2) = typecheck_expr env e2 pos in
      begin match op with
      | Add | Sub | Mul | Div | Mod ->
          if t1 = NamedType ("int", []) && t2 = NamedType ("int", []) then
            (List.sort_uniq compare (eff1 @ eff2), NamedType ("int", []))
          else
            raise (TypeError ("Arithmetic operation expects two ints", pos))
      | Concat ->
          if (t1 = NamedType ("string", []) && t2 = NamedType ("string", [])) ||
             (t1 = ListType && t2 = ListType) then
            (List.sort_uniq compare (eff1 @ eff2), t1)
          else
            raise (TypeError ("Concatenation expects two strings or lists", pos))
      | Lt | Le | Gt | Ge | Eq | Neq ->
          if t1 = t2 then
            (List.sort_uniq compare (eff1 @ eff2), NamedType ("bool", []))
          else
            raise (TypeError ("Comparison expects same types", pos))
      | And | Or ->
          if t1 = NamedType ("bool", []) && t2 = NamedType ("bool", []) then
            (List.sort_uniq compare (eff1 @ eff2), NamedType ("bool", []))
          else
            raise (TypeError ("Logical operation expects two bools", pos))
      end
  | Assign (id, e') ->
      let (eff, t) = typecheck_expr env e' pos in
      begin match List.assoc_opt id env with
      | Some (NamedType ("var", [t'])) when t = t' ->
          (eff, NamedType ("unit", []))
      | _ -> raise (TypeError ("Assignment to undefined or immutable variable", pos))
      end
  | If (cond, then_expr, elifs, else_expr) ->
      let (eff_c, t_c) = typecheck_expr env cond pos in
      if t_c <> NamedType ("bool", []) then
        raise (TypeError ("If condition must be bool", pos));
      let (eff_t, t_t) = typecheck_expr env then_expr pos in
      let eff_elif, t_elif = List.fold_left (fun (eff_acc, t_acc) (c, e) ->
        let (eff_c, t_c) = typecheck_expr env c pos in
        let (eff_e, t_e) = typecheck_expr env e pos in
        if t_c <> NamedType ("bool", []) then
          raise (TypeError ("Elif condition must be bool", pos));
        if t_acc <> t_e then
          raise (TypeError ("Elif branch type mismatch", pos));
        (List.sort_uniq compare (eff_acc @ eff_c @ eff_e), t_e)
      ) (eff_t, t_t) elifs in
      let (eff_e, t_e) = match else_expr with
        | Some e -> typecheck_expr env e pos
        | None -> ([], NamedType ("unit", []))
      in
      if t_elif <> t_e then
        raise (TypeError ("Else branch type mismatch", pos));
      (List.sort_uniq compare (eff_c @ eff_elif @ eff_e), t_elif)
  | IfReturn (cond, e') ->
      let (eff_c, t_c) = typecheck_expr env cond pos in
      let (eff_e, t_e) = typecheck_expr env e' pos in
      if t_c <> NamedType ("bool", []) then
        raise (TypeError ("If-return condition must be bool", pos));
      (List.sort_uniq compare (eff_c @ eff_e), t_e)
  | Lambda fb -> typecheck_funbody env fb pos
  | Return e' ->
      let (eff, t) = typecheck_expr env e' pos in
      (eff, t)

and typecheck_atom env a pos =
  match a with
  | Bool _ -> ([], NamedType ("bool", []))
  | Int _ -> ([], NamedType ("int", []))
  | String _ -> ([], NamedType ("string", []))
  | Unit -> ([], NamedType ("unit", []))
  | Var id ->
      begin match List.assoc_opt id env with
      | Some t -> ([], t)
      | None -> raise (TypeError ("Undefined variable: " ^ id, pos))
      end
  | Call (a, args) ->
      let (eff_a, t_a) = typecheck_atom env a pos in
      let eff_args, t_args = List.fold_left (fun (eff_acc, t_acc) arg ->
        let (eff, t) = typecheck_expr env arg pos in
        (List.sort_uniq compare (eff_acc @ eff), t :: t_acc)
      ) ([], []) args in
      begin match t_a with
      | FunType (params, Result (_, ret)) when List.length params = List.length t_args && List.for_all2 (fun t1 t2 -> t1 = t2) params t_args ->
          (List.sort_uniq compare (eff_a @ eff_args), ret)
      | _ -> raise (TypeError ("Invalid function call", pos))
      end
  | Dot (a, id) ->
      let (eff, t) = typecheck_atom env a pos in
      (* Translate e.x to x(e) *)
      let x = Var id in
      let (eff_x, t_x) = typecheck_atom env x pos in
      begin match t_x with
      | FunType ([t'], Result (_, ret)) when t' = t ->
          (List.sort_uniq compare (eff @ eff_x), ret)
      | _ -> raise (TypeError ("Invalid dot operation", pos))
      end
  | Fun (a, fb) ->
      let (eff_a, t_a) = typecheck_atom env a pos in
      let (eff_fb, t_fb) = typecheck_funbody env fb pos in
      (* Translate e fn f to e(fn f) *)
      begin match t_a with
      | FunType ([FunType (params, Result (_, ret))], Result (_, ret')) when ret = t_fb ->
          (List.sort_uniq compare (eff_a @ eff_fb), ret')
      | _ -> raise (TypeError ("Invalid fn application", pos))
      end
  | BlockApp (a, stmts) ->
      let (eff_a, t_a) = typecheck_atom env a pos in
      let (eff_b, t_b) = typecheck_block env stmts pos in
      (* Translate e{b} to e(fn{b}) *)
      let fb = FunBody ([], None, Block stmts) in
      let (eff_fb, t_fb) = typecheck_funbody env fb pos in
      begin match t_a with
      | FunType ([FunType ([], Result (_, ret))], Result (_, ret')) when ret = t_b ->
          (List.sort_uniq compare (eff_a @ eff_b @ eff_fb), ret')
      | _ -> raise (TypeError ("Invalid block application", pos))
      end
  | ListLit es ->
      let eff, ts = List.fold_left (fun (eff_acc, t_acc) e ->
        let (eff, t) = typecheck_expr env e pos in
        (List.sort_uniq compare (eff_acc @ eff), t :: t_acc)
      ) ([], []) es in
      let t = match ts with
        | [] -> ListType
        | t' :: rest -> if List.for_all (fun t'' -> t'' = t') rest then ListType else raise (TypeError ("List elements must have same type", pos))
      in
      (eff, t)

and typecheck_block env stmts pos =
  match stmts with
  | [] -> ([], NamedType ("unit", []))
  | [ExprStmt e] -> typecheck_expr env e pos
  | ExprStmt e :: rest ->
      let (eff, _) = typecheck_expr env e pos in
      let (eff_rest, t_rest) = typecheck_block env rest pos in
      (List.sort_uniq compare (eff @ eff_rest), t_rest)
  | ValDecl (id, e) :: rest ->
      let (eff, t) = typecheck_expr env e pos in
      let env' = (id, t) :: env in
      let (eff_rest, t_rest) = typecheck_block env' rest pos in
      (List.sort_uniq compare (eff @ eff_rest), t_rest)
  | VarDecl (id, e) :: rest ->
      let (eff, t) = typecheck_expr env e pos in
      let env' = (id, NamedType ("var", [t])) :: env in
      let (eff_rest, t_rest) = typecheck_block env' rest pos in
      (List.sort_uniq compare (eff @ eff_rest), t_rest)
  | _ -> raise (TypeError ("Block must end with an expression", pos))

and typecheck_funbody env (FunBody (params, annot, body)) pos =
  let env_params = List.map (fun (Param (id, t)) -> (id, t)) params in
  let env' = env_params @ env in
  let (eff, t) = typecheck_expr env' body pos in
  let t_expected = match annot with
    | Some (Result (_, t')) -> t'
    | None -> t
  in
  if t <> t_expected then
    raise (TypeError ("Function body type does not match annotation", pos));
  let has_recursive_call = ref false in
  let rec check_recursive e id =
    match e with
    | Atom (Var id') when id' = id -> has_recursive_call := true
    | Atom (Call (a, args)) ->
        check_recursive_atom a id; List.iter (fun e' -> check_recursive e' id) args
    | Atom _ -> ()
    | Block stmts -> List.iter (fun s -> check_recursive_stmt s id) stmts
    | UnOp (_, e') -> check_recursive e' id
    | BinOp (e1, _, e2) -> check_recursive e1 id; check_recursive e2 id
    | Assign (_, e') -> check_recursive e' id
    | If (c, t, elifs, el) ->
        check_recursive c id; check_recursive t id;
        List.iter (fun (c', e') -> check_recursive c' id; check_recursive e' id) elifs;
        Option.iter (fun e' -> check_recursive e' id) el
    | IfReturn (c, e') -> check_recursive c id; check_recursive e' id
    | Lambda fb -> check_recursive_funbody fb id
    | Return e' -> check_recursive e' id
  and check_recursive_atom a id =
    match a with
    | Var id' when id' = id -> has_recursive_call := true
    | Call (a', args) -> check_recursive_atom a' id; List.iter (fun e' -> check_recursive e' id) args
    | Dot (a', _) -> check_recursive_atom a' id
    | Fun (a', fb) -> check_recursive_atom a' id; check_recursive_funbody fb id
    | BlockApp (a', stmts) -> check_recursive_atom a' id; List.iter (fun s -> check_recursive_stmt s id) stmts
    | _ -> ()
  and check_recursive_stmt s id =
    match s with
    | ExprStmt e -> check_recursive e id
    | ValDecl (_, e) -> check_recursive e id
    | VarDecl (_, e) -> check_recursive e id
  and check_recursive_funbody (FunBody (_, _, e)) id = check_recursive e id
  in
  check_recursive body (List.hd env_params |> fst);
  let eff' = if !has_recursive_call then List.sort_uniq compare (Div :: eff) else eff in
  (eff', FunType (List.map (fun (Param (_, t)) -> t) params, Result ([], t_expected)))

let typecheck_file (File decls) =
  let rec process_decls env = function
    | [] -> ()
    | FunDecl (id, fb) :: rest ->
        let pos = Lexing.dummy_pos (* Adjust to get actual position from parser *) in
        let (eff, t) = typecheck_funbody env fb pos in
        if List.mem_assoc id env then
          raise (TypeError ("Function " ^ id ^ " redefined", pos));
        let env' = (id, t) :: env in
        process_decls env' rest
    | _ -> raise (TypeError ("Invalid declaration", Lexing.dummy_pos))
  in
  let has_main = List.exists (function
    | FunDecl ("main", FunBody ([], _, _)) -> true
    | _ -> false
  ) decls in
  if not has_main then
    raise (TypeError ("No main function defined", Lexing.dummy_pos));
  process_decls [] decls