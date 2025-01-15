(* Interpréteur de mini Cola *)

open Ast

(*
(* Evaluate an expression *)
let rec eval_expr env = function
  | AIntConst c -> c
  | Evar v -> (try Hashtbl.find env v with Not_found -> failwith ("Variable " ^ v ^ " non définie"))
  | Ebool b -> if b then 1 else 0
  | Ebinop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
      | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> if v2 != 0 then v1 / v2 else failwith "Division par zéro"
      | Eq -> if v1 = v2 then 1 else 0
      | Neq -> if v1 != v2 then 1 else 0
      | Lt -> if v1 < v2 then 1 else 0
      | Gt -> if v1 > v2 then 1 else 0
      | And -> if v1 != 0 && v2 != 0 then 1 else 0
      | Or -> if v1 != 0 || v2 != 0 then 1 else 0)
  | Enot e -> if eval_expr env e = 0 then 1 else 0
  | Ecall (name, args) ->
      let func = try Hashtbl.find env name with Not_found -> failwith ("Fonction " ^ name ^ " non définie") in
      let arg_values = List.map (eval_expr env) args in
      func arg_values

(* Execute a statement *)
let rec exec_stmt env = function
  | Sval (id, e) -> let value = eval_expr env e in Hashtbl.add env id value
  | Svar (id, e) -> let value = eval_expr env e in Hashtbl.add env id value
  | Sexpr e -> eval_expr env e |> ignore
  | Scall (fn, args) ->
      let func = try Hashtbl.find env fn with Not_found -> failwith ("Fonction " ^ fn ^ " non définie") in
      let arg_values = List.map (eval_expr env) args in
      ignore (func arg_values)
  | Sif (cond, s1, s2) ->
      if eval_expr env cond != 0 then exec_stmt env s1 else exec_stmt env s2
  | Sreturn e -> eval_expr env e
  | Scall (fn, args) -> failwith "Appels de fonctions non implémentés"
  | Sblock stmts -> List.iter (exec_stmt env) stmts 

(* Execute a program *)
let exec_program prog =
  let env = Hashtbl.create 10 in
  List.iter (fun { name; body } -> exec_stmt env body) prog.defs *)

let exec_program prog = ()

exception Error of string

let error s = raise (Error s)