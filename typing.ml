(* typing.ml *)

open Ast

(* Définition des effets possibles d'une expression *)
type effect =
  | Nothing (* Pas d'effet *)
  | Div (* Division par zéro possible *)
  | Console (* Effet console, comme println *)
  | Div_and_Console (* Combinaison des effets Division et Console *)

(* Type de calcul, composé d'un type de valeur et d'un effet *)
type calculation_type = value_type * effect

(* Définition des types de valeurs possibles *)
and value_type =
  | Tunit (* Type unité *)
  | Tbool (* Type booléen *)
  | Tint (* Type entier *)
  | Tstring (* Type chaîne de caractères *)
  | TList of value_type (* Type liste paramétré par le type des éléments *)
  | Tmaybe of value_type (* Type optionnel paramétré par le type de la valeur *)
  | Tfunc of value_type list * calculation_type (* Type fonction avec liste de types d'arguments et type de retour *)

(* Définition des expressions possibles *)
and expr =
    | EUnit (* Expression unité *)
    | EBool of bool (* Expression booléenne *)
    | EInt of int (* Expression entière *)
    | EString of string (* Expression chaîne de caractères *)
    | EVar of string (* Expression variable *)
    | EBinop of string * expr * expr (* Expression binaire avec opérateur et deux sous-expressions *)
    | EIf of expr * expr * expr (* Expression conditionnelle if *)
    | ELet of string * expr * expr (* Expression let *)
    | EList of expr list (* Expression liste *)
    | EFunc of string list * value_type list * expr (* Expression fonctionnelle *)
    | EApp of expr * expr list (* Expression d'application de fonction *)
    | EPrintln of expr (* Expression println *)
    | EHead of expr (* Expression head pour liste *)
    | ETail of expr (* Expression tail pour liste *)
    | EDefault of expr * expr (* Expression default pour valeurs optionnelles *)
    | EFor of expr * expr * expr (* Expression for *)
    | ERepeat of expr * expr (* Expression repeat *)
    | EWhile of expr * expr (* Expression while *)

(* Fonction pour convertir un type de valeur en chaîne de caractères *)
let rec string_of_value_type = function
  | Tunit -> "unit"
  | Tbool -> "bool"
  | Tint -> "int"
  | Tstring -> "string"
  | TList t -> "list of " ^ string_of_value_type t
  | Tmaybe t -> "maybe " ^ string_of_value_type t
  | Tfunc (args, (ret, eff)) ->
      "(" ^ String.concat " * " (List.map string_of_value_type args) ^ ") -> " ^ string_of_value_type ret

(* Fonction pour combiner deux effets *)
let combine_effect(eff, eff2) =
  match eff with
  | Nothing -> eff2
  | Div -> match eff2 with
    | Nothing -> Div
    | Div -> Div
    | Console -> Div_and_Console
    | Div_and_Console -> Div_and_Console
  | Console -> match eff2 with
    | Nothing -> Console
    | Div -> Div_and_Console
    | Console -> Console
    | Div_and_Console -> Div_and_Console
  | Div_and_Console -> Div_and_Console
  | _ -> failwith "Unexpected effect"

(* Fonctions pour convertir une expression de la syntaxe abstraite en expression de notre langage *)

let rec convert_stmt = function
  | Sblock s -> convert_block s
  | _ -> failwith "Not implemented"

and convert_block = function
  | Sblock s -> convert_stmt s
  | _ -> failwith "Not implemented"

and convert_param = function
  | Pdots (x, t) -> (x, convert_param_type t)

and convert_param_type = function
  | PBase t -> convert_atype t
  | PArrow (t, r) -> Tfunc ([convert_atype t], convert_result r)
  | PArrowpar (params, r) -> Tfunc (List.map convert_param_type params, convert_result r)
  | PAFn f -> Tfunc ([], convert_funbody f)

and convert_atype = function
  | ATypeApp (x, params) -> TList (List.map convert_param_type params)
  | ATypeParen t -> convert_atype t
  | AUnit -> Tunit

and convert_result = function
  | ([], Some t) -> convert_param_type t
  | (idents, Some t) -> Tfunc (List.map (fun _ -> Tunit) idents, convert_param_type t)
  | ([], None) -> Tunit
  | _ -> failwith "Not implemented"

and convert_funbody = function
  | Fbody (params, annot_opt, expr) -> EFunc (List.map (fun (Pdots (x, _)) -> x) params, List.map (fun (Pdots (_, t)) -> convert_param_type t) params, convert_expr expr)

and convert_expr = function
  | Eblock b -> convert_block b
  | Eexpr e -> convert_bexpr e
  | Call (e, params) -> EApp (convert_expr e, List.map convert_param params)
  | Fn e -> EFunc ([], [], convert_expr e)
  | Block stmts -> EList (List.map convert_stmt stmts)
  | ECall (a, params) -> EApp (convert_atom a, List.map convert_expr params)
  | ECallb (e, params) -> EApp (convert_expr e, List.map convert_expr params)
  | AeCall (a, params) -> EApp (convert_atom a, List.map convert_expr params)

and convert_atom = function
  | AIdent x -> EVar x
  | AIntConst n -> EInt n
  | AStringConst s -> EString s
  | ATrue -> EBool true
  | AFalse -> EBool false
  | AUnit -> EUnit
  | AParen (Some e) -> convert_expr e
  | AParen None -> EUnit
  | ACall (a, params) -> EApp (convert_atom a, List.map convert_expr params)
  | ADot (a, x) -> EVar x
  | AFun f -> EFunc ([], [], convert_bexpr (BFun f))
  | ABlock b -> convert_block b

and convert_bexpr = function
  | BAtom a -> convert_atom a
  | BNot e -> EBinop ("!", convert_bexpr e, EUnit)
  | BNeg e -> EBinop ("-", convert_bexpr e, EUnit)
  | BBinop (e1, op, e2) -> EBinop (op, convert_bexpr e1, convert_bexpr e2)
  | BAssign (x, e) -> ELet (x, convert_bexpr e, EVar x)
  | BIf (e1, e2, None) -> EIf (convert_bexpr e1, convert_expr e2, EUnit)
  | BIf (e1, e2, Some e3) -> EIf (convert_bexpr e1, convert_expr e2, convert_expr e3)
  | BIfElse (e1, e2, [], None) -> EIf (convert_bexpr e1, convert_expr e2, EUnit)
  | BIfElse (e1, e2, [], Some e3) -> EIf (convert_bexpr e1, convert_expr e2, convert_expr e3)
  | BIfElse (e1, e2, (e3, e4) :: elifs, else_) ->
      EIf (convert_bexpr e1, convert_expr e2, Some (EIf (convert_bexpr e3, convert_expr e4, convert_bexpr (BIfElse (EAtom (AUnit), EBlock (Sblocks elifs), else_)))))
  | BIfReturn (e1, e2) -> EIf (convert_bexpr e1, convert_expr e2, EUnit)
  | BFun f -> EFunc ([], [], convert_bexpr (BFun f))
  | BReturn e -> ELet ("_", convert_expr e, EVar "_")



(* Fonction principale de typage des expressions *)
let rec type_expr (ctx : (string * value_type) list) (e : expr) : calculation_type =
  match e with
  | EUnit -> (Tunit, Nothing)
  | EBool _ -> (Tbool, Nothing)
  | EInt _ -> (Tint, Nothing)
  | EString _ -> (Tstring, Nothing)
  | EVar x ->
      (try (List.assoc x ctx, Nothing)
       with Not_found -> failwith ("Variable non définie : " ^ x))
  | EBinop (op, e1, e2) ->
      let (t1, eff1) = type_expr ctx e1 in
      let (t2, eff2) = type_expr ctx e2 in
      begin
        match op, t1, t2 with
        | ("+" | "-" | "*" | "/" | "%"), Tint, Tint -> (Tint, combine_effect(eff1, eff2))
        | ("<" | ">" | "<="| ">="), Tint, Tint -> (Tbool, combine_effect(eff1, eff2))
        | ("&&"| "||"), Tbool, Tbool -> (Tbool, combine_effect(eff1, eff2))
        | ("++"), Tstring, Tstring -> (Tstring, combine_effect(eff1, eff2))
        | ("++"), TList t1, TList t2 when t1 = t2 -> (TList t1, combine_effect(eff1, eff2))
        | ("=="| "!="), Tbool, Tbool -> (Tbool, combine_effect(eff1, eff2))
        | ("=="| "!="), Tint, Tint -> (Tint, combine_effect(eff1, eff2))
        | ("=="| "!="), Tstring, Tstring -> (Tstring, combine_effect(eff1, eff2))
        | _ -> failwith "Opération binaire non supportée ou types incompatibles"
      end
  | EIf (e1, e2, e3) ->
      let (t1, eff1) = type_expr ctx e1 in
      let (t2, eff2) = type_expr ctx e2 in
      let (t3, eff3) = type_expr ctx e3 in
      if t1 = Tbool && t2 = t3 then (t2, combine_effect(combine_effect(eff1, eff2), eff3))
      else failwith "Condition mal typée ou branches de types différents"
  | ELet (x, e1, e2) ->
      let (t1, eff1) = type_expr ctx e1 in
      type_expr ((x, t1) :: ctx) e2
  | EList es ->
      begin match es with
      | [] -> (TList Tunit, Nothing)
      | e1 :: _ ->
          let (t1, eff1) = type_expr ctx e1 in
          List.iter (fun e -> if fst (type_expr ctx e) <> t1 then failwith "Liste mal typée") es;
          (TList t1, List.fold_left (fun eff e -> combine_effect(snd (type_expr ctx e), eff)) Nothing es)
      end
  | EFunc (args, arg_types , body) ->
      let (t_body, eff_body) = type_expr (List.combine args arg_types @ ctx) body in
      (Tfunc (arg_types, (t_body, eff_body)), Nothing)
  | EApp (f, args) ->
      let (t_f, eff_f) = type_expr ctx f in
      begin match t_f with
      | Tfunc (arg_types, (t_ret, eff_ret)) ->
          let arg_types2 = List.map (fun arg -> fst (type_expr ctx arg)) args in
          if arg_types = arg_types2 then (t_ret, List.fold_left (fun eff e -> combine_effect(snd (type_expr ctx e), eff)) eff_ret args)
          else failwith "Types des arguments de la fonction incorrects"
      | _ -> failwith "Tentative d'application d'une expression non fonctionnelle"
      end
  | EPrintln e1 -> let (t, eff) = type_expr ctx e1 in
    if List.mem t [Tunit; Tbool; Tint; Tstring] then
      (Tunit, combine_effect(eff, Console))
    else failwith "println : type non supporté"
  | EHead e1 ->
    let (t, eff) = type_expr ctx e1 in
    begin match t with
    | TList t2 -> (Tmaybe t2, eff)
    | _ -> failwith "head : argument doit être une liste"
    end
  | ETail e1 ->
    let (t, eff) = type_expr ctx e1 in
    begin match t with
    | TList t2 -> (TList t2, eff)
    | _ -> failwith "tail : argument doit être une liste"
    end
  | EDefault (e1, e2) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    (match t1 with
      | Tmaybe t3 -> if t1 = t2 then (t1, combine_effect(eff1, eff2))
        else failwith "default : types incompatibles"
      | _ -> failwith "default : types incompatibles")
  | EFor (e1, e2, e3) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    let (t3, eff3) = type_expr ctx e3 in
    if t1 = t2 && t1 = Tint then
      (match t3 with
        | Tfunc ([Tint], (Tunit, eff4)) -> (Tunit, combine_effect(eff4, combine_effect(eff3, combine_effect(eff1, eff2))))
        | _ -> failwith "for : mauvais troisieme argument")
    else failwith "for : premier et deuxieme argument doit être un entier"
  | ERepeat (e1, e2) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    if t1 = Tint then
      (match t2 with
        | Tfunc ([], (Tunit, eff3)) -> (Tunit, combine_effect(eff3, combine_effect(eff1, eff2)))
        | _ -> failwith "repeat : mauvais deuxieme argument")
    else failwith "repeat : premier argument doit être un entier"
  | EWhile (e1, e2) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    (match t1 with
        | Tfunc ([], (Tbool, eff3)) -> (match t2 with
          | Tfunc ([], (Tunit, eff4)) -> (Tunit, Div)
          | _ -> failwith "while : mauvais deuxieme argument")
        | _ -> failwith "while : mauvais premier argument")

(* Exception pour les erreurs de typage *)
exception Error of string

(* Fonction pour lever une exception d'erreur *)
let error s = raise (Error s)