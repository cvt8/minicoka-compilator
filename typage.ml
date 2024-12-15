open Ast

type effect =
  | Nothing
  | Div
  | Console
  | Div_and_Console

type value_type =
  | Tunit
  | Tbool
  | Tint
  | Tstring
  | TList of value_type
  | Tmaybe of value_type
  | Tfunc of value_type list * calculation_type

type calculation_type= value_type * effect

type expr =
  | EUnit
  | EBool of bool
  | EInt of int
  | EString of string
  | EVar of string
  | EBinop of string * expr * expr 
  | EIf of expr * expr * expr 
  | ELet of string * expr * expr
  | EList of expr list
  | EFunc of string list * value_type list * expr
  | EApp of expr * expr list
  | EPrintln expr
  | EHead of expr
  | ETail of expr
  | EDefault of expr * expr
  | EFor of expr * expr * expr
  | ERepeat of expr * expr
  | EWhile of expr * expr

let combine_effect(eff,eff2)=
  match eff with:
  | Nothing -> eff2
  | Div -> match eff2 with:
    | Nothing -> Div
    | Div -> Div
    | Console -> Div_and_Console
    | Div_and_Console -> Div_and_Console
  | Console -> match eff2 with:
    | Nothing -> Console
    | Div -> Div_and_Console
    | Console -> Console
    | Div_and_Console -> Div_and_Console
  | Div_and_Console -> Div_and_Console

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
        | ("+", "-", "*", "/", "%"), Tint, Tint -> (Tint, combine_effect(eff1,eff2))
        | ("<", ">", "<=", ">="), Tint, Tint -> (Tbool, combine_effect(eff1,eff2))
        | ("&&", "||"), Tbool, Tbool -> (Tbool, combine_effect(eff1,eff2))
        | ("++"), Tstring, Tstring -> (Tstring, combine_effect(eff1,eff2))
        | ("++"), TList of v_type, TList of v_type -> (TList of v_type, combine_effect(eff1,eff2))
        | ("==", "!="), Tbool, Tbool -> (Tbool, combine_effect(eff1,eff2))
        | ("==", "!="), Tint, Tint -> (Tint, combine_effect(eff1,eff2))
        | ("==", "!="), Tstring, Tstring -> (Tstring, combine_effect(eff1,eff2))
        | _ -> failwith "Opération binaire non supportée ou types incompatibles"
      end
  | EIf (e1, e2, e3) ->
      let (t1, eff1) = type_expr ctx e1 in
      let (t2, eff2) = type_expr ctx e2 in
      let (t3, eff3) = type_expr ctx e3 in
      if t1 = Tbool && t2 = t3 then (t2, combine_effect(combine_effect(eff1,eff2),eff3))
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
          (TList t1, List.iter (fun e eff -> combine_effect(snd (type_expr ctx e),eff)) es Nothing )
      end
  | EFunc (args, arg_types , body) ->
      let (t_body, eff_body) = type_expr (List.combine args arg_types @ ctx) body in
      (Tfunc (arg_types, (t_body, eff_body)),Nothing)
  | EApp (f, args) ->
      let (t_f, eff_f) = type_expr ctx f in
      begin match t_f with
      | Tfunc (arg_types, (t_ret,eff_ret)) ->
          let arg_types2 = List.map (fun arg -> fst (type_expr ctx arg)) args in
          if arg_types = arg_types2 then (t_ret, combine_effect(eff_ret,(List.iter (fun e eff -> combine_effect(snd (type_expr ctx e),eff)) args eff_f)))
          else failwith "Types des arguments de la fonction incorrects"
      | _ -> failwith "Tentative d'application d'une expression non fonctionnelle"
      end
  | EPrintln e1 -> let (t, eff) = type_expr ctx e1 in
    if List.mem t [Tunit; Tbool; Tint; Tstring] then
      (Tunit, combine_effect(eff,Console))
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
      | Tmaybe t3 -> if t1 = t2 then (t1, combine_effect(eff1,eff2))
        else failwith "default : types incompatibles"
      | _ -> failwith "default : types incompatibles")
  | EFor (e1, e2, e3) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    let (t3, eff3) = type_expr ctx e3 in
    if t1=t2 && t1=Tint then
      (match t3 with
        | Tfunc ([Tint],(Tunit,eff4)) -> (Tunit, combine_effect(eff4,combine_effect(eff3,combine_effect(eff1,eff2))))
        | _ -> failwith "for : mauvais troiseme argument")
    else failwith "for : premier et deuxieme argument doit être un entier"
  | ERepeat (e1, e2) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    if t1 = Tint then
      (match t2 with
        | Tfunc ([],(Tunit,eff3)) -> (Tunit, combine_effect(eff3,combine_effect(eff1,eff2)))
        | _ -> failwith "repeat : mauvais deuxieme argument")
    else failwith "repeat : premier argument doit être un entier"
  | EWhile (e1, e2, e3, e4) ->
    let (t1, eff1) = type_expr ctx e1 in
    let (t2, eff2) = type_expr ctx e2 in
    (match t1 with
        | Tfunc ([],(Tbool,eff3)) -> (match t2 with
          | Tfunc ([],(Tunit,eff4)) -> (Tunit, Div)
          | _ -> failwith "while : mauvais deuxieme argument")
        | _ -> failwith "while : mauvais premier argument")
  