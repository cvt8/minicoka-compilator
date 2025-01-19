(* compile.ml *)

open Format
open X86_64
open Ast
open Typing

(* Phase 1: allocation des variables *)

(* Exception levée lorsqu'une variable n'est pas définie *)
exception VarUndef of string

(* Table de hachage globale pour stocker les variables *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Module pour les maps de chaînes de caractères *)
module Smap = Map.Make(String)

(* Type pour l'environnement local, qui mappe les identifiants *)
type local_env = ident Smap.t

(* Fonction pour allouer les variables *)
let rep_movsb = inline "rep movsb"

let rec convert_expr = function
  |Ast.Eblock b -> Typing.EBlock b
  |Ast.Eexpr e -> Typing.EExpr e

let rec convert_bexpr = function
  |Ast.Eexpr e -> Typing.EExpr e

(* Fonction pour allouer les variables d'une expression *)
let rec alloc_expr = function
  | EUnit -> EUnit
  | EBool b -> EBool b
  | EInt n -> EInt n
  | EVar x -> EVar x
  | EBinop (op, e1, e2) -> EBinop (op, alloc_expr e1, alloc_expr e2)
  | EIf (e1, e2, e3) -> EIf (alloc_expr e1, alloc_expr e2, alloc_expr e3)
  | EElse (e1, e2) -> EElse (alloc_expr e1, alloc_expr e2)
  | EFn (Fbody (params, annot_opt, expr)) -> EFn (Fbody (params, annot_opt, expr))
  | EFun (Fbody (params, annot_opt, expr)) -> EFun (Fbody (params, annot_opt, expr))
  | EReturn e -> EReturn (alloc_expr e)
  | EThen (e1, e2) -> EThen (alloc_expr e1, alloc_expr e2)
  | EVal e -> EVal (alloc_expr e)
  | EElif (e1, e2, e3) -> EElif (alloc_expr e1, alloc_expr e2, alloc_expr e3)
  | EString s -> EString s
  | ELet (x, e1, e2) -> ELet (x, alloc_expr e1, alloc_expr e2)
  | EApp (e, es) -> EApp (alloc_expr e, List.map alloc_expr es)
  | EPrintln e -> EPrintln (alloc_expr e)
  | EFor (e1, e2, e3) -> EFor (alloc_expr e1, alloc_expr e2, alloc_expr e3)
  | ERepeat (e1, e2) -> ERepeat (alloc_expr e1, alloc_expr e2)
  | EWhile (e1, e2) -> EWhile (alloc_expr e1, alloc_expr e2)

(* Fonction pour allouer les variables d'une déclaration *)
let rec alloc_decl = function
  | Ast.Eexpr e -> alloc_expr (convert_bexpr e)
  | _ -> failwith "Unsupported declaration type"

(* Fonction pour allouer les variables d'un programme *)
let alloc (p: Ast.file) = List.map alloc_decl p

(******************************************************************************)
(* phase 2 : production de code *)

(* Fonction récursive pour compiler une expression *)
let rec compile_expr = function
  | EUnit -> nop  (* Expression unité *)
  | EBool b -> movq (imm (if b then 1 else 0)) !%rax  (* Expression booléenne *)
  | EInt n -> movq (imm n) !%rax  (* Expression entière *)
  | EVar x ->  (* Expression variable *)
    begin
      try movq (lab x) !%rax
      with Not_found -> raise (VarUndef x)
    end
  | EBinop (op, e1, e2) -> compile_binop op e1 e2  (* Expression binaire *)
  | EIf (e1, e2, e3) -> compile_if e1 e2 e3  (* Expression conditionnelle *)
  | EElse (e1, e2) -> compile_expr e1 ++ compile_expr e2  (* Expression séquentielle *)
  | EFn (Fbody (_, _, _)) -> failwith "Unsupported function type"  (* Expression fonction *)
  | EFun (Fbody (_, _, _)) -> failwith "Unsupported function type"  (* Expression fonction *)
  | EReturn e -> compile_expr e  (* Expression return *)
  | EThen (e1, e2) -> compile_expr e1 ++ compile_expr e2  (* Expression séquentielle *)
  | EVal e -> compile_expr e  (* Expression valeur *)
  | EElif (e1, e2, e3) -> compile_expr e1 ++ compile_expr e2 ++ compile_expr e3  (* Expression elif *)
  | EString s ->  (* Expression chaîne de caractères *)
    let l = ".S" ^ string_of_int (Hashtbl.length genv) in
    Hashtbl.add genv l ();
    movq (ilab l) !%rax ++
    movq (imm (String.length s)) !%rdi ++
    call "malloc" ++
    movq !%rax (lab l) ++
    movq (imm 0) !%rdi ++
    movq (imm (String.length s)) !%rcx ++
    movq (ilab s) !%rsi ++
    rep_movsb ++
    movq (lab l) !%rax
  | ELet (x, e1, e2) ->  (* Expression let *)
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rdi ++
    movq !%rax (lab x) ++
    movq !%rdi !%rax
  | EApp (e, es) ->  (* Expression application *)
    let code = List.fold_left (fun acc e -> acc ++ compile_expr e ++ pushq !%rax) nop es in
    code ++ compile_expr e ++ call "*%rax" ++ inline "addq $8, %rsp"
  | EPrintln e ->  (* Expression println *)
    compile_expr e ++
    movq !%rax !%rdi ++
    call "print_int" ++
    movq (imm 0) !%rax
  | EFor (e1, e2, e3) ->  (* Expression for *)
    compile_expr e1 ++
    label "for" ++
    compile_expr e2 ++
    testq !%rax !%rax ++
    jz "endfor" ++
    compile_expr e3 ++
    jmp "for" ++
    label "endfor"
  | ERepeat (e1, e2) ->  (* Expression repeat *)
    label "repeat" ++
    compile_expr e1 ++
    testq !%rax !%rax ++
    jnz "repeat" ++
    compile_expr e2
  | EWhile (e1, e2) ->  (* Expression while *)
    label "while" ++
    compile_expr e1 ++
    testq !%rax !%rax ++
    jz "endwhile" ++
    compile_expr e2 ++
    jmp "while" ++
    label "endwhile"
  

(* Fonction pour compiler une opération binaire *)
and compile_binop op e1 e2 =
  let code1 = compile_expr e1 in
  let code2 = compile_expr e2 in
  match op with
  | "+" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ addq !%rbx !%rax
  | "-" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ subq !%rbx !%rax
  | "*" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ imulq !%rbx !%rax
  | "/" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cqto ++ idivq !%rbx
  | "%" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cqto ++ idivq !%rbx ++ movq !%rdx !%rax
  | "&&" -> code1 ++ testq !%rax !%rax ++ jz "false" ++ code2 ++ testq !%rax !%rax ++ jz "false" ++ movq (imm 1) !%rax ++ jmp "end" ++ label "false" ++ movq (imm 0) !%rax ++ label "end"
  | "||" -> code1 ++ testq !%rax !%rax ++ jnz "true" ++ code2 ++ testq !%rax !%rax ++ jnz "true" ++ movq (imm 0) !%rax ++ jmp "end" ++ label "true" ++ movq (imm 1) !%rax ++ label "end"
  | "==" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ sete !%al ++ movzbq !%al rax
  | "!=" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ setne !%al ++ movzbq !%al rax
  | "<" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ setl !%al ++ movzbq !%al rax
  | "<=" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ setle !%al ++ movzbq !%al rax
  | ">" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ setg !%al ++ movzbq !%al rax
  | ">=" -> code1 ++ pushq !%rax ++ code2 ++ popq rbx ++ cmpq !%rbx !%rax ++ setge !%al ++ movzbq !%al rax
  | _ -> failwith "Opérateur binaire non supporté"


(* Fonction pour compiler une expression conditionnelle *)
and compile_if e1 e2 e3 =
  let code1 = compile_expr e1 in
  let code2 = compile_expr e2 in
  let code3 = compile_expr e3 in
  code1 ++ testq !%rax !%rax ++ jz "else" ++ code2 ++ jmp "end" ++ label "else" ++ code3 ++ label "end"

(* Fonction pour compiler le corps d'une fonction *)
and compile_funbody = function
  | Fbody (params, annot_opt, expr) ->
      let allocate_closure =
        List.fold_left (fun acc param -> acc ++ pushq (!%rbp)) nop params ++
        movq (imm (List.length params * 8)) (!%rax) ++
        inline "subq %rax, %rsp" ++
        List.fold_left (fun acc param -> acc ++ popq rbp ++ movq (!%rbp) (ind ~ofs:0 (rsp))) nop params
      in
      let initialize_closure =
        List.fold_left (fun acc param -> acc ++ movq (!%rbp) (ind ~ofs:(List.length params * 8) (rsp))) nop params
      in
      let call_closure =
        movq (ind ~ofs:0 (rsp)) (!%rax) ++
        inline "call *%rax" ++
        inline "addq $8, %rsp"
      in
      let convert_param_to_pair = function
        | Pdots (x, t) -> (x, convert_param_type t)
        | PFn (Fbody (_, _, _)) -> failwith "Unsupported parameter type"
      in
      let typed_params = List.map convert_param_to_pair params in
      let typed_expr = Typing.type_expr typed_params (convert_expr expr) in
      allocate_closure ++ initialize_closure ++ compile_expr (convert_expr expr) ++ call_closure


(* Fonction pour compiler un programme complet *)
let compile_program (p: Ast.file) (ofile: string) =
  let p = alloc p in
  Format.eprintf "%a@." print p;
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?"