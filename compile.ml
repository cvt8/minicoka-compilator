

open Format
open X86_64
open Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
  | PCst i ->
    Cst i, fpcur

  | PVar x ->
    begin
      try
        let ofs_x = Smap.find x env in
        LVar ofs_x, fpcur
      with Not_found ->
        if not (Hashtbl.mem genv x) then raise (VarUndef x);
        GVar x, fpcur
    end

  | PBinop (o, e1, e2)->
    let e1, fpmax1 = alloc_expr env fpcur e1 in
    let e2, fpmax2 = alloc_expr env fpcur e2 in
    Binop(o, e1, e2), (max fpmax1 fpmax2)

  | PLetin (x,e1,e2) ->
    let e1, fpmax1 = alloc_expr env fpcur e1 in
    let fpcur = fpcur + 8 in
    let e2, fpmax2 = alloc_expr (Smap.add x (-fpcur) env) fpcur e2 in
    Letin (-fpcur, e1, e2), max fpmax1 fpmax2

  | PCall (f, l) ->
    let l, fpmax =
      List.fold_left
        (fun (l, fpmax) e ->
          let e, fpmax' = alloc_expr env fpcur e in
          e::l, max fpmax fpmax') ([], fpcur) l
    in
    Call (f, l), fpmax

let alloc_stmt = function
  | PSet (x, e) ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Hashtbl.replace genv x ();
    Set (x, e, fpmax)

  | PFun (f, l, e) ->
    (* Format.eprintf "fun %s@." f; *)
    let env, fpcur =
      List.fold_left
        (fun (env, fpcur) x ->
          let fpcur = fpcur + 8 in
          (* Format.eprintf "  %s = %d@." x fpcur; *)
          Smap.add x fpcur env, fpcur)
        (Smap.empty, 8) l
    in
    let e, fpmax = alloc_expr env 0 e in
   Fun (f, e, fpmax)

  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)

let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function
  | Cst i ->
      pushq (imm i)

  | LVar fp_x ->
      pushq (ind ~ofs:fp_x rbp)

  | GVar x ->
      pushq (lab x)

  | Binop (o, e1, e2)->
      compile_expr e1 ++
      compile_expr e2 ++
      popq rbx ++ popq rax ++
      (match o with
        | Add -> addq !%rbx !%rax
        | Sub -> subq !%rbx !%rax
        | Mul -> imulq !%rbx !%rax
        | Div -> cqto ++ idivq !%rbx) ++
       pushq !%rax

  | Letin (ofs, e1, e2) ->
      compile_expr e1 ++
      popq rax ++ movq !%rax (ind ~ofs rbp) ++
      compile_expr e2

  | Call (f, l) ->
      List.fold_left (fun code e -> code ++ compile_expr e) nop l ++
      call f ++ popn (8 * List.length l) ++ pushq !%rax

let compile_stmt (codefun, codemain) = function
  | Set (x, e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rax ++ movq !%rax (lab x) ++
      popn fpmax
    in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code =
      label f ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++ pushn fpmax ++
      compile_expr e ++ popq rax ++
      popn fpmax ++ popq rbp ++ ret
    in
    code ++ codefun, codemain

  | Print (e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rdi ++
      popn fpmax ++
      call "print_int"
    in
    codefun, codemain ++ code


let compile_program p ofile =
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
  fprintf fmt "@?";
  close_out f

This document was generated using caml2html 