(* compile.ml *)

open Format
open X86_64
open Ast
open Typing

(* Phase 1: allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec compile_expr = function
  | EUnit -> nop
  | EBool b -> movq (imm (if b then 1 else 0)) !%rax
  | EInt n -> movq (imm n) !%rax
  | EVar x ->
    begin
      try movq (lab x) !%rax
      with Not_found -> raise (VarUndef x)
    end
  | EBinop (op, e1, e2) -> compile_binop op e1 e2
  | EIf (e1, e2, e3) -> compile_if e1 e2 e3

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

and compile_if e1 e2 e3 =
  let code1 = compile_expr e1 in
  let code2 = compile_expr e2 in
  let code3 = compile_expr e3 in
  code1 ++ testq !%rax !%rax ++ jz "else" ++ code2 ++ jmp "end" ++ label "else" ++ code3 ++ label "end"

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
      let typed_expr = Typing.type_expr [] (Typing.convert_expr expr) in
      allocate_closure ++ initialize_closure ++ compile_expr typed_expr ++ call_closure

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
  fprintf fmt "@?"