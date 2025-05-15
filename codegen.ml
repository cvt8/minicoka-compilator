open Ast
open X86_64

let compile_expr env e =
  match e with
  | Atom (Int n) ->
      movq (imm n) !%rax
  | Atom (Bool b) ->
      movq (imm (if b then 1 else 0)) !%rax
  | Atom (String s) ->
      let lbl = "string_" ^ string_of_int (Hashtbl.hash s) in
      let data = string s in
      let text = movq (lab lbl) !%rax in
      (text, data)
  | Atom (Unit) ->
      movq (imm 0) !%rax
  | BinOp (e1, Add, e2) ->
      let t1, d1 = compile_expr env e1 in
      let t2, d2 = compile_expr env e2 in
      t1 ++ pushq !%rax ++ t2 ++ popq !%rbx ++ addq !%rbx !%rax, d1 ++ d2
  (* Add cases for other expressions, functions, closures, etc. *)
  | _ -> failwith "Code generation not implemented"

let compile_file (File decls) =
  let text = ref nop in
  let data = ref nop in
  List.iter (fun (FunDecl (id, fb)) ->
    let t, d = compile_funbody id fb in
    text := !text ++ t;
    data := !data ++ d
  ) decls;
  { text = globl "main" ++ !text; data = !data }

let compile_to_file ast filename =
  let outfile = Filename.chop_suffix filename ".koka" ^ ".s" in
  let prog = compile_file ast in
  print_in_file ~file:outfile prog1