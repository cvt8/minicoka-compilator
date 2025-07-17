open Format
open X86_64
open Ast

exception VarUndef of string

let compile_program (_ : Ast.file) (ofile : string) =
  let prog =
    { text =
        globl "main" ++ label "main" ++
        movq (imm 0) !%rax ++
        ret;
      data = nop }
  in
  let oc = open_out ofile in
  let fmt = formatter_of_out_channel oc in
  X86_64.print_program fmt prog;
  pp_print_flush fmt (); (* flush output *)
  close_out oc
