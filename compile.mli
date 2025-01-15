
exception VarUndef of string
  (** exception levée pour signaler une variable non déclarée *)

module Smap : Map.S with type key = string
type local_env = Ast.ident Smap.t

val alloc_expr : local_env -> int -> Ast.pexpr -> Ast.expr * int
val alloc_stmt : Ast.pstmt -> Ast.stmt
val alloc : Ast.pprogram -> Ast.program

val compile_program : Ast.pprogram -> string -> unit
  (** [compile_program p f] compile le programme [p] et écrit le code MIPS
      correspondant dans le fichier [f] *)

