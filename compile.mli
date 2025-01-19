
exception VarUndef of string
  (** exception levée pour signaler une variable non déclarée *)

module Smap : Map.S with type key = string
type local_env = Ast.ident Smap.t

val alloc_expr : local_env -> int -> Ast.bexpr -> Ast.expr * int
val alloc_stmt : Ast.bexpr -> Ast.stmt
val alloc : Ast.file -> Ast.file


(* Type pour les environnements de typage *)
type env = (string * Typing.value_type) list

(* Fonction de typage *)
val type_check : env -> Ast.file -> unit

(* Fonction de génération de code x86-64 *)
val compile_to_x86 : Ast.file -> X86_64.program


val compile_program : Ast.file -> string -> unit
  (** [compile_program p f] compile le programme [p] et écrit le code MIPS
      correspondant dans le fichier [f] *)

