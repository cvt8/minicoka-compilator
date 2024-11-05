


(** Analyse syntaxique élémentaire d'expressions arithmétiques
    formées de constantes, addition, multiplication et parenthèses. *)

{
  open Lexing

  type token =
    | CONST of int
    | PLUS
    | TIMES
    | LEFTPAR
    | RIGHTPAR
    | EOF

}

let white_space = [' ' '\t' '\n']+
let integer     = ['0'-'9']+

rule token = parse
  | white_space
      { token lexbuf }
  | integer as s
      { CONST (int_of_string s) }
  | '+'
      { PLUS }
  | '*'
      { TIMES }
  | '('
      { LEFTPAR }
  | ')'
      { RIGHTPAR }
  | eof
      { EOF }
  | _ as c
      { failwith ("illegal character" ^ String.make 1 c) }

{

  type expr =
    | Const of int
    | Add   of expr * expr
    | Mul   of expr * expr

  (** Avant d'écrire l'analyseur syntaxique, on écrit un pretty-printer *)

  open Format

  (* d'abord simplement, avec des parenthèses partout *)
  let rec print fmt = function
    | Const n      -> fprintf fmt "%d" n
    | Add (e1, e2) -> fprintf fmt "(%a + %a)" print e1 print e2
    | Mul (e1, e2) -> fprintf fmt "(%a * %a)" print e1 print e2

  (* puis plus joliment, en utilisant les boîtes de Format *)
  let rec print fmt = function
    | Const n      -> fprintf fmt "%d" n
    | Add (e1, e2) -> fprintf fmt "(@[%a +@ %a@])" print e1 print e2
    | Mul (e1, e2) -> fprintf fmt "(@[%a *@ %a@])" print e1 print e2

  (* puis plus subtilement, avec des parenthèses seulement lorsque nécessaire *)
  let rec print_expr fmt = function
    | Add (e1, e2) -> fprintf fmt "%a +@ %a" print_expr e1 print_expr e2
    | e            -> print_term fmt e

  and print_term fmt = function
    | Mul (e1, e2) -> fprintf fmt "%a *@ %a" print_term e1 print_term e2
    | e            -> print_factor fmt e

  and print_factor fmt = function
    | Const n -> fprintf fmt "%d" n
    | e       -> fprintf fmt "(@[%a@])" print_expr e

  (* tests *)
  let e = Add (Const 2, Mul (Const 8, Const 5))
  let e = Add (e, Mul (e, e))
  let e = Add (e, Mul (e, e))
  let e = Add (e, Mul (e, e))
  let e = Add (e, Mul (e, e))
  let () = printf "e = @[%a@]@." print e
  let () = printf "e = @[%a@]@." print_expr e

  (** Analyse syntaxique (de l'entrée standard) *)

  (* un peu de machinerie pour la lecture des lexèmes *)

  let lb = from_channel stdin
  let t = ref EOF                  (* le prochain lexème à examiner *)
  let next () = t := token lb
  let () = next ()                 (* lire le tout premier lexème *)

  (* le pretty-printer nous a mis sur la bonne voie en distinguant
     les sommes, les produits et les facteurs

     on écrit donc trois fonctions d'analyse syntaxique,
     parse_expr, parse_term et parse_factor *)

  let error () = failwith "syntax error"

  let rec parse_expr () =
    let e = parse_term () in
    if !t = PLUS then begin next (); Add (e, parse_expr ()) end else e
  and parse_term () =
    let e = parse_factor () in
    if !t = TIMES then begin next (); Mul (e, parse_term ()) end else e
  and parse_factor () = match !t with
    | CONST n -> next (); Const n
    | LEFTPAR -> next ();
        let e = parse_expr () in if !t <> RIGHTPAR then error (); next (); e
    | _ -> error ()

  let e = parse_expr ()
  let () = if !t <> EOF then error ()

  let () = printf "e = @[%a@]@." print e
  let () = printf "e = @[%a@]@." print_expr e

}

(*
Local Variables:
compile-command: "ocamllex arith_expr.mll && ocamlopt arith_expr.ml"
End:
*)
 
