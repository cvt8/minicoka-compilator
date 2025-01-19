(* Fichier principal de l'interprète mini-Koka *)

open Format
open Lexing

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Nom du fichier source *)
let ifile = ref ""
let ofile = ref ""

(* Fonction pour modifier le nom du fichier source *)
let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
  [
  "--parse-only", Arg.Set parse_only,
  "  Pour ne faire uniquement que la phase d'analyse syntaxique";
  "--type-only", Arg.Set type_only,
  "  Pour ne faire uniquement que la phase de typage";
  "-o", Arg.String (set_file ofile),
  "<file>  Pour indiquer le nom du fichier de sortie"
]

let usage = "usage: koka [option] file.koka"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  (* Ce fichier doit avoir l'extension .koka *)
  if not (Filename.check_suffix !ifile ".koka") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .koka\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Par défaut, le fichier cible a le même nom que le fichier source,
     seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".koka" ^ ".s";

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: la fonction Parser.file transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.file pour obtenir
       le prochain token. *)

    let p = Parser.file Lexer.token buf in
    close_in f;
    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;

    (* Environnement initial vide pour le typeur *)
    let initial_env = [] in
    let (_, eff) = Typing.type_expr initial_env p in
    close_in f;
    (* On s'arrête ici si on ne veut faire que le typage *)
    if !type_only then exit 0;

    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine
       résultant de cette transformation doit être écrit dans le fichier
       cible ofile. *)
    Compile.compile_program p !ofile
    (*Interp.prog p *)
  with
  | Lexer.Lexing_error c ->
    (* Erreur lexicale. On récupère sa position absolue et
       on la convertit en numéro de ligne *)
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur lexicale: %s@." c;
    exit 1
  | Parser.Error ->
    (* Erreur syntaxique. On récupère sa position absolue et on la
       convertit en numéro de ligne *)
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur syntaxique@.";
    exit 1
  | Typing.Error s ->
    (* Erreur pendant le typage *)
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur de typage : %s@." s;
    exit 1
  | Interp.Error s ->
    (* Erreur pendant l'interprétation *)
    eprintf "Erreur : %s@." s;
    exit 1
  | Compile.VarUndef s ->
    (* Erreur d'utilisation de variable pendant la compilation *)
    eprintf "Erreur de compilation: la variable %s n'est pas définie@." s;
    exit 1
  | e ->
    (* Erreur du compilateur lui-même *)
    eprintf "Erreur interne du compilateur : %s@." (Printexc.to_string e);
    exit 2
