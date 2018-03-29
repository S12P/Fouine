(*
Type des identifieur.
Pas forcement indispensable, mais de cet manière, on ne depend pas de type ocaml dans notre AST.

Denis
*)
type id = string

let concat_id i = (^) i;

(*
Liste des differents opérateurs possibles.
*)
type operator =
    | Add (* +  *)
    | Sub (* -  *)
    | Mul (* x  *)
    | Lt  (* <  *)
    | Le  (* <= *)
    | Gt  (* >  *)
    | Ge  (* >= *)
    | Eq  (* =  *)
    | Neq (* <> *)

let string_of_operator = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Lt  -> "<"
    | Le  -> "<="
    | Gt  -> ">"
    | Ge  -> ">="
    | Eq  -> "="
    | Neq -> "<>"

(* On definie le type instruction utile pour la machine *)
type instr =
  | C of int (* Pour les entiers *)
  | Op of operator (* Pour les operator + - * *)
  | PrInt (* Pour la fonction PrInt *)

(* On definie le type code qui est une liste d'instruction qui sera utile pour la machine *)
type code = instr list

(*
Nom de l'exception dans l'environnement.
*)

let exception_name = "_Exception"


(*
Valeurs et leur type.
Trois types de valeurs possible, seuls les Ints et les Functions seront utilisé dans un premier temps.
*)
type value =
    | None
    | Int of int
    | Bool of bool
    | Function of ((environment * value) -> value)

(*
Liste des differentes expressions du langage.
Généré par le parser comme l'AST du programme.
N'est pas typé pour le moment.
*)
and expr =
    | Ide   of id                      (* Identificateur          (ex. x) *)
    | Val   of value                   (* Valeur                  (ex. 5) *)
    | Ope   of expr  * operator * expr (* Operateur infix         (ex. 3+5) *)
    | Fun   of id    * expr            (* Fonction                (ex. fun x -> 1) *)
    | App   of expr  * expr            (* Application             (ex. (fun x -> 1) 5) *)
    | Let   of id    * expr            (* let                     (ex. let x = 5) *)
    | LetR  of id    * expr            (* let rec                 (ex. let rec x = fun y -> x y) *)
    | LetI  of id    * expr     * expr (* let in                  (ex. let x = 5 in x) *)
    | LetIR of id    * expr     * expr (* let rec in              (ex. let rec x = fun y -> x y in x) *)
    | IfE   of expr  * expr     * expr (* if then else            (ex. if 4 < 0 then 1 else 0) *)
    | Rai   of expr                    (* soulevement d'exception (ex. raise 42) *)
    | TrW   of expr  * id       * expr (* attrapage d'exception   (ex. try f with x -> g x) *)

(*
Type de l'environment.
Idéalement il faudrait faire un module pour généraliser les fonction : 'add' / 'replace' / 'remove' / 'find' / ...
*)
and environment = Env of (id, expr) Hashtbl.t

let environment_add = function
    | Env e -> Hashtbl.add e;;

let environment_replace = function
    | Env e -> Hashtbl.replace e;;

let environment_remove = function
    | Env e -> Hashtbl.remove e;;

let environment_find = function
    | Env e -> Hashtbl.find e;;

let environment_mem = function
    | Env e -> Hashtbl.mem e;;

let environment_copy = function
    | Env e -> Env (Hashtbl.copy e);;

let environment_remove_all = function
    | Env e -> fun x -> (let l = Hashtbl.find_all e x in
                        for i = 1 to List.length l do Hashtbl.remove e x done);;

let environment_clear = function
    | Env e -> Hashtbl.clear e;;

(*
Fonctions d'affichage de l'AST
*)
let affiche_id = print_string;;


let affiche_operator = function
    | Add -> print_string "+";
    | Sub -> print_string "-";
    | Mul -> print_string "*";
    | Lt  -> print_string "<";
    | Le  -> print_string "<=";
    | Gt  -> print_string ">";
    | Ge  -> print_string ">=";
    | Eq  -> print_string "=";
    | Neq -> print_string "<>";;

let affiche_value = function
    | None       -> print_string "??";
    | Int      v -> print_int  v;
    | Bool     v -> print_string (string_of_bool v);
    | Function _ -> print_string "Fun ??? -> ???";;

let rec affiche_expression = function
    | Ide  i            -> affiche_id i;
    | Val  v            -> affiche_value v;
    | Ope  (e1, o, e2)  -> print_char '('; affiche_expression e1; print_char ' ';
                           affiche_operator o;
                           print_char ' '; affiche_expression e2; print_char ')';
    | Fun  (i, e)       -> print_string "(fun "; affiche_id i; print_string " -> ";
                           affiche_expression e; print_char ')';
    | App  (e1, e2)     -> affiche_expression e1;  print_char ' ';
                           affiche_expression e2;
    | LetI  (i, e1, e2) -> print_string "let "; affiche_id i; print_string " = ";
                           affiche_expression e1; print_string " in ";
                           affiche_expression e2;
    | LetIR (i, e1, e2) -> print_string "let rec "; affiche_id i; print_string " = ";
                           affiche_expression e1; print_string " in ";
                           affiche_expression e2;
    | Let  (i, e1)      -> print_string "let "; affiche_id i; print_string " = ";
                           affiche_expression e1;
    | LetR (i, e1)      -> print_string "let rec "; affiche_id i; print_string " = ";
                           affiche_expression e1;
    | IfE  (e1, e2, e3) -> print_string "if "; affiche_expression e1; print_string " then ";
                           affiche_expression e2; print_string " else ";
                           affiche_expression e3;
    | Rai  (e)          -> print_string "raise (E "; affiche_expression e; print_string ")";
    | TrW  (e1, i, e2)  -> print_string "try "; affiche_expression e1;
                           print_string " with E "; affiche_id i; print_string " -> ";
                           affiche_expression e2;;

(*
Fonctions d'affichage de code
*)
let rec affichage_code_fichier f = function
    | C   i -> Printf.fprintf f "[%d];" i;
    | Op  o -> Printf.fprintf f "%s;" (string_of_operator o);
    | PrInt -> Printf.fprintf f "prInt;";;
