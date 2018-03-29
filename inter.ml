open Expr;;

(*
Function d'extraction du type 'value'.
Si le paramètre est un identifier, alors il est inconnue et le programme est faux.
*)
let int_of_value id = function
    | None  -> failwith (concat_id id ": is unknown")
    | Int i -> i
    | _     -> failwith "Typage error"

let int_of_value' = function
    | Int i -> i
    | _     -> failwith "Typage error"

let bool_of_value id = function
    | None   -> failwith (concat_id id ": is unknown")
    | Bool b -> b
    | _      -> failwith "Typage error"

let function_of_value id = function
    | None       -> failwith (concat_id id ": is unknown")
    | Function f -> f
    | _          -> failwith "Typage error"

let function_of_value' = function
    | Function f -> f
    | _          -> failwith "Typage error"

let bool_of_int = (=) 1

(*
Interpreteur d'expressions.
On suppose les types correctes, on passe après le 'typechecker' (Mais le sujet ne demande pas de 'typechecker').
On utilise la Hashtbl pour acceder à un élément de l'environnement rapidement, et empiler les doublons.
*)
(* environment -> expr -> value *)
let rec interpreteur (env_exc: environment) (env: environment) = function
    | Ide  i              -> if environment_mem env i then interpreteur env_exc env (environment_find env i) (* Si l'identifieur existe dans l'environnement alors on l'interprete *)
                             else failwith (concat_id i ": is unknown");                                     (* Sinon on retourne une erreur *)
    | Val  v              -> v;                                                                              (* On retourne la valeur *)
    | Ope  (e1, o, e2)    -> (function_of_value'                                                             (* Ici on appel la second function de la currification de l'opérateur *)
                              ((function_of_value (string_of_operator o)                                     (* Ici on appel la première function de la currification de l'opérateur *)
                               (interpreteur env_exc env ((environment_find env) (string_of_operator o))))   (* Ici on récupère la function de l'opérateur *)
                              (env, interpreteur env_exc env e1)))
                             (env, interpreteur env_exc env e2)
    | Fun  (i, e)         -> let env' = environment_copy env in environment_remove_all env' i;               (* On supprime le paramètre de la fonction de l'environnement *)
                              Function (fun (_, x) -> (environment_add env' i (Val x);                       (* On ajoute dans l'environnement le paramètre de la fonction *)
                               interpreteur env_exc env' e));                                                (* On interprete la fonction *)
    | App  (e1, e2)       -> if not (environment_mem env exception_name) then                                (* On regarde si une exception  à été levée *)
                              let e2' = (interpreteur env_exc env e2) in                                     (* On interprète d'abord le paramètre *)
                               if not (environment_mem env exception_name) then                              (* On regarde si une exception  à été levée *)
                                (function_of_value' (interpreteur env_exc env e1)) (env, e2')                (* On récupère la fonction et on applique *)
                               else                                                                          (* Sinon on ne continue pas*)
                                None                                                                         (* On retourne None *)
                             else                                                                            (* Sinon on ne continue pas *)
                              None                                                                           (* On retourne None *)
    | LetI  (i, e1, e2)   -> let e1' = interpreteur env_exc env e1 in                                        (* On interprète l'expression *)
                              environment_add env i (Val e1');                                               (* On ajoute le resultat de l'expression à l'environnement *)
                              let e2' = interpreteur env_exc env e2 in                                       (* On interprète l'expression *)
                               environment_remove env i;                                                     (* On retire le resultat de l'expressions de l'environnement *)
                              e2';                                                                           (* On retourne le resultat *)
    | LetIR (i, e1, e2)   -> environment_add env i e1;                                                       (* On ajoute la fonction à l'environnement *)
                             let e1' = interpreteur env_exc env e1 in                                        (* On interprète l'expression *)
                              environment_remove env i;                                                      (* On enlève la fonction de l'environnement *)
                              environment_add env i (Val e1');                                               (* On ajoute le resultat de l'expression à l'environnement *)
                              let e2' = interpreteur env_exc env e2 in                                       (* On interprète l'expression *)
                               environment_remove env i;                                                     (* On retire le resultat de l'expressions de l'environnement *)
                              e2';                                                                           (* On retourne le resultat *)

    | Let   (i, e)        -> let e' = interpreteur env_exc env e in                                          (* On interprète l'expression *)
                              environment_add env i (Val e');                                                (* On ajoute le resultat à l'environnement *)
                              None;                                                                          (* On retourne None *)
    | LetR  (i, e)         -> environment_add env i e;                                                       (* On ajoute la fonction à l'environnement *)
                              let e' = interpreteur env_exc env e in                                         (* On interprète l'expression *)
                              environment_remove env i;                                                      (* On enlève la fonction de l'environnement *)
                              environment_add env i (Val e');                                                (* On ajoute le resultat à l'environnement *)
                              None;                                                                          (* On retourne None *)

    | IfE   (e1, e2, e3)   -> let e1' = interpreteur env_exc env e1 in                                       (* On interprète la condition *)
                               if bool_of_int (int_of_value' e1') then                                       (* On test l'expression booléenne*)
                                interpreteur env_exc env e2                                                  (* Si vrai on retourne la première expression *)
                               else
                                interpreteur env_exc env e3;                                                 (* Sinon on retourne la second expression *)
    | Rai   (e)            -> let e' = interpreteur env_exc env e in                                         (* On interprète l'expression *)
                               environment_add env_exc exception_name (Val e');                              (* On ajoute l'exception dans l'environnement d'exceptions *)
                               None;                                                                         (* On retourne None *)
    | TrW   (e1, i, e2)    -> let e1' = interpreteur env_exc env e1 in                                       (* On interprète l'expression *)
                              if environment_mem env_exc exception_name then                                 (* S'il y a une exception sur la pile *)
                                let e = environment_find env_exc exception_name in                           (* On récupère la valeur de l'exception *)
                                 environment_remove env_exc exception_name;                                  (* On supprime la dernière exception *) 
                                 environment_add env i e;                                                    (* On rajoute la valeur de l'exception dans l'environnement *)
                                 let e2' = interpreteur env_exc env e2 in                                    (* On interprète l'expression *)
                                  environment_remove env i;                                                  (* On enlève la valeur de l'exception de l'environnement *)
                                  e2';                                                                       (* On retourne la valeur de la seconde expression *)
                              else
                                e1';;                                                                        (* On retourne la valeur de la première expression *)

(*
Brique primitives du langage.
On y ajoute toutes les fonctions et variables élémentaires du langage.
On utilise une Hashtbl pour ajouter des éléments à l'environnement.
*)
let pervasives (env: environment) =
    begin
        environment_replace env (string_of_operator Add) (Val (Function (fun (env1, e1) -> (* + *)
         (Function (fun (env2, e2) ->
          Int ((int_of_value' e1) + (int_of_value' e2)))))));
        environment_replace env (string_of_operator Sub) (Val (Function (fun (env1, e1) -> (* - *)
         (Function (fun (env2, e2) ->
          Int ((int_of_value' e1) - (int_of_value' e2)))))));
        environment_replace env (string_of_operator Mul) (Val (Function (fun (env1, e1) -> (* * *)
         (Function (fun (env2, e2) ->
          Int ((int_of_value' e1) * (int_of_value' e2)))))));
        environment_replace env (string_of_operator Lt) (Val (Function (fun (env1, e1) ->  (* < *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) < (int_of_value' e2) then
                    1 else 0))))));
        environment_replace env (string_of_operator Le) (Val (Function (fun (env1, e1) ->  (* <= *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) <= (int_of_value' e2) then 
                    1 else 0))))));
        environment_replace env (string_of_operator Gt) (Val (Function (fun (env1, e1) ->  (* > *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) > (int_of_value' e2) then
                    1 else 0))))));
        environment_replace env (string_of_operator Ge) (Val (Function (fun (env1, e1) ->  (* >= *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) >= (int_of_value' e2) then 
                    1 else 0))))));
        environment_replace env (string_of_operator Eq) (Val (Function (fun (env1, e1) ->  (* = *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) = (int_of_value' e2) then
                    1 else 0))))));
        environment_replace env (string_of_operator Neq) (Val (Function (fun (env1, e1) -> (* <> *)
         (Function (fun (env2, e2) ->
          Int (if (int_of_value' e1) <> (int_of_value' e2) then
                    1 else 0))))));
        environment_replace env "prInt" (Val (Function (fun (env1, e) ->                   (* prInt *)
         print_int (int_of_value' e);
         print_newline ();
         e)));
    end;
    env;;

