open Expr;;


(*
    Fonction de compilation du code.
*)
(* expr -> code *)
let rec compilateur = function
    | Val (Int v)            -> [C v];
    | Ope (e1, o, e2)        -> List.append (List.append (compilateur e1) (compilateur e2)) [Op o];
    | App ((Ide "prInt"), e) -> List.append (compilateur e) [PrInt];
    | _                      -> failwith "Unimplemented";;
