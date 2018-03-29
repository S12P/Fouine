open Expr;;
open Inter;;
open Compi;;
open Machine;;

let print_result = function
    | None       -> ();
    | Int      i -> print_string "  --Int: "; print_int i; print_newline ();
    | Bool     b -> print_string "  --Bool: "; print_string (string_of_bool b); print_newline ();
    | Function _ -> print_string "  --Function"; print_newline ();;

let debug () =
                let environment_exception = (Env (Hashtbl.create 0)) in
                let environment = pervasives (Env (Hashtbl.create 0)) in
                if Array.length Sys.argv > 2 then
                    let arg=open_in Sys.argv.(2) in
                    let lexbuf = Lexing.from_channel arg in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let results = List.map (let result = interpreteur environment_exception environment in environment_clear environment_exception; result) asts in
                    List.iter2 (fun x y -> affiche_expression x; print_string ";;"; print_newline (); print_result y) asts results;
                else
                    let lexbuf = Lexing.from_channel stdin in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let results = List.map (let result = interpreteur environment_exception environment in environment_clear environment_exception; result) asts in
                    List.iter2 (fun x y -> affiche_expression x; print_string ";;"; print_newline (); print_result y) asts results;
                ;;

let normal () =
                let environment_exception = (Env (Hashtbl.create 0)) in
                let environment = pervasives (Env (Hashtbl.create 0)) in
                if Array.length Sys.argv > 1 then
                    let arg=open_in Sys.argv.(1) in
                    let lexbuf = Lexing.from_channel arg in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let results = List.map (let result = interpreteur environment_exception environment in environment_clear environment_exception; result) asts in
                    List.iter print_result results;
                else
                    let lexbuf = Lexing.from_channel stdin in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let results = List.map (let result = interpreteur environment_exception environment in environment_clear environment_exception; result) asts in
                    List.iter print_result results;
                ;;

let machine () =
                if Array.length Sys.argv > 2 then
                    let arg=open_in Sys.argv.(2) in
                    let lexbuf = Lexing.from_channel arg in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let resultat = List.concat (List.map compilateur asts) in
                    let resultat2 = mach resultat in print_int resultat2; print_newline ();
                else
                    let lexbuf = Lexing.from_channel stdin in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    let resultat = List.concat (List.map compilateur asts) in
                    let resultat2 = mach resultat in print_int resultat2; print_newline ();
                ;;

let interm () =
                match Array.length Sys.argv with
                 | 4 -> begin
                        let arg = open_in Sys.argv.(2) in
                        let code = open_out Sys.argv.(3) in
                        let lexbuf = Lexing.from_channel arg in
                        let parse () = Parser.main Lexer.token lexbuf in
                        let asts = parse () in
                        let resultat = List.concat (List.map compilateur asts) in List.iter (affichage_code_fichier code) resultat;
                        Printf.fprintf code "\n";
                        end
                 | 3 -> begin
                        let code = open_out Sys.argv.(2) in
                        let lexbuf = Lexing.from_channel stdin in
                        let parse () = Parser.main Lexer.token lexbuf in
                        let asts = parse () in
                        let resultat = List.concat (List.map compilateur asts) in List.iter (affichage_code_fichier code) resultat;
                        Printf.fprintf code "\n";
                        end
                 | _ -> print_string "Erreur : le fichier [file.code] est manquant."; print_newline ();;

let autotest () =
                let environment_exception = (Env (Hashtbl.create 0)) in
                let environment = pervasives (Env (Hashtbl.create 0)) in
                if Array.length Sys.argv > 2 then
                    let arg=open_in Sys.argv.(2) in
                    let lexbuf = Lexing.from_channel arg in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    print_string "Interpreteur :"; print_newline ();
                    List.iter (fun x -> let _ = interpreteur environment_exception environment x in environment_clear environment_exception) asts;
                    print_string "Machine :"; print_newline ();
                    let resultat = List.concat (List.map compilateur asts) in
                    let _ = mach resultat in ();
                else
                    let lexbuf = Lexing.from_channel stdin in
                    let parse () = Parser.main Lexer.token lexbuf in
                    let asts = parse () in
                    print_string "Interpreteur :"; print_newline ();
                    List.iter (fun x -> let _ = interpreteur environment_exception environment x in environment_clear environment_exception) asts;
                    print_string "Machine :"; print_newline ();
                    let resultat = List.concat (List.map compilateur asts) in
                    let _ = mach resultat in ();
                ;;

let _ = if Array.length Sys.argv > 1 then
          match Sys.argv.(1) with
          | "-debug"  -> debug () (*Avec debug avec ou sans fichier *)
          | "-machine"  -> machine () (* Compilation & execution avec ou sans fichier *)
          | "-interm"  -> interm () (* Compilation avec ou sans fichier *)
          | "-autotest"  -> autotest () (* Compilation avec ou sans fichier *)
          | _ -> normal() (* Cas ou il y a un fichier *)
        else normal () (* Sans debug sans fichier *)
