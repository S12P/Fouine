open Expr

open List

let mach (pile : code)=
  let rec aux p p'=match p with
  (*On renvoit le premier élément de la pile si la pile est vide il y a une erreur *)
  | [] -> begin try hd p' with  _ -> failwith "la pile est vide" end
  (* Si on a access on renvoie la fonction avec la pile modifiée *)
  (*| AccessId(x)::q -> f(q,e,access(x)::s)*)
  (*| (Access x)::q -> aux  q ((Access x)::p')*)
  (* Si on a Ope(Add) on prend ensuite les deux arguments qui sont dans la pile et on les additionne et on renvoie la fonction sur le reste de la pile *)
  | Op(Add)::q  -> begin
                      match p' with
                        | t1::t2::q2 -> aux q ((t1+t2)::q2)
                        | _ -> failwith "il n'y a pas d'arguments pour add"
                    end;
  (* Si on a Ope(Sub) on prend ensuite les deux arguments qui sont dans la pile et on les soustrait et on renvoie la fonction sur le reste de la pile *)
  | Op(Sub)::q  -> begin
                      match p' with
                        | t1::t2::q2 -> aux q ((t2-t1)::q2)
                        | _ -> failwith "il n'y a pas d'arguments pour sub"
                    end;
  (* Si on a Ope(Mul) on prend ensuite les deux arguments qui sont dans la pile et on les multiplie et on renvoie la fonction sur le reste de la pile *)
  | Op(Mul)::q  -> begin
                      match p' with
                        | t1::t2::q2 -> aux q ((t1*t2)::q2)
                        | _ -> failwith "il n'y a pas d'arguments pour mul"
                    end;
  (* Si on a prInt on prend ensuite l'argument qui est dans la pile et on l'affiche et on renvoie la fonction sur le reste de la pile *)
  | PrInt::q     ->  begin
                       match p' with
                        | t1::q2     -> print_int t1; print_newline (); aux q (t1::q2)
                        | _ -> failwith "il n'y a pas d'arguments pour prInt"
                      end;
  (* Si on a un entier on le met directement dans la pile *)
  | (C x)::q          -> begin
                      aux q (x::p')
                        end;
  | _                 -> failwith "Unimplemented";
        in aux pile [];;
