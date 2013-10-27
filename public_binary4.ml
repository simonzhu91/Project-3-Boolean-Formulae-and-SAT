#use "boolean.ml";;
#use "testUtils.ml";;

let prt_f f = print_endline (f_to_str f) ;;
let prt_vec x = prt_str_list (List.map f_to_str x) ;;

(* Test add *)

let n2 = [False;True] ;;
let n3 = [True ;True] ;;
let n4 = [Var 'a';True] ;;

prt_int (int_of_vec (subst [] (add n2 n2))) ;;
prt_int (int_of_vec (subst [] (add n2 n3))) ;;
prt_int (int_of_vec (subst [] (add n3 n2))) ;;
prt_int (int_of_vec (subst [] (add n3 n3))) ;;

prt_int (int_of_vec (subst [] (add (vec_of_int 6) (vec_of_int 5)))) ;;
prt_int (int_of_vec (subst [] (add (vec_of_int 5) (vec_of_int 7)))) ;;

prt_int (int_of_vec (subst [('a',true)] (add n3 n4))) ;;
prt_int (int_of_vec (subst [('a',false)] (add n3 n4))) ;;
