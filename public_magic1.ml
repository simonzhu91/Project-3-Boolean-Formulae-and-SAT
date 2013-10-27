#use "boolean.ml";;
#use "testUtils.ml";;

let prt_vec x = prt_str_list (List.map f_to_str x) ;;

(* Test pad *)
 
let n1 = pad (vec_of_int 1) 4 ;;
let n2 = pad (vec_of_int 2) 4 ;;
let n3 = pad (vec_of_int 3) 4 ;;
let n4 = pad [Var 'a'] 4;;

let n10 = pad (vec_of_int 10) 4 ;;
let n11 = pad (vec_of_int 11) 4 ;;

prt_vec n1 ;;
prt_vec n2 ;;
prt_vec n3 ;;
prt_vec n4 ;;
prt_vec n10 ;;
prt_vec n11 ;;

(* Test add_three *)

prt_int (int_of_vec (subst [] (add_three n1 n1 n1))) ;;
prt_int (int_of_vec (subst [] (add_three n1 n2 n3))) ;;
prt_int (int_of_vec (subst [('a',true)] (add_three n1 n2 n4))) ;;
prt_int (int_of_vec (subst [('a',false)] (add_three n1 n2 n4))) ;;

