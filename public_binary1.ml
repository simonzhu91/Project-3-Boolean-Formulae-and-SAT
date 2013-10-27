#use "boolean.ml";;
#use "testUtils.ml";;

let prt_vec x = prt_str_list (List.map f_to_str x) ;;

let n0 = [] ;;
let n1 = [True] ;;
let n2 = [False;True] ;;
let n3 = [True ;True] ;;
let n4 = [False;False;True] ;;

prt_int (int_of_vec n0) ;;
prt_int (int_of_vec n1) ;;
prt_int (int_of_vec n2) ;;
prt_int (int_of_vec n3) ;;
prt_int (int_of_vec n4) ;;

prt_vec (vec_of_int 1) ;;
prt_vec (vec_of_int 2) ;;
prt_vec (vec_of_int 3) ;;
prt_vec (vec_of_int 4) ;;
prt_vec (vec_of_int 13) ;;
