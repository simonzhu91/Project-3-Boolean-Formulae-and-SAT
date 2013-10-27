#use "boolean.ml";;
#use "testUtils.ml";;

(* Tests is_magic *) 

let n1 = pad (vec_of_int 1) 4 ;;
let n2 = pad (vec_of_int 2) 4 ;;
let n3 = pad (vec_of_int 3) 4 ;;
let n4 = pad (vec_of_int 4) 4 ;;
let n5 = pad (vec_of_int 5) 4 ;;
let n6 = pad (vec_of_int 6) 4 ;;
let n7 = pad (vec_of_int 7) 4 ;;
let n8 = pad (vec_of_int 8) 4 ;;
let n9 = pad (vec_of_int 9) 4 ;;

let square1 = [n1; n2; n3; n4; n5; n6; n7; n8; n9] ;;
let square2 = [n8; n1; n6; n3; n5; n7; n4; n9; n2] ;;

prt_bool (eval (is_magic square1) []) ;;
prt_bool (eval (is_magic square2) []) ;;
