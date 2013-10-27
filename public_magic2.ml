#use "boolean.ml";;
#use "testUtils.ml";;

let n1 = pad (vec_of_int 1) 4 ;;
let n2 = pad (vec_of_int 2) 4 ;;
let n3 = pad (vec_of_int 3) 4 ;;
let n4 = pad [Var 'a'] 4;;

let n10 = pad (vec_of_int 10) 4 ;;
let n11 = pad (vec_of_int 11) 4 ;;

(* Test is_digit *)

prt_bool (eval (is_digit n1) []) ;;
prt_bool (eval (is_digit n2) []) ;;
prt_bool (eval (is_digit n4) [('a',true)]) ;;
prt_bool (eval (is_digit n10) []) ;;
prt_bool (eval (is_digit n11) []) ;;

(* Test disjoint *)

prt_bool (eval (disjoint [n1; n2]) []) ;;
prt_bool (eval (disjoint [n1; n1]) []) ;;
prt_bool (eval (disjoint [n1; n4]) [('a',true)]) ;;
prt_bool (eval (disjoint [n1; n2; n3]) []) ;;
prt_bool (eval (disjoint [n3; n2; n3]) []) ;;

