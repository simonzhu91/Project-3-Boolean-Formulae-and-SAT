#use "boolean.ml";;
#use "testUtils.ml";;

let prt_f f = print_endline (f_to_str f) ;;

(* Test eq *)

let n0 = [False] ;;
let n1 = [True] ;;
let n2 = [Var 'a'] ;;

let a1 = [('a',true)] ;;
let a2 = [('a',false)] ;;

let result = eq n0 n1;;
print_string "% " ; prt_f result ;;
prt_bool (eval result []) ;;

let result = eq n2 n1;;
print_string "% " ; prt_f result ;;
prt_bool (eval result a1) ;;
prt_bool (eval result a2) ;;
