#use "boolean.ml";;
#use "testUtils.ml";;

let prt_f f = print_endline (f_to_str f) ;;
let prt_vec x = prt_str_list (List.map f_to_str x) ;;

(* Test subst *)

let f1 = True ;;
let f2 = Var 'x';;
let f3 = And(Var 'y', True) ;;

let a1 = [] ;;
let a2 = [('x', true)] ;;
let a3 = [('x', false)] ;;
let a4 = [('x', true); ('y', false)] ;;

prt_vec (subst a1 [f1]) ;;
prt_vec (subst a2 [f2]) ;;
prt_vec (subst a4 [f3]) ;;
prt_vec (subst a4 [f2;f1;f3]) ;;

