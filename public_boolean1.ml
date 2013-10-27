#use "boolean.ml";;
#use "testUtils.ml";;

let f1 = True ;;
let f2 = False ;;
let f3 = And (True, True) ;;
let f4 = And (True, False) ;;
let f5 = Var 'x';;
let f6 = Not (Var 'x') ;;
let f7 = And(Or(Var 'x', Var 'y'), Var 'z') ;;

let a1 = [] ;;
let a2 = [('x', true)] ;;
let a3 = [('x', false)] ;;
let a4 = [('x', true); ('y', false) ; ('z', true)] ;;

(* Test eval *)

prt_bool (eval f1 a1) ;;
prt_bool (eval f2 a1) ;;
prt_bool (eval f3 a1) ;;
prt_bool (eval f4 a1) ;;
prt_bool (eval f5 a2) ;;
prt_bool (eval f5 a3) ;;
prt_bool (eval f6 a3) ;;
prt_bool (eval f7 a4) ;;

