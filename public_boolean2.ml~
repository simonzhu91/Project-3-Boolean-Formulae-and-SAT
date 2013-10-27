#use "boolean.ml";;
#use "testUtils.ml";;

(* Helper functions *)

let test_sat f = match (sat f) with
        None -> "None"
        | Some a -> print_string "% " ; prt_assignment a ; "SAT" 
;;

(* Test vars_of *)

let f1 = True ;;
let f2 = And (True, False) ;;
let f3 = Var 'x';;
let f4 = Exists('x', Or(Var 'x', Var 'y')) ;;

prt_char_list_sorted (vars_of f1) ;;
prt_char_list_sorted (vars_of f2) ;;
prt_char_list_sorted (vars_of f3) ;;
prt_char_list_sorted (vars_of f4) ;;

(* Test sat *)

let f1 = And (True, True) ;;
let f2 = And (True, False) ;;
let f3 = Var 'x' ;;
let f4 = Or(Var 'x', Var 'y') ;;
let f5 = And(Var 'x', Not (Var 'x')) ;;

print_endline (test_sat f1);;
print_endline (test_sat f2);;
print_endline (test_sat f3);;
print_endline (test_sat f4);;
print_endline (test_sat f5);;

