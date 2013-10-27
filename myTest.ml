#use "boolean.ml";;
#use "testUtils.ml";;

(* More utility functions *)

let prt_f f = print_endline (f_to_str f) ;;

let prt_vec x = prt_str_list (List.map f_to_str x) ;;

let test_sat f = match (sat f) with
        None -> "None"
        | Some a -> print_string "% " ; prt_assignment a ; "SAT" 
;;

(* Example formulae *)
let f1 = True ;;
let f2 = False ;;
let f3 = And (True, True) ;;
let f4 = And (True, False) ;;
let f5 = Var 'x';;
let f6 = Not (Var 'x') ;;
let f7 = And(Or(Var 'x', Var 'y'), Var 'z') ;;

(* Example assignments *)
let a1 = [] ;;
let a2 = [('x', true)] ;;
let a3 = [('x', false)] ;;
let a4 = [('x', true); ('y', false) ; ('z', true)] ;;
let a5 = [('a',true)] ;;
let a6 = [('a',false)] ;;
let a7 = [('a',false); ('b',false); ('c',false); ('d',false)] ;;
let a8 = [('a',false); ('b',true); ('c',true); ('d',false)] ;;

(* Example vectors *)

let v0 = [] ;;
let v1 = [True] ;;
let v2 = [False;True] ;;
let v3 = [True ;True] ;;
let v4 = [False;False;True] ;;

let v5 = [Var 'a'] ;;
let v6 = [Var 'a'; False; Var 'a'] ;;

let x = [True; False; True; True] ;;
let y = [Var 'a'; Var 'b'; Var 'c'; Var 'd'] ;;

let n1 = pad (vec_of_int 1) 4 ;;
let n2 = pad (vec_of_int 2) 4 ;;
let n3 = pad (vec_of_int 3) 4 ;;
let n4 = pad (vec_of_int 4) 4 ;;
let n5 = pad (vec_of_int 5) 4 ;;
let n6 = pad (vec_of_int 6) 4 ;;
let n7 = pad (vec_of_int 7) 4 ;;
let n8 = pad (vec_of_int 8) 4 ;;
let n9 = pad (vec_of_int 9) 4 ;;

let nv = pad v6 4 ;;

(* Example squares *)

let square1 = [n1; n2; n3; n4; n5; n6; n7; n8; n9] ;;
let square2 = [n8; n1; n6; n3; n5; n7; n4; n9; n2] ;;
let square3 = [nv; n1; n6; n3; n5; n7; n4; n9; n2] ;;

(* Your test cases here *)



