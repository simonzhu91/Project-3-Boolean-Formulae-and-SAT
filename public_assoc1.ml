#use "boolean.ml";;
#use "testUtils.ml";;

(* Example assignments *) 

let a1 = [('a', true)] ;; 
let a2 = [('a',true); ('a',false)] ;;
let a3 = [('a',true); ('b',false); ('c',true); ('a',false)] ;;

(* Test count_assoc *)

prt_int (count_assoc a1 'a');;
prt_int (count_assoc a2 'a');;
prt_int (count_assoc a3 'a');;

(* Test remove_assoc_all *)

prt_assignment (remove_assoc_all a1 'a');;
prt_assignment (remove_assoc_all a2 'a');;
prt_assignment (remove_assoc_all a3 'a');;
