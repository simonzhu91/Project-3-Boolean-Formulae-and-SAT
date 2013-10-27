#use "boolean.ml";;
#use "testUtils.ml";;

(* Example assignments *)

let a1 = [('a', true)] ;;
let a2 = [('a',true); ('a',false)] ;;
let a3 = [('a',true); ('b',false); ('a',false)] ;;

(* Test change_assoc_all *)

prt_assignment (change_assoc_all a1 'a' false);;
prt_assignment (change_assoc_all a2 'a' false);;
prt_assignment (change_assoc_all a3 'a' false);;

(* Test assoc_last *)

prt_bool (assoc_last a1 'a');;
prt_bool (assoc_last a2 'a');;
prt_bool (assoc_last a3 'a');;
