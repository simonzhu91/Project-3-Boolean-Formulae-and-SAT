(* CMSC 330 Project - Boolean Formulae & SAT *)

(* NAME: Simon Zhu *)

(* To load this file into the OCaml interpreter, type

   #use "boolean.ml"

   at the OCaml prompt
*)

type formula =
    False
  | True
  | Var of char
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | Forall of char * formula
  | Exists of char * formula

type assignment = (char * bool) list

type vec = formula list

(*----------------------------------------------------------
  function f_to_str : formula -> string. 

	converts formula into a string 
*)

let rec f_to_str f = match f with
    False -> "F"
  | True -> "T"
  | Var x -> Char.escaped x
  | And (f1,f2) -> "(" ^ (f_to_str f1) ^ " && " ^ (f_to_str f2) ^ ")"
  | Or (f1,f2) -> "(" ^ (f_to_str f1) ^ " || " ^ (f_to_str f2) ^ ")"
  | Not f1 -> "(! " ^ (f_to_str f1) ^ ")"
  | Forall (x,f1) -> "(forall " ^ (Char.escaped x) ^ " : " ^ (f_to_str f1) ^ ")"
  | Exists (x,f1) -> "(exists " ^ (Char.escaped x) ^ " : " ^ (f_to_str f1) ^ ")"
;;

(*  YOUR CODE PAST THIS POINT *)


(*----------------------------------------------------------
  function count_assoc : assignment char -> int. 

	Counts the number of times an association appears 
	for some symbol x in the assignment. 

	count_assoc [('a',true);('a',false)] 'a' returns 2.
*)

let rec count_assoc lst x = match lst with
       [] -> 0
| ((y,_)::t) -> if y = x then 1 + count_assoc t x
                else 0 + count_assoc t x
;;

(*----------------------------------------------------------
  function remove_assoc_all: assignment char -> assignment. 

	Removes all associations for some symbol x in the assignment. 

	remove_assoc_all [('a',true);('b',true);('a',false)] 'a'
	returns [('b',true)].
*)

let rec remove_assoc_all lst x = match lst with
	[] -> []
| ((y,a)::t) -> if y <> x then (y,a)::remove_assoc_all t x
                else remove_assoc_all t x
;;

(*----------------------------------------------------------
  function change_assoc_all: assignment char bool -> assignment. 

	Changes all associations for some symbol x in the 
	assignment to the specified boolean value. 

	change_assoc_all [('a',true);('a',false)] 'a' true
	returns [('a',true);('a',true)].
*)

let rec change_assoc_all lst x y = match lst with
	[] -> []
| ((a,b)::t) -> if a = x && b <> y then (a,y)::change_assoc_all t x y
                else (a,b)::change_assoc_all t x y 
;;

(*----------------------------------------------------------
  function assoc_last: assignment char -> bool. 

	Returns the value bound to the last association 
	for some symbol x in the assignment. 

	assoc_last [('a',true);('a',false)] 'x' returns false. 
*)

let rec assoc_last_helper lst x = match lst with
  [] -> false
| ((a,b)::t) -> if (a = x) then b else (assoc_last_helper t x)
;;

let assoc_last lst x =
  let q = (List.rev lst) in
      assoc_last_helper q x
;;

(*----------------------------------------------------------
  function eval : formula -> assignment -> bool. 

	evaluates the boolean formula on the given variable assignment 
	returns the result as an OCaml bool. 
*)

let rec traverse lst x = match lst with
    [] -> false
| ((a,b)::t) -> if (a = x) then b else traverse t x
;;

let rec eval f e = match f with 
    False -> false
  | True -> true
  | Var x -> traverse e x
  | And (f1,f2) -> eval f1 e && eval f2 e
  | Or (f1,f2) -> eval f1 e || eval f2 e
  | Not f1 -> not(eval f1 e)
  | Forall (x,f1) -> eval f1((x, true)::e) && eval f1((x, false)::e)
  | Exists (x,f1) -> eval f1((x, true)::e) || eval f1((x, false)::e)
;;

(*----------------------------------------------------------
  function vars_of : formula -> char list. 

	takes a formula and returns a list of the names of the 
	free variables of the formula. 
*)

let rec checking f = match f with
    False -> []
  | True -> []
  | Var x -> []
  | And (f1,f2) -> List.append (checking f1) (checking f2)
  | Or (f1,f2) -> List.append (checking f1) (checking f2)
  | Not f1 -> checking f1
  | Forall (x,f1) -> if (List.mem x (checking f1)) then (checking f1)
                     else x::(checking f1)
  | Exists (x,f1) -> if (List.mem x (checking f1)) then (checking f1)
                     else x::(checking f1)
;;

let rec not_contain x f a  = match a with
    False -> x
  | True -> x
  | Var c -> if List.mem c f then []
	     else [c]
  | And (f1,f2) -> List.append (not_contain x f f1) (not_contain x f f2)
  | Or (f1,f2) -> List.append (not_contain x f f1) (not_contain x f f2)
  | Not f1 -> not_contain x f f1
  | Forall (y,f1) -> not_contain x (y::f) f1
  | Exists (y,f1) -> not_contain x (y::f) f1
;;

let rec one lst newLst = match lst with
  [] -> newLst
  | a::b -> if not(List.mem a newLst) then  one b (a::newLst)
        else one b newLst
;;

let vars_of x = 
  one (not_contain [] [] x) []
;;

(*----------------------------------------------------------
  function sat : formula -> assignment option. 

	function returns Some a, where a is a satisfying assignment, 
	if the formula is satisfiable, or None otherwise. 
*)

let rec second_helper y l = 
  if y = 0 then List.rev l
  else second_helper (y/2) ((if(y mod 2) <> 1 then false else true)::l)
;;

let rec first_helper l1 l2 l =
    if List.length l2 < l then match l1 with 
     [] -> first_helper l1 (false::l2) l
    |(h::t) -> first_helper t (h::l2) l
    else List.rev l2
;;


let rec sat_helper y lst i =
  if (List.length lst) == 0 && (eval y []) then Some []
  else if ((List.length lst)==0 && not(eval y [])) then None
  else if i > ((List.length lst)*(List.length lst)) then None
  else let a = (List.combine lst (first_helper (second_helper i []) [] (List.length lst))) in
    if (eval y a) then Some a 
    else sat_helper y lst (i+1)
;;

let sat y = 
  sat_helper y (vars_of y) 0
;;
(*----------------------------------------------------------
  function int_of_vec : vec -> int. 

	takes a vec composed solely of Trues and Falses 
	returns the integer equivalent. 
*)

let rec power_two p = 
        if p = 1 then p else 2 * power_two (p-1)
;;

let rec sum_all x i = match x with
        [] -> 0
|  a::b -> if a = True then power_two i + sum_all b (i+1)
           else sum_all b (i+1)
;;

let int_of_vec x = 
        let count = 1 in sum_all x count
;;

(*----------------------------------------------------------
  function vec_of_int : int -> vec. 

	takes a non-negative integer 
	returns the corresponding vec. 
*)

let rec mod_divide x lst = 
    if x = 0 then List.rev lst
	  else mod_divide (x/2) ((if (x mod 2) = 1 then True else False)::lst)
   ;;

let vec_of_int x = 
	mod_divide (x) ([])
;;

(*----------------------------------------------------------
  function subst : assignment -> vec -> vec. 

	reduces the vec argument to a vec of all Trues and Falses 
	by replacing the variables in the vec according to the 
	assignment and then evaluating each bit. 
*)

let rec subst x y = match y with
	[] -> []
|  a::b -> if eval a x = true then True::(subst x b)
           else False::(subst x b)
;;

(*----------------------------------------------------------
  function eq : vec -> vec -> formula. 

	returns a formula representing whether the two bit vectors are equal. 
*)

let eq_helper x y = 
     (Or(And(x, y), And(Not(x), Not(y))))
;;

let rec eq x y = match x with
     [] -> False
|    [a] -> eq_helper a (List.hd y)
|    a::b -> match y with [] -> False
             | c::d -> And((eq_helper a c), eq b d)
;;

(*----------------------------------------------------------
  function add : vec -> vec -> vec. 

	returns a new vec representing the sum of the two vectors. 
*)

let xor x y = 
      Or(And(x, Not(y)), And(Not(x), y))
;;

let full_adder x y c = 
      ((xor (xor x y) c), (Or(And(x,y),And(c,xor x y))))
;;

let rec every_add x y c = match x with
      [] -> [c]
| a::b -> match full_adder a (List.hd y) c with 
                  (s, out) -> match y with [] -> [c]
                              | c::d -> s::every_add b d out
;;
 
let add x y =
      every_add x y False
 ;;

(*----------------------------------------------------------
  function pad : vec -> int -> vec. 

	pad v i returns a new vec that is equal to v 
	but whose length is the greater of i and the length of v. 
*)

let rec pad_helper y = 
      if y = 0 then []
      else False::(pad_helper (y-1))
;;

let rec pad x y =
      let i = List.length x in
         if y <= i then x
         else List.append x (pad_helper (y-i))
;;

(*----------------------------------------------------------
  function add_three : vec -> vec -> vec -> vec. 

	returns a new vector that represents the sum of 
	the three input vectors. 
*)

let add_three x y z =
	   (add (add x y) (pad z (List.length (add x y))))
;;

(*----------------------------------------------------------
  function is_digit : vec -> formula. 

	input is a vec of length 4 (i.e., exactly 4 boolean formulae). 

	returns a formula that is true if and only if the vec is 
	greater than or equal to 1 and less than or equal to 9.
*)

let rec generate_digit x i = 
      if i = 9 then eq x (pad (vec_of_int i) 4)
      else Or(eq x (pad (vec_of_int i) 4), (generate_digit x (i+1)))
;;

let is_digit x =
      generate_digit x 1
;;

(*----------------------------------------------------------
  function disjoint : vec list -> formula. 

	takes a list of vecs and returns a formula representing whether 
	all the vecs are different from each other.
*)

let rec move_y x y = match y with
      [] -> False
| [a] -> Not(eq x a)
| a::b -> And(Not(eq x a), move_y x b)
;;

let rec move_x x y = match x with
      [] -> False
| [a] -> True
| a::b -> match y with [] -> False
          | u::v -> And(move_y a v, move_x b v)

let disjoint x =
	     move_x x x
;;

(*----------------------------------------------------------
  function is_magic : vec list -> formula. 

	takes a list of exactly nine vecs and returns a formula 
	representing whether the list is a magic square. 
*)

let check_magic x = match x with [v1;v2;v3;v4;v5;v6;v7;v8;v9] ->
  let s1 = add_three v1 v2 v3 in
    let s2 = add_three v1 v4 v7 in
       let s3 = add_three v7 v8 v9 in
         let s4 = add_three v3 v6 v9 in
            let s5 = add_three v1 v5 v9 in
              let s6 = add_three v3 v5 v7 in
                let s7 = add_three v2 v5 v8 in
                   let s8 = add_three v4 v5 v6 in
                      And((eq s1 s2),And((eq s2 s3),And((eq s3 s4),And((eq s4 s5),And((eq s5 s6),And((eq s6 s7),(eq s7 s8)))))))
;;

let rec check_digits lst x = match lst with
      [] -> x
|   a::b -> check_digits b (And(x, is_digit a))
;;

let is_magic x =
      And((check_digits x True), And((disjoint x), (check_magic x)))    
;;
