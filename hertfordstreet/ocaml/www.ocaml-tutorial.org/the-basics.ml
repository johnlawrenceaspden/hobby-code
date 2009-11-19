(*This is a single line comment*)

(* This is a 
 * multi line 
 * comment(*With an embedded bit*)*)


(*no automatic type conversions*)
(*allows inference*)
let average a b =
  (a +. b) /. 2.0;;


(*recursion must be explicitly allowed with let rec*)
let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

(*polymorphism*)
let give_me_a_three x = 3;;

