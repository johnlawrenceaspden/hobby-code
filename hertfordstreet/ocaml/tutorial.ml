(*From the tutorial at http://www.ocaml-tutorial.org/the_basics*)

(*This is a single line comment*)

(* This is a 
 * multi-line comment.
 *)

(*Primality test.*)

let average a b =
  (a +. b) /. 2.0;;
    
(average 2.0 3.4);;

1 + 2.7;;

float_of_int 2 +. 3.0;;

let rec range a b =
  if a>b then []
  else a :: range (a+1) b
;;

range 0 10;;

let range1to = range 1;;

range1to 6;;

let give_me_a_three x = 3;;

give_me_a_three 'a'

let average a b =
  (a +. b) /. 2.0;;

