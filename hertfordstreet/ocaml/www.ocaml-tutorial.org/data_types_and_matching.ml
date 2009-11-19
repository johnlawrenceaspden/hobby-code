(*literal list*)

[1;2;3];;

(*list with the cons operator*)
let a = 1::2::3::[];;

(*list functions*)
List.length a;;
List.hd a;;
List.tl a;;
List.nth a 2;;

type pair_of_ints = { a : int; b : int};;

let a = { a = 3; b = 5};;

a;;
a.a;;
a.b;;


(*creating a new union type, with some examples*)
type foo = Nothing | Int of int | Pair of int * int | String of string;;

Nothing;;
Int 2;;
[Nothing; Int 2; Pair ( 4 , 5); String "doom"];;

(*enums are done in the same way*)
type sign = Positive | Zero | Negative;;
List.nth [Positive;Zero;Negative] 2;;

(*Recursive variants (for trees) *)

type binary_tree = Leaf of int | Tree of binary_tree * binary_tree;;

Leaf 3;;
Tree (Leaf 3, Leaf 5);;
Tree (Leaf 2, Tree (Leaf 3, Leaf 5));;

(*Parameterized variants*)
(*We can say what a tree is without having to say what it holds*)
type 'a binary_tree = Leaf of 'a | Tree of 'a binary_tree * 'a binary_tree;;

Leaf "hello";;
Tree (Leaf 2.0, Leaf 4.0);;
(*Tree (Leaf "hello", Leaf 2);; is still an error though*)

(*Patten matching on datatypes*)
type expr = Plus of expr * expr
	    |Minus of expr * expr
	    |Times of expr * expr
	    |Divide of expr * expr
	    |Value of string
;;

(*example expression n*(x+y)*)
let example = Times ( Value "n", Plus (Value "x", Value "y"));;

let rec to_string e =
  match e with
      Plus (left, right) -> "(" ^ (to_string left) ^ " + " ^ (to_string right) ^ ")"
    | Times (left, right) -> "(" ^ (to_string left) ^ " * " ^ (to_string right) ^ ")"
    | Value v -> v
;;

to_string (Plus (Value "n", Value "x"));;

Plus(Value "n" , Value "x");;
to_string example;;

let print_expr e = 
  print_endline (to_string e);;

print_expr example;;

let rec multiply_out e = 
  match e with
      Times (e1, Plus (e2, e3)) ->
	Plus (Times (multiply_out e1, multiply_out e2),
	      Times (multiply_out e1, multiply_out e3))
    | e -> e
;;

print_expr (multiply_out example);;


let factorize e =
  match e with
      Plus (Times (e1, e2), Times (e3, e4)) when e1 = e3 -> Times (e1, Plus (e2, e4))
    | e -> e
;;

factorize example;;
example;;
multiply_out example;;
print_expr(factorize (multiply_out example));;
print_expr example;;
print_expr (multiply_out example);;

