let apply fn list = 
  if (null list) then []
  else (fn (head list))::(apply fn (tail list));;


let l = [1;2;3;4];;

open List;;

length l;;

(3,4);;

type pair_of_ints = { a : int; b: int; };;

{a=3; b=5};;

type foo = Nothing | Int of int | Pair of int * int | String of string;;

Nothing;;

[Int 2;Pair (4,5);];;

type sign = Positive | Zero | Negative;;

type binary_tree = Leaf of int | Tree of binary_tree * binary_tree;;

Leaf 3;;
(Leaf 1, (Leaf 3, Leaf 3));;

Tree (Leaf 1, Tree (Leaf 3, Leaf 3));;

type 'a binary_tree = Leaf of 'a | Tree of 'a binary_tree * 'a binary_tree;;

Leaf "hello";;
Leaf 2.0;;

type 'a list = Nil | :: of 'a * 'a list;;

Nil;;

1 :: Nil;;
1 :: 2 :: Nil;;

type expr = Plus of expr * expr
	    | Minus of expr * expr
	    | Times of expr * expr
	    | Divide of expr * expr
	    | Value of string
;;




let a = Times (Value "n", Plus (Value "x", Value "y"));;
let b = (Minus ((Plus (Value "2", Value "3")), Times (Value "3", Value "4")));;

let rec to_string e =
  match e with
      Plus   (left, right) -> "(" ^ (to_string left) ^ "+" ^ (to_string right) ^ ")"
    | Times  (left, right) -> "(" ^ (to_string left) ^ "*" ^ (to_string right) ^ ")"
    | Divide (left, right) -> "(" ^ (to_string left) ^ "/" ^ (to_string right) ^ ")"
    | Minus  (left, right) -> "(" ^ (to_string left) ^ "-" ^ (to_string right) ^ ")"
    | Value  (string) -> string
;;
	
to_string (Plus (Value "2", Value "3"));;
to_string a;;
to_string b;;

let print_expr e =
  print_endline (to_string e);;

print_expr b;;

let rec multiply_out e =
  match e with
      Times (e1, Plus (e2, e3)) ->
	Plus (Times (multiply_out e1, multiply_out e2),
	      Times (multiply_out e1, multiply_out e3))
    | Times (Plus (e1, e2), e3) ->
	Plus (Times (multiply_out e1, multiply_out e3),
	      Times (multiply_out e2, multiply_out e3))
    | Plus (left, right) -> Plus (multiply_out left, multiply_out right)
    | Divide (left, right) -> Divide (multiply_out left, multiply_out right)
    | Minus (left, right) -> Minus (multiply_out left, multiply_out right)
    | Times (left, right) -> Times (multiply_out left, multiply_out right)
    | Value v -> Value v
;;

to_string a;;
to_string (multiply_out a);;

let factorize e =
  match e with
      Plus (Times (e1, e2), Times (e3, e4)) when e1=e3 -> Times (e1, Plus (e2, e4))
    | Plus (Times (e1, e2), Times (e3, e4)) when e2=e4 -> Times (Plus (e1, e3), e4)
    | e->e
;;

to_string (factorize (multiply_out a));;

