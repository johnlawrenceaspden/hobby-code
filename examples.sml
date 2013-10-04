fun pow(x :int , y :int) =
    if  y = 0 then 1 else x*pow (x, y-1);

val p32=pow(3,2);

fun cube(x : int) = pow(x,3);

val sixtyfour=cube(4);

val fortytwo=pow(2,4)+cube(2) + pow(2+2,2) + 2;
val t=[];

val t=2::[];

val t=3::t; 

val t=[3,4,5,6];

fun last(ls:int list)=
    case ls of
        [] => 0 
      | [h] => h
      |  h::t => (last t);
                  

val t= last([3,4,5,6]);
val t=last([1,2]);

val t=[2+3,4,if 3<4 then 3 else 4];

val t=[6]::[7]::[8,9]::[];

val s = hd t;

val r =tl t;

val t = [(3,2),(9,8)];


(9,8);

#1 (9,8);

fun swap (pr: int*bool)=
    (#2 pr, #1 pr);

val t=swap(9,true);


fun div_mod(a:int, b:int)=
    (a div b, a mod b);

val t=div_mod(257,2);

fun sort_pair(pr: int*int)=
    if #1 pr < #2 pr then pr else (#2 pr,#1 pr);

val t= sort_pair(3,4);
val t= sort_pair(4,3);

val x4=((1,2),(2,3),(3,4));

val t= #2 (#3 x4);

             
val a = 10 ;

val b = a * 2;

val a = 5;

val c = b;


(* This is a comment *)

val x = 34; (* int *)
(* static environment x:int *)
(* dynamic environment x->34 *)

val y = 17; (* each of these is a binding *)
(* static environment x:int, y:int *)
(* dynamic environment x->34, y-> 17 *)

(* in REPL type use "test.sml"; *)

val z = ( x + y ) + ( y + 2);
(* static environment x:int, y:int, z:int (because int+int is int) *)
(* dynamic environment x->34, y-> 17, z->70 *)

val q=z+1;

val abs_of_z = if z<0 then 0-z else z;
(* static environment ..., abs_of_z = int *)
(* dynamic environment ..., abs_of_z = 70 *)

val abs_of_z_simpler = abs z ;

(* Syntax is how you write something *)
(* Semantics is what it means : type checking and evaluation *)


(* every kind of expression
syntax
type checking
evaluation *)

(* For variable bindings, 
type check and extend static environment
evaluate and extend dynamic environment*)

(* Addition
syntax e1 + e1
type check int if e1 and e2 are int
evaluate eval e1 and e2 and add them *)


(*Values are expressions, but not all expressions are values *)

(* eg () is of type unit, and evaluates to () *)

(* if
syntax if e1 then e2 else e3
type check e1 must be bool, e2 and e3 must have same type
evaluation if e1 true evaluate e2, if e1 false evaluate e3 *)

(* less than
syntax <
type check e1 and e2 are same type
evaluation eval e1, eval e2, compare, true if e1 < e2 *)


if x < 0 then 0 else 1;

val q = 2

val funn = 12
