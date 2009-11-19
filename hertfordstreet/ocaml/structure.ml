let average a b =
  let sum = a +. b in
    sum /. 2.0;;

let f a b =
  (a +. b) +. (a +. b) ** 2.
;;

let f a b =
  let x = a +. b in
    x +. x**2.
;;

f 1. 2.

let html = 
  let content = read_whole_file file in
    GHtml.html_from_string content
;;

ref 0;;

let my_ref = ref 0;;

my_ref := 100;;

!my_ref

let read_whole_channel chan =
  let buf = Buffer.create 4096 in
  let rec loop() =
    let newline = input_line chan in
      Buffer.add_string buf newline;
      Buffer.add_char buf '\n';
      loop()
  in
    try
      loop()
    with
	End_of_file -> Buffer.contents buf;;


let y = 2;;
let _ = y + 3 in ();;

let sum_list = List.fold_left ( + ) 0;;

sum_list [1;2;3;4;5;6];;

[1;2;3;4;5;6;7];;

2::1::[];;

