(*using let*)
let average a b = 
  let sum = a +. b in 
    sum /. 2.0;;

let f a b =
   (a +. b) +. (a +. b) ** 2.
;;

let f a b = 
  let x = a +. b in
    x +. x ** 2.
;;

(*let creates a binding to a variable*)
let my_ref = ref 0;;
my_ref;;

(*here's assignment through the reference*)
my_ref := 100;;

(*dereference*)
!my_ref;;

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

