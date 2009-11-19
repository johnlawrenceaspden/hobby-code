Some 3;;

let a = [None; Some 3; Some 6; None];;

Some [1;2;3];;

match Sys.os_type with
    "Unix" | "Cygwin" -> "righteous"
  | "Win32" | "MacOS" -> "heinous"
  | _ -> failwith "this system is not supported"
;;

let double x =
  x*2
in
List.map double [1;2;3];;

let multiply n list =
  let f x =
    n * x
  in
List.map f list;;

multiply 3 [1;2;3;];;

multiply 5 [1;2;3];;

let double = multiply 2;;

double [1;2;3];;

let multiply n = List.map (( * ) n);;

multiply 3 [1;2;3];;

( * ) 3
