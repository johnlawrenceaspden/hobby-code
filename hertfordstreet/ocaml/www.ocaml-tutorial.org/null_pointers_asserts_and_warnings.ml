
(*An optional integer*)
Some 3;;

(*A list of optional integers*)
[None ; Some 3; Some 6; None];;

(*An optional list*)
Some [1;2;3];;
None;;

[Some [1;2;3];None];;

(*Exceptions*)
assert (Sys.os_type = "Win32");;

failwith "error message";;

match Sys.os_type with
    "Unix" | "Cygwin" ->  "ix"
  | "Win32" ->  "win"
  | _ -> failwith "this system is not supported"
;;

open Graphics;;
open Printf;;

open_graph " 640x480";;
for i = 12 downto 1 do
  let radius = i * 20 in
(*    prerr_endline ("radius is " ^ (string_of_int radius));*)
    eprintf "radius is %d\n" radius;
    set_color (if (i mod 2) = 0 then red else yellow);
    fill_circle 320 240 radius
done;;

