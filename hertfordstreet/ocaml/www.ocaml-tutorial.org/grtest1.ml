(* To compile this example: ocamlc graphics.cma grtest1.ml -o grtest1 *)

(* to run interactively from emacs 
   use ocamlmktop -o ocaml-graphics graphics.cma
   and then when tuareg-mode asks which caml-toplevel to use, 
   use ./ocaml-graphics*)

open Graphics;;

open_graph " 640x480";;

for i = 12 downto 1 do
  let radius = i * 20 in 
    set_color (if (i mod 2) = 0 then red else yellow);
    fill_circle 320 240 radius
done;;

read_line();;
