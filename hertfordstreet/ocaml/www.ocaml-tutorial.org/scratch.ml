module Gr = Graphics;;

let fluffy = Gr.open_graph " 640x480";;

Gr.set_color Gr.green;;
Gr.fill_circle 320 240 240;;

Gr.set_window_title "The ace of spades";;

Gr.set_color Gr.red;;
Gr.fill_circle 320 240 200;;

let coloured_circle colour radius =
  Gr.set_color colour;
  Gr.fill_circle 320 240 radius;;

coloured_circle Gr.yellow 180;;
coloured_circle Gr.blue 160;;

Gr.clear_graph fluffy;;

