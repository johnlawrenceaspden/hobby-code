#!/usr/bin/ocamlrun ocaml

let year_size = 365.;;

let rec birthday_paradox prob people =
  let prob' = (year_size -. float people) /. year_size *. prob in
    if prob' < 0.5 then
        Printf.printf "answer = %d\n" (people+1)
    else
      birthday_paradox prob' (people+1);;

birthday_paradox 1.0 1;;
