#lang scheme

(call-with-input-file "input.txt" 
  (λ(in) (call-with-output-file "output.txt" #:exists 'truncate/replace
    (λ(out) 
      (for ((l (in-lines in)))
        (fprintf out "~a~n" (regexp-replace* "search" l "replace"))))))) 

