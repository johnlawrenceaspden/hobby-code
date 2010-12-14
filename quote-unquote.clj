;; Quote Unquote

;; For those having trouble with quote, eval, syntax quote, unquote, and splicing unquote, a kata:

;; Find a REPL, and type these forms in one by one.

;; Do not copy and paste them.

;; Type them. Into the REPL. One by one.

(def x '(* 3 5))

(def y (* 3 5)) 

x 

y 

(list 'println x (eval x) y) 

(list `println x (eval x) y) 

`(list println x (eval x) y) 

`(println x (eval x) y) 

`(println ~x (eval x) y)

`(println ~x ~(eval x) y)

`(println ~x ~(eval x) ~y)

`(println ~x ~(eval x) ~y ~@x)

;; Now play. Try variants of these expressions.