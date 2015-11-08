#lang racket

;; A procedure
(define (factorial n)
  (if (< n 2) 1
      (* 2 (factorial (- n 1)))))

;; And the process it generates 
(factorial 10)
(* 10 (factorial 9))
(* 10 (* 9 (factorial 8)))
(* 10 (* 9 (* 8 (factorial 7))))
(* 10 (* 9 (* 8 (* 7 (factorial 6)))))
(* 10 (* 9 (* 8 (* 7 (* 6 (factorial 5))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (factorial 4)))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (factorial 3))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (factorial 2)))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (* 2 1)))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 2))))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 6)))))))
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 24))))))
(* 10 (* 9 (* 8 (* 7 (* 6 120)))))
(* 10 (* 9 (* 8 (* 7 720))))
(* 10 362880)
3628800

;; This is called a linear recursion. It's linear in time and space.

(time (factorial 10))         ; cpu time: 0 real time: 0 gc time: 0
(time (factorial 100))        ; cpu time: 0 real time: 1 gc time: 0
(time (factorial 1000))       ; cpu time: 0 real time: 1 gc time: 0
(time (even? (factorial 2500)))       ; cpu time: 0 real time: 2 gc time: 0
(time (even? (factorial 5000)))       ; cpu time: 8 real time: 8 gc time: 0                 0.008 seconds
(time (even? (factorial 10000)))      ; cpu time: 120 real time: 123 gc time: 76            0.044 seconds
(time (even? (factorial 20000)))      ; cpu time: 1836 real time: 1835 gc time: 1404        0.43 seconds
(time (even? (factorial 40000)))      ; cpu time: 2408 real time: 2404 gc time: 908         1.5 seconds
(time (even? (factorial 80000)))      ; cpu time: 14176 real time: 14159 gc time: 6056      8.1 seconds
(time (even? (factorial 160000)))     ; cpu time: 59268 real time: 59209 gc time: 25512     33 seconds

;; Although it looks more as though it's quadratic (or worse!). Why is that?

(let-values  ([(result cpu real garbage) (time-apply factorial (list 10000))]) (- cpu garbage))