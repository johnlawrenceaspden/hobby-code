#lang racket

;; Two procedures

(define (fact-helper n total)
  (if (= n 0) total
      (fact-helper (- n 1) (* n total))))

(define (factorial n)
  (fact-helper n 1))

;; And the process it generates 
(factorial 10)
(fact-helper 10 1)
(fact-helper 9 10)
(fact-helper 8 90)
(fact-helper 7 720)
(fact-helper 6 5040)
(fact-helper 5 30240)
(fact-helper 4 151200)
(fact-helper 3 604800)
(fact-helper 2 1814400)
(fact-helper 1 3628800)
(fact-helper 0 3628800)
3628800

;; This is called a linear iteration. It's linear in time and constant in space.

(time (factorial 10))         ; cpu time: 0 real time: 0 gc time: 0
(time (factorial 100))        ; cpu time: 0 real time: 0 gc time: 0
(time (factorial 1000))       ; cpu time: 0 real time: 0 gc time: 0
(time (even? (factorial 2500)))       ; cpu time: 0 real time: 4 gc time: 0
(time (even? (factorial 5000)))       ; cpu time: 56 real time: 53 gc time: 56              0.000 seconds
(time (even? (factorial 10000)))      ; cpu time: 40 real time: 57 gc time: 0               0.040 seconds
(time (even? (factorial 20000)))      ; cpu time: 508 real time: 520 gc time: 164           0.34  seconds
(time (even? (factorial 40000)))      ; cpu time: 2484 real time: 2489 gc time: 892         1.59 seconds
(time (even? (factorial 80000)))      ; cpu time: 13448 real time: 12897 gc time: 5844      8.1 seconds
(time (even? (factorial 160000)))     ; cpu time: 66028 real time: 63365 gc time: 29760     36 seconds

;; Notice that it does the multiplications in the other order. That may explain why it seems ever so slightly slower!

(let-values  ([(result cpu real garbage) (time-apply factorial (list 10000))]) (- cpu garbage))