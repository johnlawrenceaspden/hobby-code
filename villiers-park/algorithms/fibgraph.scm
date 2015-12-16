#lang racket

(define (fib n str)
  (printf "fib~a [label=\"fib~a\"]\n" str n) ;; make a node for each call
  (if (< n 2)
      (begin
        
        n)
      (begin
        (let* [(s (- n 1))
               (newstring (string-append str (number->string s)))]
          (printf "fib~a->fib~a [label=\"~a\"]\n" str newstring s )
          (let [(a (fib s newstring))]
            (printf "fib~a->fib~a [label=\"~a\"]\n" newstring str a )
            (let* [(s (- n 2))
                   (newstring (string-append str (number->string s)))]
              (printf "fib~a->fib~a [label=\"~a\"]\n" str newstring s )
              (let [(b (fib s newstring))]
                (printf "fib~a->fib~a [label=\"~a\"]\n" newstring str b)
                  (+ a b))))))))



(printf "digraph G {
        rankdir=LR

        call [label=\"\"]\n")

(let [(val 7)]
  (printf "call->fib~a [label=\"~a\"]\n" val val)
  (let [(return (fib val (number->string val)))]
    (printf "fib~a->call [label=\"~a\"]\n" val return)
    (printf "}")))

