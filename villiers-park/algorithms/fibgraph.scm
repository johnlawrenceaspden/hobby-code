#lang racket

(define (fib n str)
  (if (< n 2)
      (begin
        ;(printf "fib~a [label=\"fib~a\", color=black,fontcolor = white, style = filled]\n" str n)
        (printf "fib~a [label=\"fib~a\"]\n" str n)
        1)
      (begin
        (printf "fib~a [label=\"fib~a\"]\n" str n)
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

(let [(val 6)]
  (printf "call->fib~a [label=\"~a\"]\n" val val)
  (let [(return (fib val (number->string val)))]
    (printf "fib~a->call [label=\"~a\"]\n" val return)
    (printf "}")))

