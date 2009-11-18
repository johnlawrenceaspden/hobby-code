#lang scheme

;;ruminations on the continuations section of "The Scheme Programming Language".
;; http://scheme.com/tspl3/further.html#./further:h4

;making explicit some of the continuations in (if (null? x) (quote ()) (cdr x))
;I don't know how to make explicit the continuation waiting for the value of null? for instance

(define (supercdr x)
  (if (null? x) (quote ()) (cdr x)))

(define (cpsnull? x c)
  (c (null? x)))

(define (cpscdr x c)
  (c (cdr x)))

(define (cpssupercdr x c)
  (cpsnull? x (λ(r) (if r (c (quote ())) (cpscdr x c)))))

(define (testsupercdr)
  (equal?
   (list (supercdr '(a b c))
         (supercdr '()))
   (list (cpssupercdr '(a b c) (λ(x)x))
         (cpssupercdr '() (λ(x)x)))))

;;simple call/cc examples

(define (simpleexamples)
  (list
   
   (call/cc
    (λ(k) (* 5 4)))
   
   (call/cc
    (λ(k) (* 5 (k 4))))
   
   (+ 2
      (call/cc
       (lambda (k)
         (* 5 (k 4)))))      ))

(define (testsimpleexamples) (equal? (simpleexamples) '(20 4 6)))

;;nonlocal exit from recursion

(define (product ls)
  (call/cc
   (λ(break)
     (let f ((ls ls))
       (cond
         ((null? ls) 1)
         ((= (car ls) 0) (break 0))
         (else (* (car ls) (f (cdr ls)))))))))


(define (testproduct)
  (and (equal? (product '(1 2 3 4 5 6)) 720)
       (equal? (product '()) 1)
       (equal? (product '(1 2 3 0 5 6)) 0)))

;;makes brain hurt...
(define (hey) 
  (((call/cc (λ(k)k)) (λ(x)x)) "HEY!"))

;there doesn't appear to be any way to test this:
(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (λ(k) (set! retry k) 1))
        (* x (factorial (- x 1))))))

;type at the REPL
;(factorial 4)
;(retry 1)
;(retry 2)
;(factorial 6)
;(retry 2)

;;testing infrastructure
(define (testall)
  (and (testsimpleexamples)
       (testproduct)
       (testsupercdr)
       (equal? "HEY!" (hey))))

(display (if (testall) "tests passed" "tests failed"))


