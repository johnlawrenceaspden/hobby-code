#lang typed-scheme

(: digit-num : (Number -> (U Number String)))
(define (digit-num n)
  (cond [(<= n 9)    1]
        [(<= n 99)   2]
        [(<= n 999)  3]
        [(<= n 9999) 4]
        [else        "a lot"]))

(: factr : (Number -> Number))
(define (factr n)
  (if (zero? n)
      1
      (* n (factr (- n 1)))))


(: helper1 : (Number Number -> Number))
(define (helper1 n acc)
  (if (zero? n)
      acc
      (helper1 (- n 1) (* acc n))))
(: fact1 : (Number -> Number))
(define (fact1 n)
  (helper1 n 1))

(: facti : (Number -> Number))
(define (facti n)
  (define: (helperi [n : Number] [acc : Number]) : Number
    (if (zero? n)
        acc
        (helperi (- n 1) (* acc n))))
  (helperi n 1))

(: every? : (All (A) ((A -> Boolean) (Listof A) -> Boolean)))
;; Returns false if any element of lst fails the given pred, true if
;; all pass pred.
(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every? pred (cdr lst)))))

(every? even? (map + (map facti '(1 2 3 4 5 6))(map facti '(1 2 3 4 5 6))(map facti '(1 2 3 4 5 6))))
(every? odd?  (map + (map facti '(1 2 3 4 5 6))(map facti '(1 2 3 4 5 6))(map facti '(1 2 3 4 5 6))))

;;Haskell maybe
(define-struct: Nothing ())
(define-struct: (a) Just ((v : a)))
(define-type-alias (Maybe a) (U Nothing (Just a)))

(: find (Number (Listof Number) -> (Maybe Number)))
(define (find v l)
  (cond ((null? l) (make-Nothing))
        ((= v (car l)) (make-Just v))
        (else (find v (cdr l)))))

(if (find 3 '(1 2 4 5 6)) 'a 'b)