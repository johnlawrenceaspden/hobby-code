(define eviltree      '(((1 2) (3 4) 5 6 (((7)(8 9)) a  11 12 ()))13) )
(define (square x) (* x x))
(define seq-of-seqs '((1 2 3)(4 5 6)(7 8 9)(10 11 12)))
(define v '( 1 2 3 4 ))
(define w '( 2 3 4 5 ))
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 2) (3 4) (5 6) (7 8)))

(define (fib n)
  (define (fibiter a b count n)
    (if (= count n) 
        a
        (fibiter b (+ a b) (+ 1 count) n)))
  (fibiter 0 1 0 n))

;(define (sum-odd-squares tree)
;  (cond ((null? tree) 0)
;        ((number? tree) (if (odd? tree) (square tree) 0))
;        ((pair? tree) (+ (sum-odd-squares (car tree))
;                         (sum-odd-squares (cdr tree))))
;        (else 0)))
;
;(define (even-fibs n)
;  (define (next k)
;    (if (> k n)
;        ()
;        (let ((f (fib k)))
;          (if (even? f)
;              (cons f (next (+ k 1)))
;              (next (+ k 1))))))
;  (next 0))



(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (filter p sequence)
  (accumulate (lambda (x y) (if (p x) (cons x y) y)) () sequence))

(define (enumerate-interval low high)
  (if (> low high) () (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))


(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (filter number? (enumerate-tree tree))))))

(define (even-fibs n)
  (accumulate cons () (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fibs n)
  (map fib (enumerate-interval 0 n)))

(define (productofsquaresofoddelements tree)
  (accumulate * 1 (map square (filter odd? (filter number? (enumerate-tree tree))))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
  
(define (horner-eval poly x)
  (accumulate 
   (lambda (a b) (+(* b x) a)) 0  poly))

;(horner-eval (list 5 2 3 5 8) 100)

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (dot-product v w) (accumulate + 0 (map * v w)))

(define (matrix*vector m v)
  (map (lambda (b) (dot-product v b)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix*matrix m n)
  (let ((cols (transpose n)))(map (lambda (v)(matrix*vector cols v)) m)))

(define fold-right accumulate) ;(accumulate cons () (a b c)) is (cons a(cons b(cons c ())))

(define (fold-left op initial sequence) ;(fold-left cons () (a b c)) is (cons (cons (cons () a) b) c)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x)) ) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x() ) () sequence)))

seq-of-seqs
(reverse seq-of-seqs)


(fold-right cons () '(a b c)) ;    (a+(b+(c+0))
(fold-left cons () '(a b c)) ;  ((0+a)+b)+c)

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left  / 1 (list 1 2 3)) ; 1/6
(fold-right list () (list 1 2 3)) ;(1(2(3())))
(fold-left list () (list 1 2 3)) ; ((((()1)2)3)4) (op (op a b) c) = (op a (op b c)) and (op x initial)=(op initial x)










 








