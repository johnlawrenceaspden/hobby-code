;functions

(define (identity x) x)

(define (_functiontest)
   (=(identity 2) 2))

;enumerations

(define (enumerate-interval low high)
  (if (> low high) () (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))

(define (_enumeratetest)
  (and (equal? (enumerate-interval 0 5) (list 0 1 2 3 4 5))
       (equal? (enumerate-tree eviltree) '(1 2 3 4 5 6 7 8 9 a 11 12 13))))


;Number theory

(define (square x) (* x x))

(define (divides? a b) (= 0 (remainder b a)))

(define (smallest-divisor n)
  (define (test-divisors i)
    (cond ((> (square i) n) n)
          ((divides? i n) i)
          (else (test-divisors (+ i 1)))))
  (if (< n 2) (error "smallest divisor of 1 undefined")
      (test-divisors 2)))

(define (prime? n) (if (> n 1) (= (smallest-divisor n) n) #f))

(define (fib n)
  (define (fibiter a b count n)
    (if (= count n) 
        a
        (fibiter b (+ a b) (+ 1 count) n)))
  (fibiter 0 1 0 n))

(define (_numbertheorytest)
  (and 
   (prime? 7) (not(prime? 6)) 
   (divides? 3 12) (not(divides? 5 12))
   (= (smallest-divisor 35) 5)
   (not (= (smallest-divisor 12) 3))
   (= (+(fib 5) (fib 6)) (fib 7))
   (= (fib 5) 5)
   (= (square 3) 9)
   ))




;list operations

(define (fold-right op initial sequence) ;(fold-right cons () (a b c)) is (cons a(cons b(cons c ())))
  (if (null? sequence) initial (op (car sequence) (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence) ;(fold-left cons () (a b c)) is (cons (cons (cons () a) b) c)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define accumulate fold-right)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (filter p sequence) (accumulate (lambda (x y) (if (p x) (cons x y) y)) () sequence))

(define (remove item sequence) (filter (lambda (x) (not (eq? x item))) sequence)) 

(define (flatmap proc seq) (accumulate append () (map proc seq)))

(define (_listtest)
  (equal? (fold-right cons () (list 1 2 3))                                (list 1 2 3))
  (equal? (fold-left  cons () (list 1 2 3))                                '((((). 1). 2). 3) )
  (equal? (fold-left (lambda (a b) (cons b a)) () (list 1 2 3))            (list 3 2 1))
  (equal? (fold-right / 6 (list 1 2 3))                                    1/4 ) 
  (equal? (fold-left / 6 (list 1 2 3))                                     1)
  (equal? (fold-left  + 0 (list 1 2 3 4 5 6 7))                            28)
  (equal? (accumulate + 0 (list 1 2 3 4 5 6 7))                            28)
  (equal? (accumulate-n * 1 (list (list 1 2 3) (list 4 5 6)))              (list 4 10 18))
  (equal? (filter prime? (list 1 2 3 4 5 6 7 8))                           (list 2 3 5 7))
  (equal? 
   (flatmap (lambda (n) (enumerate-interval 1 n)) (list 1 2 3 4 5))        (list 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5) )
  (equal? (remove 'x (list 1 2 'x 3 4 'x 5))                               (list 1 2 3 4 5))
  )

;matrices and vectors

(define (dot-product v w) (accumulate + 0 (map * v w)))

(define (matrix*vector m v)
  (map (lambda (b) (dot-product v b)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix*matrix m n)
  (let ((cols (transpose n)))(map (lambda (v)(matrix*vector cols v)) m)))

(define (_matrixtest)
  (let((v (list 1 2 3 4))
       (w (list 5 6 7 8))
       (m (list (list 1  2  3  4)
                (list 5  6  7  8)
                (list 9 10 11 12))))
  (and
   (equal? (dot-product v w) (+ 5 12 21 32))
   (equal? (matrix*vector m v) (list 30 70 110))
   (equal? (transpose m) '((1 5 9) (2 6 10) (3 7 11) (4 8 12)) )
   (equal? (matrix*matrix m (transpose m)) '((30 70 110) (70 174 278) (110 278 446)) )
   )))




;examples/tests

(define eviltree      '(((1 2) (3 4) 5 6 (((7)(8 9)) a  11 12 ()))13) )

(define (_testjohnlib)
  (if (and
       (_functiontest)
       (_numbertheorytest)
       (_enumeratetest)
       (_listtest)
       (_matrixtest)
       )
      
      (display "johnlib passed self-tests")
      (display "johnlib failed self-tests"))
  )

(if #t (_testjohnlib) (display "johnlib not tested"))
(newline)


















