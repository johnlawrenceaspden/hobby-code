(load "johnlib.scm")


(define (enumerate-pairs n a b)
  (map (lambda (x) (list x n)) (enumerate-interval a b)))

(define (unique-pairs n)
(flatmap (lambda (x) (enumerate-pairs x 1 (- x 1)))  (enumerate-interval 2 n)))

(define (enumerate-triples n)
  (map (lambda (x) (append x (list n))) (unique-pairs (- n 1))))

(define (unique-triples n)
  (flatmap (lambda (x) (enumerate-triples x)) (enumerate-interval 2 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+(car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda(x) 
             (map (lambda (a) (cons x a)) 
                  (permutations (remove x s)))) 
           s)))

(define (triplesundernthatsumtos n s)
  (filter (lambda (x) (= s (+ (car x)(cadr x)(caddr x)))) (unique-triples n)))

(triplesundernthatsumtos 5 8)

(permutations '(a b c))
(prime-sum-pairs 5)
  



      


