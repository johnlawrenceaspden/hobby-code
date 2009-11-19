(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+
               (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount
                    (first-denomination coin-values))
                   coin-values)))))

(define (first-denomination l)(car l))
(define (except-first-denomination l) (cdr l))
(define (no-more? l) (null? l))

(define us-coins (list 1 5 10 25 50 ))
(define uk-coins (list 1 2 5 10 20 50))

(cc 100 us-coins)
(cc 100 uk-coins)
         
  