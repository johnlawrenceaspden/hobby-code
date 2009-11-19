(require (lib "trace.ss"))

(define list-product
  (lambda (s)
    (let recur ((s s))
      (trace recur)
      (if (null? s) 1
          (* (car s) (recur (cdr s)))))))


(define list-product-with-bail
  (lambda (s)
    (call/cc
     (lambda (exit)
       (let recur ((s s))
         (trace recur)
         (cond ((null? s) 1)
               ((= (car s) 0) (exit 0))
               (else (* (car s) (recur (cdr s))))))))))


(define listwithzero '(2 3 0 4 5 6 7 8 9))

(list-product listwithzero)
(list-product-with-bail listwithzero)
