(define (fnfactorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter) (+ counter 1))))
  (iter 1 1))

(fnfactorial 5)

(define (impfactorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n) 
          product
          (begin
            (set! product (* counter product))
            (set! counter (+ counter 1))
            (iter))))
    (iter)))

(impfactorial 5)
