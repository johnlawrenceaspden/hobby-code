#lang scheme

;we want to instrument the divides? operation. Is a class or a closure clearer?
(define (divides? x n)
  (printf "called ~a ~a ~n" x n)
  (= (remainder n x) 0))

(define (prime? n divides?)
  (cond ((= n 1) #f)
        ((= n 2) #t)
        (else (let ((lim (inexact->exact (floor (sqrt n)))))
                (let loop ((t 2))
                  (if (divides? t n) #f
                      (if (< t lim) (loop (+ t 1)) 
                          #t)))))))

(map (λ(n) (prime? n divides?)) '(1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21))

(let ()
  
  (define-values (divides? reset-div-count get-div-count)
    (let ([divcount 0])
      (values
       (λ(x n) (set! divcount (+ divcount 1)) (= (remainder n x) 0))
       (λ() (set! divcount 0))
       (λ() divcount))))
  
  
  (reset-div-count)
  (list (get-div-count)
        (map (λ(n) (prime? n divides?)) '(1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18))
        (get-div-count)))

(let ()
  
  (define divider% 
    (class object%
      (define divcount 0)
      (super-new)
      (define/public (get-count) divcount)
      (define/public (reset-count) (set! divcount 0))
      (define/public (divides? x n) (set! divcount (+ divcount 1)) (= (remainder n x) 0))))
  (define a-div (new divider%))
  (define (divides? x n) (send a-div divides? x n))
  
  (list (send a-div get-count)
        (map (λ(n) (prime? n divides?)) '(1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19))
        (send a-div get-count)))

(let()
  
  (define-values (divides? reset-div-count get-div-count)
    (let ([divcount 0])
      (define (divides? x n) (set! divcount (+ divcount 1)) (= (remainder n x) 0))
      (define (reset-div-count) (set! divcount 0))
      (define (get-div-count) divcount)
      (values divides? reset-div-count get-div-count)))
  
  (reset-div-count)
  (list (get-div-count)
        (map (λ(n) (prime? n divides?)) '(1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20))
        (get-div-count)))

(let ()
  
  (close-over ((divcount 0))
    ((divides? x n)    (set! divcount (+ divcount 1)) (= (remainder n x) 0))
    ((reset-div-count) (set! divcount 0))
    ((get-div-count)   divcount))
  
  (reset-div-count)
  (list (get-div-count)
        (map (λ(n) (prime? n divides?)) '(1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21))
        (get-div-count)))
  

;the last one needs the help of this macro.
(define-syntax close-over
  (syntax-rules ()
    ((_ vars ((name lambdalist ...) body ...) ...) 
     (define-values (name ...) 
       (let vars  
         (values 
          (λ (lambdalist ...) body ...) ...))))))

