(define (make-monitored f)
  (define count 0)
    (lambda (x)
      (cond ((equal? x 'reset) (set! count 0))
            ((equal? x 'get)    count)
            (else (set! count (+ count 1)) (f x)))))


(define ms (make-monitored sqrt))
(define msin (make-monitored sin))

(msin 1.0)
(msin 2.0)

(ms 64)
(ms 49)
(ms 36)
(ms 25)
(ms 'get)
(ms 'reset)
(ms 9)
(ms 'reset)
(msin 'get)
(ms 1)
(msin 'reset)
(msin 'get)


