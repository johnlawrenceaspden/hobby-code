(define (make-monitored f)
  (define callcount 0)
  (lambda (x)
    (cond ((equal? x 'how-many-calls) callcount)
          ((equal? x 'reset) (set! callcount 0))
          (else (set! callcount (+ callcount 1))
                (f x)))))

(define ms (make-monitored sqrt))

(ms 25)
(ms 36)
(ms 'how-many-calls)
(ms 'reset)
(ms 'how-many-calls)
(ms 81)
(ms 'how-many-calls)

