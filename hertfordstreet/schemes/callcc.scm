(load "macroexamples.scm")
;defining a generator using call/cc (I don't understand)

(define (counter n)
  (letrec ((generator
            (lambda (yield)
              (let counter ((n n))
                (call/cc
                 (lambda (continue)
                   (set! generator (lambda (k)
                                     (set! yield k)
                                     (continue n)))
                   (yield n)))
                (counter (+ n n))))))
    (lambda () (call/cc
                (lambda (yield)
                  (generator yield))))))

;(define a (counter 10))
;(define b (counter 10))
;
;(a)
;(a)
;(b)
;(b)
;(a)

(define (list->generator list finished-thunk)
  (letrec ([caller (void)]
           [entry-point (lambda ()
                          (call/cc (lambda (k)
                                     (set! caller k)
                                     (cond
                                       [(restoring-from-save?) (continue-from-restore-point!)]
                                       [else (do-work list)]))))]
           
           [saved-point (void)]
           [restoring-from-save? (lambda() (not (void? saved-point)))]
           [continue-from-restore-point! (lambda () (saved-point))]
           [save-point! (lambda (f) (set! saved-point f))]
           [do-work (lambda (list)
                      (cond
                        [(empty? list) (caller (finished-thunk))]
;                        [else 
;                         (save-point! (lambda() (do-work (rest list))))
;                         (caller (first list))]
                        [else
                         (call/cc (lambda (k) 
                                    (save-point! k)
                                    (caller (first list))))
                                  (do-work (rest list))]
                        
                        
                        
                        
                        ))])
    entry-point))


(define traffic-light (list->generator '(red red&yellow green yellow) (lambda() (void))))

(for (10) (display (traffic-light))(newline))

















