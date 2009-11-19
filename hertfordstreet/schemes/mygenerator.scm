(define (make-generator)
  (letrec ((savedcontinuation #f)
           (exitcontinuation #f)
           (yeild (lambda (a)
                    (call/cc
                     (lambda(cont)
                       (set! savedcontinuation cont)
                       (exitcontinuation a))))))
    (lambda ()
      (call/cc
       (lambda (cont)
         (set! exitcontinuation cont)
         (if savedcontinuation (savedcontinuation))
         (let loop ()
           (yeild 'a)
           (yeild 'b)
           (yeild 'c)
           (loop)))))))

(define gen1 (make-generator))
(define gen2 (make-generator))

(display "hoping for abcaabcb") (newline)
(gen1) (gen1) (gen1)
(gen2)
(gen1) (gen1) (gen1)
(gen2)

