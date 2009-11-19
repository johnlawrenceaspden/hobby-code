#lang scheme

;SICP lecture 9a register machines

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))


(define-machine gcd
  (registers a b t)
  (controller
   loop (branch (zero? (fetch b)) done)
        (assign t (remainder (fetch a) (fetch b)))
        (assign a (fetch b))
        (assign b (fetch t))
        (goto loop)
   done))

(define (remainder n d)
  (if (< n d) n (remainder (- n d) d)))

(define (fact n)
  (if (= n 1) 1 (* n (fact (- n 1)))))

(define-machine fact
  (registers ...)
     (assign continue done)
loop (branch (= 1 (fetch n)) base)
     (save continue)
     (save n)
     (assign n (-1+ (fetch n)))
     (assign continue aft)
     (goto loop)
aft  (restore n)
     (restore continue)
     (assign val (* (fetch n) (fetch val)))
     (goto (fetch continue)))
base (assign val (fetch n))
     (goto (fetch continue))
done)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


         (assign continue fib-done)
fib-loop ; N contains argument, continue is recipient
         (branch (< (fetch n) 2) immediate-answer)
         (save continue)
         (assign continue after-fib-n-1)
         (save n)
         (assign n (- (fetch n) 1))
         (goto fib-loop)
after-fib-n-1
         (restore n)
         ;(restore continue) restore and save leave everything unchanged
         (assign n (- (fetch n) 2))
         ;(save continue)
         (assign continue after-fib-n-2)
         (save val)
         (goto fib-loop)
after-fib-n-2
         (assign n (fetch val))
         (restore val)
         (restore continue)
         (assign val (+ (fetch n)))
         (goto (fetch continue))
immediate-answer
         (assign val (fetch n))
         (got (fetch continue))
fib-done
         








