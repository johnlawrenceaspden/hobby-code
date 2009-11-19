#lang scheme

;;; a program as naturally written

(define (request-age-page)
  (begin (printf "Enter your age:")
         (read)))

(define (age-page-non-web)
  (local ((define age (request-age-page)))
    (cond [(>= age 18) (printf "Don't forget to vote!")]
          [else (printf "You'll be able to vote in ~a years" (- 18 age))])))


;;; macro to make functions that terminate
(define abort #f)

(define-syntax define-script
  (syntax-rules ()
    ((define-script (script-name arg ... ) body)
     (define (script-name arg ... )
       (abort body)))))

;;; web-like version where scripts can't return values, must call succeeding scripts
(define-script (submit-age age)
  (cond [(>= age 18) (printf "Don't forget to vote!")]
          [else (printf "You'll be able to vote in ~a years" (- 18 age))]))

(define-script (request-age-page-script)
  (begin (printf "Enter your age: ")
         (submit-age (read))))

(define-script (age-page-web)
  (request-age-page-script))


;;; another natural program
(define (request-num1)
  (printf "Enter first number:")
  (read))

(define (request-num2)
  (printf "Enter second number:")
  (read))

(define (adder)
  (let* ([n1 (request-num1)]
         [n2 (request-num2)])
    (printf "sum:~a~n" (+ n1 n2))))

;;;webified
(define (adder-page-web)
  (print "Enter first number: ")
  (submit1 (read)))

(define (submit1 n1)
  (printf "Enter second number: ")
  (submit2 n1 (read)))

(define (submit2 n1 n2)
  (printf "sum: ~a~n" (+ n1 n2)))









;;grab the terminating continuation for the define-script macro
(let/cc grab-abort
  (set! abort grab-abort))