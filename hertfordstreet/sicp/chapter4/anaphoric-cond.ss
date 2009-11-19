#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))

;an anaphoric cond macro for plt scheme

(define-syntax acond
  (lambda (stx)
    (syntax-case stx (else)
      ((acond) (syntax #f))
      ((acond (else action)) (syntax action))
      ((acond (else action) _ ) (raise-syntax-error #f "else must be last clause" stx))
      ((acond (pred action))
       (with-syntax ((it-id (datum->syntax (syntax acond) 'it)))
         (syntax
          (let ((it-id pred))
            (if it-id action #f)))))
      ((acond (pred action) (etcp etca) ...)
       (with-syntax ((it-id (datum->syntax (syntax acond) 'it)))
         (syntax
          (let ((it-id pred))
            (if it-id action (acond (etcp etca) ...)))))))))


(define my-list '((a . 1)(b . 2)(c . 3)))

(check-equal? (acond) #f)
(check-equal? (acond (2 3)) 3)
(check-equal? (acond (2 it)) 2)

;note that cond can already do the things acond can do, if you wrap the consequent in a lambda
(check-equal? (acond ((assq 'z my-list) (cdr it)) ((assq 'b my-list) (cdr it))) 2)
(check-equal? (cond ((assq 'z my-list) => cdr)((assq 'b my-list) => cdr)) 2)

(check-equal? (acond (#f 3)(#f 5)('doom (format "~a" it))) 
              (cond  (#f 3)(#f 5)('doom => (λ(x)(format "~a" x)))))

(check-equal? (cond  (#f 23) (#f 'hi) (else 'oops))
              (acond (#f 23) (#f 'hi) (else 'oops)))


(acond (else 'hi)(#f 'doom))

(define (main) (syntax->datum (expand-once '(acond   (else 'oops)))))


