#lang scheme

(provide (struct-out exn:meval))
(provide merror)

;special exception type for errors which are detected by the meta-evaluator.
;so that any exn:fails are errors in the evaluator code rather than in the code being meta-evaluated
(define-struct (exn:meval exn) ())
(define-syntax-rule (merror formatstring arg ...)
  (raise (make-exn:meval (format formatstring arg ...) (current-continuation-marks))))


(define (self-tests)
  (if (and
       (with-handlers ((exn:fail? (λ(v) #f))
                       (exn:meval? (λ(v) #t)))
         (merror "yo: ~a" 'yo)))
      "tests passed"
      "tests failed"
  ))