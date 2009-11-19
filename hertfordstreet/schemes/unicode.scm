(define (fluffy x)
  (display (integer->char x)))

(define (unicode a b)
  (fluffy a)
  (if (< a b) (unicode (+ a 1) b)))

(define (display-range a)
  (display a)
  (unicode (car a) (cadr a))
  (newline)
  (display "------------------")
  (newline))
  

(define (allunicode)
  (for-each
   display-range
   (make-known-char-range-list))
)