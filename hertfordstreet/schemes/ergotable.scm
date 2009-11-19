;(define o (open-output-file "greeting.txt"))
;(display "hello" o)
;(write-char #\space o)
;(display 'world o)
;(newline o)
;(close-output-port o)

(call-with-input-file "ergs.scm"
  (lambda (i)
    (let* ((a (read i))
           (b (read i))
           (c (read i)))
      (list a b c)
      )))