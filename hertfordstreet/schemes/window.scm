(define (hello)
  (let* ((w (make-object frame% "Hello" #f 400 100))
         (e (make-object text-field% "Caption" w))
         (b (make-object button% "OK" w
              (lambda (button event)
                (send w set-label (send e get-value))
                ))))
    (send w show #t)))

(hello)

