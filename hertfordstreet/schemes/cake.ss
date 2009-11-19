(module cake scheme

  (provide print-cake)

  (define (print-cake n)
    (printf "   ~a    \n" (make-string n #\.))
    (printf " .-~a-.\n" (make-string n #\|))
    (printf " | ~a |\n" (make-string n #\space))
    (printf "---~a---\n" (make-string n #\-))))

