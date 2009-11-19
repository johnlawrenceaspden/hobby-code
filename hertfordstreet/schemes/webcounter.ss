#lang web-server/insta

(define (start request)
  (show-counter 0 request))

(define (show-counter n request)
  (local [(define (response-generator make-url)
            `(html (head (title "Counting example"))
                   (body
                    (a ((href ,(make-url next-number-handler)))
                       ,(number->string n)))))
          (define (next-number-handler request)
            (show-counter (+ n 1) request))]
    (send/suspend/dispatch response-generator)))