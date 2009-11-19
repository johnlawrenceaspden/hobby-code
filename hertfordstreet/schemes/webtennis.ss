#lang web-server/insta

(define phase-history-list '())

(define (start request)
  (set! phase-history-list (cons 'start phase-history-list))
  (phase-1 request '(start)))

(define (phase-1 request phases)
  (local [(define (response-generator make-url)
            `(html
              (body (h1 "Phase 1")
                    (p ,(format "current chain: ~a" phases))
                    (p ,(format "global record: ~a" phase-history-list))
                    (p (a ((href ,(make-url phase-2-handler)))
                       "click me!")))))
          (define (phase-2-handler request)
            (set! phase-history-list (cons 2 phase-history-list))
            (phase-2 (redirect/get) (cons 2 phases)))]
    (send/suspend/dispatch response-generator)))

(define (phase-2 request phases)
  (local [(define (response-generator make-url)
            `(html
              (body (h1 "Phase 2")
                    (p ,(format "current chain: ~a" phases))
                    (p ,(format "global record: ~a" phase-history-list))
                    (p (a ((href ,(make-url phase-1-handler)))
                       "click me!")))))
          (define (phase-1-handler request)
            (set! phase-history-list (cons 1 phase-history-list))
            (phase-1 (redirect/get) (cons 1 phases)))]
    (send/suspend/dispatch response-generator)))


