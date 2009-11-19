(module contracts scheme

  (define (naturalnum? x)
    (and (number? x) (not (negative? x))))
  
    (define (posnum? x)
      (and (number? x) (positive? x)))

  (provide/contract
   [amount naturalnum?]
   [create (-> string? (and/c number? positive?) any)]
   [deposit (-> posnum? any)]
   [withdraw (-> posnum? any)])
  
  (define amount 0.01)

  (define (create name initial-deposit)
    (print name)
    (print initial-deposit)
    (set! amount initial-deposit))
  
  (define (deposit a) (set! amount (+ amount a)))
  (define (withdraw a) (set! amount (- amount a)))
  )

(require 'contracts)

amount
(deposit 10)
(withdraw 10)
(withdraw 10)
amount