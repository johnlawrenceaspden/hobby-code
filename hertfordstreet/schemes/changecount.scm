
(define (ukcoinval coins)
  (cond ((= coins 2) 2)
        ((= coins 3) 5)
        ((= coins 4) 10)
        ((= coins 5) 20)
        ((= coins 6) 50)))

(define (uscoinval coins)
  (cond ((= coins 2) 5)
        ((= coins 3) 10)
        ((= coins 4) 25)
        ((= coins 5) 50)
        (else (error "Coin out of range"))))


(define (count-change coinval amount coins)
  (cond ((< amount 0) 0)
        ((= amount 0) 1)
        ((= amount 1) 1)
        ((= coins 1) 1)
        (else (+
               (count-change coinval (- amount (coinval coins) ) coins) 
               (count-change coinval amount (- coins 1))))))

(define (ukchange amount) (count-change ukcoinval amount 6))
(define (uschange amount) (count-change uscoinval amount 5))
