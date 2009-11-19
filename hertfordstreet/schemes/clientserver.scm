(define SERVICE-PORT 2000)
(define SERVER-HOST "localhost")

(define token-client
  (lambda (how-many-tokens)
    (if (<= how-many-tokens 0)
        '()
        (let-values ([(server->me me->server)
                (tcp-connect SERVER-HOST SERVICE-PORT)])
          (let ([token (read server->me)])
            (close-input-port server->me)
            (close-output-port me->server)
            (cons token (token-client (- how-many-tokens 1))))))))
            
            
(define server
  (let ([next-token -1])
    (lambda ()
      (let ([listener (tcp-listen SERVICE-PORT)])
        (let loop ()
          (let-values ([(client->me me->client)
                        (tcp-accept listener)])
            (set! next-token (+ next-token 1))
            (close-input-port client->me)
            (write next-token me->client)
            (close-output-port me->client))
          (loop))))))

(define (test)
  (thread server)
  (token-client 3))

      
