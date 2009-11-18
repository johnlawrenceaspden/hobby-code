#lang scheme

(define (greet given (surname "Smith") #:space (space " ") #:hi (hi "Hello, "))
      (string-append hi given space surname))

(greet "John")
(greet "John" "Aspden")
(greet #:hi "Yo, " "John" "Aspden")
(greet #:space "-" "John" "Aspden")
(greet "John" #:hi "Hi! " #:space "_")

(define (((curry (a "chicken")) b) (c ""))
  (format "~a ~a ~a" a b c))
  
(((curry "chicken") "tikka") "masala")
(((curry) "tikka") "masala")
(((curry) "madras"))
