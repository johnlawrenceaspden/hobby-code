#lang scheme

;;;; from "A page about call/cc", http://www.madore.org/~david/computers/callcc.html

(call/cc (λ(k) (k 42))) ; ie. return 42

(call/cc (λ(k) (+ (k 42) 1729))) ; also return 42

(call/cc (λ(k) 42)) ; return 42 (never invokes continuation)
                    ;note that this is true in scheme, but not fundamental. 
                    ;in another language call/cc might always return void.

(display "entering infinite loop......")

(let ((cont #f))    ;loop forever
  (call/cc (λ(k) (set! cont k)))
  (cont #f))






