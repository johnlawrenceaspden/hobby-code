#lang scheme

;;;; from "A page about call/cc", http://www.madore.org/~david/computers/callcc.html

(call/cc (λ(k) (k 42))) ; ie. return 42

(call/cc (λ(k) (+ (k 42) 1729))) ; also return 42

(call/cc (λ(k) 42)) ; return 42 (never invokes continuation)
                    ;note that this is true in scheme, but not fundamental. 
                    ;in another language call/cc might always return void.

(define-syntax :
  (syntax-rules ()
    ((_ lbl) (call/cc (λ(k)(set! lbl k))))))

(define-syntax goto
  (syntax-rules ()
    ((_ lbl) (lbl #f))))


(let ((label1 #f) (counter 0))
  (: label1)
  (display "hello\n")
  (set! counter (+ counter 1))
  (when (< counter 10) (goto label1)))


;(display "entering infinite loop......")
;
;(let ((cont #f))    ;loop forever
;  (call/cc (λ(k) (set! cont k)))
;  (cont #f))

