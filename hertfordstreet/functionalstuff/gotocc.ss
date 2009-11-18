#lang scheme

;using continuations to provide a backwards goto

(define-syntax :
  (syntax-rules ()
    ((_ lbl) (call/cc (λ(k)(set! lbl k))))))

(define-syntax goto
  (syntax-rules ()
    ((_ lbl) (lbl #f))))

;the program

(let ((label1 #f) (label2 #f) (counter 1))
  (: label1)
  (display "hello")
  (when (even? counter) (goto label2)) ;this is so cheating.... only works if label2 has been executed
  (display "\n")
  (: label2)
  (set! counter (+ counter 1))
  (when (< counter 10) (goto label1)))