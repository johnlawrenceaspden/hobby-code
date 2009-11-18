;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cps) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (cfold* f z l)
  (if (null? l) 
      z
      (f (car l) z (lambda (y) (cfold* f y (cdr l))))))


(define (cfold f z l) (cfold* (lambda (x t g) (f x (g t))) z l))

(cfold + 0 '(1 2 3 4))
(cfold cons '() '(1 2 3 4))