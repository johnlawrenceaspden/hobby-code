;; So today we're going to learn how to find the square root of two

;; Can anyone remember what it is? Or make a guess at it?

;; Okay, I'll take 1.414

;; Now, how good a guess is that? What do we need to do to it find out?

;; We need to square it. 

;; So here we have an extremely powerful computer, and it's running a language called Scheme, which is one of the most powerful and 
;; expressive languages in the world.

;; And our first task is to find out how to use it as a calculator, so that we can multiply 1.414 by 1.414 and see how close that is to two.




(sqrt 2)

;; so we're done.

; interact with the interpreter
2
"a"
+

(+ 2 3)



(define square (lambda (x) (* x x)))
(define average (lambda (a b) (/ (+ a b) 2)))
(define improve (lambda(x) (average x (/ 2 x))))

(square 1)
(square (improve 1))
(square (improve (improve 1)))
(square (improve (improve (improve 1))))
(square (improve (improve (improve (improve 1)))))