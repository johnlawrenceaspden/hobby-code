(defmacro macroexpandandexecute(&rest body)
  "When writing macros, this allows us to both expand and run them"
  `(progn
     (format t "-expression: ")
     (write ,@body)
     (format t "~%-expansion:  ")
     (write (macroexpand-1 ,@body))
     (format t "~%-evaluating now:~%")
     (let ((x (eval ,@body)))
     (format t "~%-evaluation finished: result: ~a~%" x))))

(defmacro writeln (&rest body)
  "A convenient print statement"
  `(format t "~{~a ~}~%" (list ,@body)))

(defmacro linebreak()
  `(progn
     (writeln) 
     (writeln "-------------------------")))

(defmacro m (&rest body) "alias for macroexpandandexecute" `(macroexpandandexecute ,@body))

(defun do-x ()
        (write "x"))

(defun do-y ()
        (write "y"))

(defun do-z ()
        (write "z"))

(defun do-w ()
  (write "w"))

(defun expand (a)
  (setf a (macroexpand-1 a)))

(writeln "Macro writing environment")
(linebreak)

(defun primep (number)
  "Is it a prime?"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Find the next prime"
  (loop for n from number when (primep n) return n))

;print out the primes up to 19
(do ((p (next-prime 0) (next-prime (1+ p)) ))
    (( > p 19 ))
    (format t "~d " p))

(linebreak)
(writeln "do-primes1: ;an attempt to write a macro to create that loop from (do-primes (p 0 19) (format t \"~d \" p))
;it works, but it leaks by evaluating the end expression many times.")

(defmacro do-primes1 (varspec &rest body)
  (let ((var (car varspec))
         (begin (cadr varspec))
         (end (caddr varspec)))
    `(do ((,var (next-prime ,begin) (next-prime (1+ ,var)))) 
       (( > ,var ,end))
       ,@body)))

(writeln "here is a good use of it")
(do-primes1 (p 0 19) (write p))
(writeln)
(writeln "here the abstraction breaks. the probability distribution is not quite what we expect")
(do-primes1 (p 0 (random 100)) (write p))

(linebreak)

(writeln "do-primes2 is the same, but using a better syntax, taking advantage of 'destructuring parameter lists'," 
         "and giving a subtle hint to whatever IDE we use that body is the body by using &body in place of &rest")

(defmacro do-primes2 ((p start end) &body body)
  `(do ((,p (next-prime ,start) (next-prime (1+ ,p))))
     (( > ,p ,end))
     ,@body))

(m '(do-primes2 (p 0 19) (write p)))
(writeln)
(m '(do-primes2 (p 0 (random 100)) (write p)))

(linebreak)

(writeln "do-primes3 attempts to solve this problem by introducing a new variable a to hold the end value")
(defmacro do-primes3 ((p start end) &body body)
  `(let ((a ,end))
       (do ((,p (next-prime ,start) (next-prime (1+ ,p))))
     (( > ,p a))
     ,@body)))

(m '(do-primes3 (p 0 19) (write p)))
(writeln)
(m '(do-primes3 (p 0 (random 100)) (write p)))
(writeln "this works well, but introduces another leak")


 
(writeln "number of primes up to 19" (let ((a 0)) (do-primes3 (p 0 19) (incf a 1)) a))
(writeln "number of primes up to 19" (let ((b 0)) (do-primes3 (p 0 19) (incf b 1)) b))

(writeln "the variable a in the macro is capturing our count")
(writeln "if you try to add the primes up (incf a p) then you get an infinite loop")
(linebreak)


(writeln "in the final do-primes we randomize the name of the ending variable, using a special function")
(defmacro do-primes ((p start end) &body body)
  (let ((a (gensym)))
  `(let ((,a ,end))
       (do ((,p (next-prime ,start) (next-prime (1+ ,p))))
     (( > ,p ,a))
     ,@body))))

(m '(do-primes (p 0 19) (write p)))
(writeln)
(m '(do-primes (p 0 (random 100)) (write p)))
(writeln)
(m '(do-primes (a 0 19) (write a)))
(writeln)
(writeln "number of primes up to 19" (let ((a 0)) (do-primes (p 0 19) (incf a 1)) a))
(writeln "number of primes up to 19" (let ((b 0)) (do-primes (p 0 19) (incf b 1)) b))
(writeln "a no longer collides with the gensym name")
(writeln)
(writeln "here's a function to add the primes")
(writeln "the answer should be " (+ 2 3 5 7))

(write 
  (let ((a 0)) 
    (do-primes (p 0 10) 
               (incf a p)) 
    a))


(writeln "here's a slightly longer loop")
(m '(do-primes (i 0 1000)
               (progn (format t ":")
                      (format t "~d " i))))
