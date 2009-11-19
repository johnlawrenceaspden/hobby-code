(defmacro macroexpandandexecute(&rest body)
  `(progn
     (format t "-----expression: ")
     (write ,@body)
     (format t "~%expansion:  ")
     (write (macroexpand-1 ,@body))
     (format t "~%evaluating now:~%")
     (let ((x (eval ,@body)))
     (format t "~%-----evaluation finished: result: ~a~%" x))))

(defmacro writeln (&rest body)
  `(format t "~{~a ~}~%" (list ,@body)))

(defmacro linebreak()
  `(progn
     (writeln) 
     (writeln "-------------------------")))

(defmacro m (&rest body) `(macroexpandandexecute ,@body))

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

(defmacro mywhen (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro myunless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))


;(defvar *when* '(mywhen t (write "Yup")(write "Yup")))

(defvar *a*)
(defvar *b*)
(defvar *c*)

(defvar *when* (quote
                 (if *a* 
                   (do-x)
                   (if *b*
                     (do-y)
                     (do-z)))))

(defvar *cond* (quote
                 (cond (*a* (do-x))
                       (*b* (do-y))
                       (*c* (do-z))
                       (t   (do-w)))))


(setf *a* nil)
(setf *b* nil)
(setf *c* nil)


(let ((*a* nil) (*b* t) (*c* nil))
  (m *cond*)
  )

(linebreak)

(dolist (x '(1 2 3 'elephant '(1 2 3))) 
  (print x))

(linebreak)

(dolist (x '(1 2 3 'elephant '(1 2 3))) 
  (print x) 
  (if (evenp x) 
    (return)))

(linebreak)

(dotimes (i 10) 
  (writeln i (* 2 i)))

(linebreak)

(do 
  ((x 1 (1+ x))) 
  ((= x 10) (* x x)) 
  (writeln x))

(linebreak)

(defun sumfromatob (a b) 
  (do* ((x a (1+ x))(sum x (+ sum x))) ((= x b) sum) (writeln x " " sum)))

(defun printsumfromatob (a b)
  (format t "Sum of integers from ~a to ~a: ~a~%" a b (sumfromatob a b)))

(printsumfromatob -9 9)
(printsumfromatob 0 10)

(linebreak)
(writeln "The eleventh fibonnaci number")

(defun fibrec (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ( t (+(fibrec (- n 1))(fibrec (- n 2))))))


(writeln "fib11=: "(do 
  ((a 0 b) 
   (b 1 (+ a b))
   (n 1 (1+ n)))
  ((> n 10) b)
  (writeln n a b)))

(writeln "fib11=: " (fibrec 11))

(linebreak)

(m '(do ((x 0 (1+ x))) ((>= x 10) ) (do-w)))

(linebreak)

(m '(dotimes (x 10) (do-w)))

(linebreak)


(defvar *some-future-date* (+ (get-universal-time) 0 ))

(do ()
  ((> (get-universal-time) *some-future-date*))
  (writeln "Waiting")
  (sleep 1))


(linebreak)

(let ((x 0))
    (loop
      (write "doom")
      (if (> x 10) (return))
      (writeln "it cannot be!")
      (setf x (1+ x))))

(linebreak)

(writeln 
  (do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
    (push i nums)))

(linebreak)

(m '(loop for i from 1 to 10 collecting i))

(linebreak)

(m '(loop for x from 1 to 10 summing (expt x 2)))

(linebreak)

(m '(loop for x across "the quick brown fox jumps over the lazy dog" counting (find x "aeiou")))

(linebreak)

(m '(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      doing (writeln a b)
      finally (return a)))

(linebreak)







