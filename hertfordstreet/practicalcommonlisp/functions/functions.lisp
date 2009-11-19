(defun sample-function (parameter)
  "Takes parameter and does something examplish with it"
  (format t "~a" parameter))

(sample-function "hello")

(defun no-args ()
  "a void function"
  (format t "hello from the void"))

(no-args)

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(verbose-sum 2 3)

(defun var-args (a b &optional c d)
  (if d
    (list a b c d)
    (if c
      (list a b c)
      (list a b))))


(var-args 1 2 3 4)
(var-args 1 2 3)
(var-args 1 2)

(defun default-arg (a b &optional (c 10))
  (list a b c))

(default-arg 1 2 3)
(default-arg 1 2)

(defun make-rectangle (a &optional (b a))
  "I calculate my other argument from my first one"
  (list a b))

(defun default-or-not (a &optional (b 2 supplied-b))
  (if supplied-b
    (list a b "<-actually supplied")
    (list a b)))

(default-or-not 1 2)
(default-or-not 1)

(defun add (stuff)
  (if stuff 
    (+ (car stuff) (add (cdr stuff)))
    0))

(defun add-us-up (name &rest stuff)
  (format t "~a: " name)
  (add stuff))

(defun key-parameters (&key a b c)
  (list a b c))

(defun return-is-not-at-the-end (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from return-is-not-at-the-end (list i j))))))


(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(defun foo (x)
  (* 2 x))

(plot (function foo) 0 3 1)
(plot #'foo 0 3 1)
(plot (lambda (x) (* x x)) 0 5 1)
(plot #'exp 0 4 2)












