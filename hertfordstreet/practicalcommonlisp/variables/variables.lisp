(defun foo (x y z) (+ x y z))

(write 
  (let ((x 10) (y 20) (z 5)) 
    (foo x y z)))

(defun scopes (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(scopes 12)
  
(dotimes (x 10) (format t "~d " x))


(write
  (let ((x 20))
  (let* ((x 10) (y (+ x 10))) 
    (list x y))))

(write
  (let ((x 20))
  (let ((x 10) (y (+ x 10))) 
    (list x y))))

(defparameter mystery
  (let ((count 0)) 
    (lambda () 
      (setf count (1+ count))
      (write count))))


(funcall mystery)
(funcall mystery)
(funcall mystery)
(funcall mystery)
(funcall mystery)

(defvar *fnlist*
    (let ((count 0))
      (list
        #'(lambda () (incf count))
        #'(lambda () (decf count))
        #'(lambda () count))))

(defvar *count* 0
  "Count of widgets made so far")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(format t "----dynamic variables------~%")

(defvar *x* 10)
(defun foo ()
  (format t "+X: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "-X: ~d~%" *x*))

(defun bar()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(bar)


(format t "------------------~%")

(defvar *a* 10)
(defvar *b* 20)
(write *a*)
(write *b*)
(rotatef *a* *b*)
(write *a*)
(write *b*)







