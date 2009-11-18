;;Schlemiel the painter. What are the real numbers?
;;A question on Joel Spolsky's weblog

(defun schlemiel-next-position (current-position work-remaining)
  (if (< work-remaining (+ current-position 1))
      current-position
      (schlemiel-next-position (+ current-position 1) (- work-remaining (+ current-position 1)))))

(schlemiel-next-position 0 15)
(schlemiel-next-position 0 (/(* 300 301) 2))

(let ((steps 0))
  (defun schlemiel-iter()
    (setf steps (schlemiel-next-position steps (/(* 300 301) 2)))
    steps)
  (defun reset ()
    (setf steps 0)))

(defun schlemiel-list (n &optional (reset t))
  (when reset (reset))
  (if (= n 0) '()
      (cons (schlemiel-iter) (schlemiel-list (- n 1) nil) )))

(setf painted (schlemiel-list 10))

(format t "~a" (mapcar #'(lambda (x y) (- x y)) painted (cons 0 painted)))
			