(proclaim '(optimize (speed 3) (debug 0) (safety 0)))

(defun test ()
  (let ((list (loop for i from 1 to 50000000 collect i)))
    (loop for i in list sum i)))

(test)
