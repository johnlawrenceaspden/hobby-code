;from an email by Thomas A Russ on comp.lang.lisp

(defun sum-list (lst)
  (cond
    ((null lst) nil)
    (t (+ (car lst) (sum-list (cdr lst))))))

(sum-list (list 1 2 3))

(defun test(sample-lists)
  (dolist (sample sample-lists)
    (format t "Sum of ~S = ~S~%" sample (sum-list sample))))

(test '((1 1 1) (2 2 2) (1 2 3 4) (3 4 5)))

;it should be possible to catch the error at run time, fix it, and continue with the computation!


