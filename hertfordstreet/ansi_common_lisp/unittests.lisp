(defparameter *testlist* nil)

(defmacro deftest(test result)
  `(setf *testlist* (append *testlist* (list (list ',result (lambda() ,test) ',test)))))

(defun runtests()
  (let ((acc nil))
    (dolist (i *testlist*)
      (format t "test: ~S ~%(expects ~S) ~%" (caddr i) (car i))
      (let ((x (funcall (cadr i))))
	(format t "=> ~S " x)
	(if (equal x (car i))
	    (progn
	      (format t "[PASS]~%")
	      (setf acc (cons x acc)))
	    (progn
	      (format t "[FAIL]")
	      (setf acc 'TESTS-FAILED!!!)
	      (return)))))
    acc))

;evaluate (runtests) to run the tests!

