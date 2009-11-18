;;;;Some stuff off some Ruby advocacy page
(mapcar (lambda (x) (* x 2)) '( 1 2 3 4 5 6))
(defun foo(n) (lambda (i) (incf n i)))
(setf a (foo 0))
(funcall a 3)
(funcall a 5)

(defun each-natural-number (fn)
  (loop for n from 1 to 100 do (funcall fn n)))

(each-natural-number (lambda (n) (format t "~A" n)))

(defmacro with-each-natural-number (n expr)
  `(each-natural-number (lambda(,n) ,expr)))

(with-each-natural-number n
  (format t "~D~%" n))