(eval '(+ 1 2 3))

(eval '(format t "Hello"))

(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

(map 'vector #'(lambda (x) (* 2 x)) #(2 3 4))

(disassemble (lambda (x) x))

(compile  nil '(lambda (x) (+ x  2)))

(defmacro nil! (x)
  (list 'setf x nil))

(nil! x)

(macroexpand-1 '(nil! x))

((lambda (expr)
   (apply #'(lambda (x) (list 'setf x nil)) (cdr expr)))
 '(nil! a))

(setf a 1 b 2)
`(a b c)

`(a is ,a and b is ,b)

(defmacro nil! (x)
  `(setf ,x nil))

(setf lst '(a b c))

`(lst is ,lst)

`(its elements are ,@lst)


(defmacro while (test &rest body)
  `(do ()
    ((not ,test))
    ,@body))


(let ((x 0))
  (while (< x 10)
    (princ x)
    (incf x)))

(do ((x 0 (1+ x)))
    ((> x 9))
  (princ x))

(defmacro for (v a b &rest body)
  `(do ((,v ,a (1+ ,v)))
    ((> ,v ,b))
    ,@body))

(for i 0 10 (princ i)(princ (* 2 i))(terpri))

(dpr vec l r)


(setf a 1 b 2 c 3)

;;;;debugging print
;;;;(dpr a b c) ->
;;;;(format nil "~{~A->~A~^; ~}~%" (list 'a a 'b b 'c c))

(defmacro dpr (&rest params)
`(format t "~{~A->~A~^; ~}~%" 
  (list  ,@(mapcan #'(lambda (x) `(',x ,x)) params))))

(defun quicksort (vec l r)
  (dpr vec l r)
  (let ((i l)
	(j r)
	(p (svref vec (round (+ l r) 2))))
    (dpr p)
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
	(princ i)
	(rotatef (svref vec i) (svref vec j))
	(incf i)
	(decf j)))
    (if (> j l) (quicksort vec l j)) ;note version in book is wrong
    (if (> r i) (quicksort vec i r)))
  vec)

(quicksort #( 9 8 2 5 7 1 6 2 9 ) 0 8)

(defun randomlist (m &optional (n m))
  (if (= m 0) '()
      (cons (random n) (randomlist (- m 1) n))))

(quicksort (coerce (randomlist 20) 'vector) 0 19)


(ntimes 10
	(princ "."))

(do ((i 0 (+ i 1)))
    ((> i 10))
  (princ "."))

(defmacro ntimes (x &body body)
  `(do ((i 0 (+ i 1)))
    ((> i ,x))
    ,@body))

(ntimes 80 (princ i))
(let ((x 0)) (ntimes 80 (princ (incf x))))
(let ((i 0)) (ntimes 80 (princ (incf i))))

(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
      ((>= ,g ,n))
      ,@body)))

;(ntimes 80 (princ i)) doesn't work any more
(let ((x 0)) (ntimes 80 (princ (incf x))))
(let ((x 80)) (ntimes (decf x) (princ x)))

(defmacro ntimes (n &body body)
  (let ((g (gensym))
	(h (gensym)))
    `(let ((,h ,n))
      (do ((,g 0 (+ ,g 1)))
	  ((>= ,g ,h))
	,@body))))

(let ((x 0)) (ntimes 80 (princ (incf x))))
(let ((x 80)) (ntimes (decf x) (princ x)))

(pprint (macroexpand-1 '(cond (a b)
			 (c d e)
			 (t f))))

(defmacro cah (lst) `(car ,lst))

(let ((x (list 'a 'b 'c)))
  (setf (cah x) 44)
  x)





    


