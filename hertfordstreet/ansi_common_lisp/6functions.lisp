(defun foo(x)
  "Implements an enhanced paradigm of diversity"
  x)

(documentation 'foo 'function)

(labels (( pyth         (x y) (sqrt (+ (* x x) (* y y))) )
	 ( sort-of-pyth (x y) (sqrt (+ (* 2 y x)))     ))
  (pyth (pyth 3 4) (sort-of-pyth 3 4)))

;labels can recurse, unlike let

(labels ((len (lst)
	   (if (null lst)
	       0
	       (+ (len (cdr lst)) 1))))
  (len '(a b c)))

(do (( x a (b x))
     ( y c (d y)))
    ((test x y) (z x y))
  (f x y))

;macroexpands to

(BLOCK NIL
  (LET ((X A) 
	(Y C))
    (TAGBODY 
     begin 
       (IF (TEST X Y) (GO end)) 
       (F X Y)
       (PSETQ X (B X) Y (D Y)) 
       (GO begin) 
     end
       (RETURN-FROM NIL (PROGN (Z X Y))))))

;or is also equivalent to

(labels ((rec (x y)
	   (cond ((test x y)
		  (z x y))
		 (t
		  (f x y)
		  (rec (b x) (d y))))))
  (rec a c))


;;;;parameter lists

(defun our-funcall (fn &rest args)
  (apply fn args))

(defun philosoph (thing &optional property)
  (list thing 'is property))

(philosoph 'death)
(philosoph 'terrorism 'glorious) ;;fuck off government

(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))

(defun keylist (a &key x y z)
  (list a x y z))

(keylist 1 :y 2)
(keylist 1 :y 3 :x 2)

(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(our-adjoin '(a b) '((a b) c d))
(our-adjoin '(a b) '((a b) c d) :test #'equal)

(destructuring-bind ((&key w x) &rest y) '((:w 3) a b c)
  (list w x y))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(mapcar #'single? '( () (a) (a b) ))

(defun append1 (lst obj)
  (append lst (list obj)))

(append1 '(a b c) 'd)

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(mapcar (lambda (f) (map-int f 10)) (list #'sqrt 'identity))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(filter #'evenp '(1 2 3 4 5 6))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins obj
		    max score))))
	(values wins max))))

(defun poly(x) (* x (+ 2 (* x (+ 1 x )))))

(most #'poly '(-1 2 3 4 -5 6 -9))

(map-int #'identity 10)

(map-int #'(lambda (x) (random 100)) 10)

(remove-if-not #'evenp '(1 2 3 4 5 6))

(most #'length '((a b) (a b c) (a)))

(defun combiner (x)
  (typecase x
    (number #'+)
    (list   #'append)
    (t      #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
	 args))

(combine 2 3)
(combine '(a b) '(c d))
(combine 'a 'b '(c d))

(setf fn (let ((i 3)) #'(lambda (x) (+ x i))))

(funcall fn 3)

(defun add-to-list (num lst)
  (mapcar #'(lambda (x) (+ x num)) lst))

(add-to-list 3 '(1 2 3 4 5))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setf add3 (make-adder 3))

(funcall add3 5)
(funcall add3 8)


(let ((counter 0))
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (+ counter 1))))

(stamp)
(reset)

(list (stamp) (stamp) (reset) (stamp))

(mapcar (complement #'oddp) '(1 2 3 4 5 6))

(defun our-complement (f)
  #'(lambda (&rest args)
      (not (apply f args))))

(setf (symbol-function 'our-oddp) (complement #'evenp))
(mapcar #'our-oddp '(1 2 3))



(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
  #'(lambda (&rest args)
      (reduce #'(lambda (v f) (funcall f v))
	      rest
	      :initial-value (apply fn1 args)))))

(mapcar (compose #'list #'round #'sqrt) '(5 10 15 20))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))


(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
	#'(lambda (&rest args)
	    (and (apply fn args) (apply conj args))))))
		

(map-int (conjoin #'(lambda (x) (evenp (floor (/ x 2)))) #'oddp) 20)
(mapcar (disjoin #'integerp #'symbolp) '(a (quote abr) 2 3))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(funcall (curry #'+ 2 3) 5)

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x) #'(lambda (&rest args) x))

(setf (symbol-function 'ever2) (always 2))

(ever2 '(foo bar baz arfle barfle gloop))

(compose #'cdr #'cdr) 
(compose #'car #'nthcdr)
(compose #'not #'consp) 
(setf (symbol-function 'atom?) (rcurry #'typep 'atom))

(defun atom? (x)
  (typep x 'atom))

(funcall (rcurry #'typep 'atom) 'a)

(typep '() 'atom)

(atom? '())

(funcall (disjoin #'< #'=) 2 5)
(funcall (disjoin #'null #'consp) '())
(funcall (rcurry #'typep 'list) '())

(curry #'+ 1)
(rcurry #'+ 1)
(rcurry #'- 1)

(funcall (compose (curry #'apply #'nconc) #'mapcar) 
	 (lambda (x) (list x x)) 
	 '(1 2 3))

(let ((x 10))
  (defun foo ()
    x))

(let ((x 20))
  (declare (special x))
  (foo))

(let ((x 10))
  (defun foo()
    (declare (special x))
    x))

(foo)

(setf x 30)

*print-base*

(let ((*print-base* 16))
  (princ 32))


(defun foo (x) (+ x 1))

(compiled-function-p #'foo)

(compile 'foo)


(compiled-function-p (make-adder 2))

(compile 'make-adder)

(defun fib (n)
  (cond ((= n 1) 1)
	((= n 0) 0)
	((= n 5) nil)
	(t (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)

(map-int #'fib 20)


