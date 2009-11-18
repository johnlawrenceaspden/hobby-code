'(defun delay(n)
  (if (fixnump n)
      (locally
	  (declare (fixnum n) (optimize (speed 3) (safety 0)))
	(dotimes (i n))))
  (dotimes (i n)))

	
(mapcar #'sin
	(do* ((x 1 (1+ x))
	      (result (list x) (push x result)))
	     ((= x 10) (nreverse result))))

(defvar a 1)
(defvar b 10)

(do ((x a (+ 1 x)))
    ((> x b))
  (print x))

(defun dbl (x) (* x 2))

(dbl 2)

((lambda (x) (* x 2)) 5)

(mapcar #'(lambda (x) (+ x 10))
	'(1 2 3))

(mapcar #'+
	'(1 2 3)
	'(10 100 1000))

(sort '(1 4 2 5 6 7 3) #'<)

(< 1 2 3 4 3 )

(remove-if #'evenp '(1 2 3 4 5 6 7))

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
	  (our-remove-if fn (cdr lst))
	  (cons (car lst) (our-remove-if fn (cdr lst))))))

'(defun behave (animal)
  (case animal
    (dog (wag-tail)
	 (bark))
    (rat (scurry)
	 (squeak))
    (cat (rub-legs)
	 (scratch-carpet))))

'(defun behave (animal)
  (funcall (get animal 'behavior)))

'(set (get 'dog 'behavior)
     #'(lambda ()
	 (wag-tail)
	 (bark)))

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(defun list+ (lst n)
  (mapcar (lambda (x) (+ x n))
	  lst))

(list+ '(1 2 3) 10)

(scope-test 10)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(defun make-adderb (n)
  (lambda (x &optional doom) 
    (if doom 
	(setq n x) 
	(+ x n))))

(setq addx (make-adderb 1))

(funcall addx 3)

(funcall addx 100 t)

(funcall addx 3)


(defun make-dbms (db)
  (list
   (lambda (key)
     (cdr (assoc key db)))
   (lambda (key val)
     (push (cons key val) db)
     key)
   (lambda (key)
     (setf db (delete key db :key #'car))
     key)))

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'boston)
(funcall (second cities) 'london 'england)
(funcall (car cities) 'london)
(funcall (caddr cities) 'paris)

(defun lookup (key db)
  (funcall (car db) key))

(lookup 'london cities)

(mapcar (lambda (x) (+ 2 x))
	'(2 5 7 3))

(labels ((inc (x) (1+ x))) (inc 3))

(defun count-instances (obj list)
  (labels ((instances-in (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
		    (instances-in (cdr lst)))
		 0)))
    (mapcar #'instances-in list)))

(count-instances 3 '( (1) 2 (3) 3 (4 5 3)))

(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(our-length '( 1 2 (1 2) 3))

(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (our-find-if fn (cdr lst))))

(our-find-if #'evenp '(1 2 3 4))

(defun our-length-tail (lst)
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(our-length '(1 2 3 4))


(defun tri (c n)
  (declare (type fixnum n c))
  (if (zerop n)
      c
      (tri (the fixnum (+ c n)) (the fixnum (- n 1)))))

(tri 0 10)
(tri 10 9)
(tri 19 8)
(tri 27 7)

(proclaim '(optimize (speed 3) (safety 0) (size 3) (debug 0)))
(defun foo (x) (declare (type fixnum x)) (the fixnum (+ 3 x))) 
(disassemble #'foo)

(defun bad-reverse (lst)
  (let* ((len (length lst))
	 (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
	 (j (1- len) (1- j)))
	((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(defvar a '(1 2 3 4))

(bad-reverse a)

(print a)

(defun good-reverse (lst)
  (labels ((good-reverse-helper (lst revlist)
	     (if (null lst) revlist
		 (good-reverse-helper (cdr lst) (cons (car lst) revlist))))))
	   (good-reverse-helper lst '()))

(good-reverse '( 1 2 3 4))


    
(defun powers (x)
           (values x (sqrt x) (expt x 2)))

(powers 4)

(multiple-value-bind  (base root square) (powers 4) 
  (list base root square))

(defun fun (x)
  (list 'a (expt (car x) 2)))

(fun '(2 3))

(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(imp '(2 3))

(let (y (x 1)) (list x y))


(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))

(qualify '(the cat sat on the mat))

(let ((x 0))
  (defun total (y)
    (incf x y)))

(total 3)

(defvar *list* '(a b c))

(defun ok (x)
  (nconc (list 'a x) (list 'c)))

(ok *list*)
*list*

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))

(not-ok *list*)
*list*


(defvar *anything* 0)

(defun anything (x)
  (+ x *anything*))

(anything 2)


(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
      (nreverse acc)))

(remove-if-not #'evenp '(1 2 3 4 5 6))
(filter #'evenp '(1 2 3 4 5 6))
























