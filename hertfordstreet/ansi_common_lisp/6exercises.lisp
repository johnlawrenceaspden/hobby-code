;;;;1 define a version of tokens that takes :test and :start 
;;;;arguments defaulting to #'constitiuent and 0 respectively.

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str :test test :start p2)
		    nil)))
	nil)))


(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))

(tokens "the cat sat on the mat")
(tokens "the cat sat on the mat" :test #'graphic-char-p)
(tokens "the cat sat on the mat" :test #'constituent :start 5)

;;;;2 Define a version of bin-search that takes :key :test :start and :end arguments witht the usual meanings and defaults

(DEFUN finder (obj vec &key (start 0) (end (- (length vec) 1)) (key #'identity) (test #'eql))
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
	(if (funcall test obj (funcall key (aref vec start)))
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let* ((obj2  (aref vec mid))
		 (kobj2 (funcall key obj2)))
	    (if (< obj kobj2)
		(finder obj vec :start start :end (- mid 1) :key key :test test)
		(if (> obj kobj2)
		    (finder obj vec :start (+ mid 1) :end end :key key :test test)
		    (if (funcall test obj kobj2) obj2 nil))))))))

(finder 7 #(1 2 3 4 5 6 7 8 9))

;(mapcar (lambda (x) (list (* x x) x)) '(1 2 3 4 5 6 7 8 9 10))

(finder 36.0  
	#((1 1) (4 2) (9 3) (16 4) (25 5) (36 6) (49 7) (64 8) (81 9) (100 10))
	:key #'car :start 1 :end 8 :test #'=)


;;;;3 define a function that takes any number of arguments and returns the number of arguments passed to it.

(defun quantum (&rest x)
  (length x))

(quantum 1 2 3 4 5)
(quantum 'a 'b 'c 'd 'e 'f 'g)


;;;;4 modify most to return, as two values, the two highest-scoring elements of a list.

(defmacro dbg (a)
  (let ((b (gensym)))
  `(let ((,b ,a))
    (format t "~A -> ~A~%"  (quote ,a) ,b)
    ,b)))

(dbg (* 2 2))

(defun 2most (fn lst)
  (let* ((s (mapcar (lambda (x) (cons (funcall fn x) x)) lst))
	 (ss (sort s #'> :key #'car)))
    (values (caar ss) (caadr ss))))


(2most (lambda (x) (- (* x x) (/ (* x x x) 5))) '(-2 -1 0 1 2 3 4 5 6))
(2most (lambda (x) (- (* x x) (/ (* x x x) 5))) '(1))



;;;;5 define remove-if (no keywords) in terms of filter

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(filter #'evenp '(1 2 3 4 5 6))

(defun our-remove-if (predicate lst)
  (filter 
   (lambda (x) (if (funcall predicate x) nil x))
   lst))

(our-remove-if #'evenp '(1 2 3 4 5 6))


;;;;6 define a function that takes one argument, a number, and returns 
;;;;the greatest argument passed to it so far.

(let ((max nil))
  (defun maxsofar (x)
    (if (or (null max) (> x max)) 
	(progn
	  (setf max x)
	  max)
	max)))

(mapcar #'maxsofar '(1 2 1 3 5 4 3 -0))

;;;;7 define a function that takes one argument, a number, and returns true 
;;;;if it is greater than the argument passed to the function the last time it was called. 
;;;;The function should return nil the first time it is called.

(let ((last nil))
  (defun greaterthanlast (x)
    (cond ((or (null last) (< x last))  
	   (setf last x) 
	   nil)
	  ((> x last) 
	   (setf last x)
	   t))))

(mapcar #'greaterthanlast '(1 2 1 3 5 4 3 -0))
	   
	  

;;;;8 Suppose expensive is a function of one argument, an integer between 0 and 100 inclusive
;;;;that returns the result of a time-consuming computation. Define a function frugal that
;;;;returns the same answer, but only calls expensive when given an argument it has not seen before

(defun expensive (n)
  (if (< n 2) n
      (+ (expensive (- n 1)) (expensive (- n 2)))))

(expensive 20)

(let ((resultlist (make-hash-table)))
  (defun frugal (n)
    (multiple-value-bind (val precalled) (gethash n resultlist)
      (if precalled
	  val
	  (let ((val (expensive n)))
		 (setf (gethash n resultlist) val)
		 val)))))

(mapcar #'frugal    '(25 25 25 25 25 25))
(mapcar #'expensive '(25 25 25 25 25 25))

;;;;9 define a function like apply, but where any number printed out before it returns
;;;;will be printed, by default, in octal (base 8)

(defun like-apply (fn l)
  (setf *print-base* 8)
  (apply fn l)
  (setf *print-base* 10))

(like-apply  #'sum-and-print '(1 2 3 4 5 6))

(defun sum-and-print (&rest l)
  (if (null l)
      (progn 
	(princ "done")
	0)
      (progn
	(princ (car l))
	(+ (car l) (apply #'sum-and-print (cdr l))))))

(sum-and-print 1 2 3 4 5 6 7 8 9 10)
(apply #'sum-and-print '(1 2 3 4 5 6 7 8 9 10))
(like-apply #'sum-and-print '(1 2 3 4 5 6 7 8 9 10) )

