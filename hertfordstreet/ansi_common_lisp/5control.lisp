(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never get here."))

(dolist (x '(a b c d e))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))


(defun my-read-integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
	(if i
	    (setf accum (+ (* accum 10) i))
	    (return-from my-read-integer nil))))
    accum))

(my-read-integer "102034")


(tagbody
   (setf x 0)
 top
   (princ x)
   (setf x (+ x 1))
   (if (= x 10) (go end))
   (go top)
 end)

(dotimes (x 10) (princ x))
(DO ((X 0 (1+ X))) ((>= X 10) NIL) (PRINC X))
(BLOCK NIL
  (LET ((X 0))
    (TAGBODY 
     top 
       (IF (>= X 10) (GO end)) 
       (PRINC X) 
       (PSETQ X (1+ X))
       (GO top)
     end 
       (RETURN-FROM NIL (PROGN NIL)))))


(let ((x 7)
      (y 2))
  (format t "Number ~A" (+ x y)))


((lambda (x) (+ x 1)) 3)

(let ((x 3))
  (+ x 1))

(let ((x 2)
      (y (+ x 1)))
  (+ x y))

(let* ((x 2)
       (y (+ x 1)))
  (+ x y))

(let ((x 2))
  (let ((y (+ x 1)))
    (+ x y)))

(let (x (y 1))
  (list x y))

(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))

(let ((that 3))
  (when (oddp that)
    (format t "Hmmm, that's odd.")
    (+ that 1)))

(let ((that 3))
  (if (oddp that)
      (progn 
	(format t "Hmm, that's odd.")
	(+ that 1))))

(defun our-member (obj lst)
  (if (atom lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun our-member (obj lst)
  (cond ((atom lst) nil)
	((eql (car lst) obj) lst)
	(t (our-member obj (cdr lst)))))

(our-member 'a '(b c d e))


(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb 28)
    (otherwise "unknown month")))

(month-length 'fbe)

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 1 10)

(let ((x 'a))
  (do ((x 1 (+ 1 x)) ; experiment with do and do*
       (y x x))
      ((> x 5) 'done)
    (format t "(~A ~A)   " x y)))

(dolist (x '( a b c d) 'done)
  (princ x))

(dotimes (x 5 'done)
  (princ x))


(defun factorial (n)
  (do* ((x 1 (+ 1 x))
       (acc 1 (* x acc)))
      ((>= x n) acc)))

(equal (mapcar #'factorial '(0 1 2 3 4 5 6 7)) '(1 1 2 6 24 120 720 5040))

(mapc #'(lambda (x y)
	  (format t "~A ~A " x y))
      '(hip flip slip)
      '(hop flop slop))

(defun ick () (values 'a nil (+ 2 4)))

(multiple-value-bind (x y z) (ick)
  (list x y z))

(multiple-value-list (get-decoded-time))

(defun super ()
  (catch 'abort
    (sub))
  (sub2)
  (format t "We'll never see this."))

(defun sub ()
  (throw 'abort "Error in deliberately cause error function"))

(defun sub2 ()
  (unwind-protect
       (error "you can't call this!")
    (format t "but even if you do this will happen")))
    
(super)

(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))

(apply #'+ mon)

(setf nom (reverse mon))

(setf sums (maplist #'(lambda (x)
			(apply #'+ x))
		    nom))

(reverse sums)

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)


(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(month-num 7 1900)

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
	(dotimes (i (- y yzero) d)
	  (incf d (year-days (+ yzero i))))
	(dotimes (i (- yzero y) (- d))
	  (incf d (year-days (+ yzero i)))))))

(defun year-days (y) (if (leap? y) 366 365))

(year-num 2012)

(-  (date->num 18 7 2007)(date->num 11 6 1970))

(mapcar #'leap? '(1904 1900 1600))

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
	    (d (- (year-days y)) (- d (year-days y))))
	   ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
	    (prev 0 d)
	    (d (year-days y) (+ d (year-days y))))
	   ((> d n) (values y (- n prev))))))

(year-days 1999)
(num-year -500)

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
	    ((> n 59) (nmon (- n 1)))
	    (t        (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))


(num-month 60 2001)

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))

(date+ 3 1 2001 -30)


;;;;;;summary

(progn
  (format t "~A" 'A)
  (format t "~A" 'B)
  (+ 2 3))

(block doom
  (format t "~A" 'A)
  (return-from doom 3)
  (format t "~A" 'B))

(tagbody
   (setf c 0)
 start
   (format t "~A" 'A)
   (setf c (+ c 1))
   (when (< c 5)(go start))
   (format t "~A" 'B)
   (unless (< c 10) (go end))
   (format t "~A" 'C)
   (go start)
 end)

(dolist (x '(a b c d e))
  (princ x))

(dotimes (x 10 'done)
  (princ x))

(do ((x 0 (+ x 1)))
    ((>= x 10) 'done)
  (princ x))

(defun muls () (values 1 2 3))

(multiple-value-bind (a b c) (muls)
  (mapcar #'princ (list a b c)))

(mapcar #'princ (multiple-value-list (muls)))

(dolist (x (list #'+ #'*))
  (princ (multiple-value-call x (muls))))

(defun throwsummat ()
  (princ "enter throwsummat")
  (throw 'summat 69)
  (princ "leave throwsummat"))

(catch 'summat
  (throwsummat)
  (princ "oh aye")
  (throwsummat))

(defun safecall ()
  (unwind-protect
       (throwsummat)
    (princ 'doom)))

(progn
  (catch 'summat 
    (safecall)
    (princ "end of catch block"))
  (princ "program continues"))


;;;;exercises

(setf y '(a b))

(let (( x (car y)))
  (cons x x))

((lambda (x) (cons x x)) (car y))

(setf x '(10 20) z 40)

(let* ((w (car x))
      (y (+ w z)))
  (cons w y))

((lambda (a) (cons a (+ a z))) (car x))

(let ((w (car x)))
  (let ((y (+ w z)))
    (cons w y)))

((lambda (w)
   ((lambda (y) (cons w y)) (+ w z)))
  (car x))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

(mystery 'a '( b c d e ))

(defun newmystery (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (newmystery x (cdr y))))
	    (and z (+ z 1))))))

(newmystery 'a '( a b c d e a))


(defun crockedmystery (x y)
  (cond ((null y) 'noway)
	((eql (car y) x) 0)
	(t (let ((z (newmystery x (cdr y))))
	    (and z (+ z 1))))))

(crockedmystery 'a '())

(let ((testlist '((a b c)
		  (b c d)
		  (b c d a a)) ))
  (equal
   (mapcar (lambda (l) (mystery 'a l))    testlist)
   (mapcar (lambda (l) (newmystery 'a l)) testlist)
 ))

(setf testlist '((a b c)
		  (b c d)
		  (b c d a a)
		 ())
      funlist (list #'mystery #'newmystery #'crockedmystery))
  
(equallist? (mapcar 
 (lambda (f) (mapcar (lambda (l) (funcall f 'a l)) testlist))
 funlist))

(defun equallist? (l)
  (if (null (cdr l)) t
      (and (equal (car l) (cadr l))
	   (equallist? (cdr l)))))


(defun bigsquareonly (x)
  (if (<= x 5)
      'bad
      (* x x)))

(mapcar #'bigsquareonly '(0 1 5 6 7))

(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun newmonth-num (m y)
  (+
   (case m 
     (1 0)(2 31)(3 59)(4 90)(5 120)(6 151)
     (7 181)(8 212)(9 243)(10 273)(11 304)(12 334))
   (if (and (> m 2) (leap? y)) 1 0)))

(let ((months '(3 4 5 11))
      (years  '(2001 2000 1999 2000)))
  (equallist? 
   (list (mapcar #'month-num months years)
	 (mapcar #'newmonth-num months years))))

(defmacro dbg (a)
  (let ((b (gensym)))
  `(let ((,b ,a))
      (format t "~A -> ~A~%" (quote ,a) ,b)
      ,b)))

(dbg (+ 1 2))

(defun cons-if-not-member (a l)
  (if (member a l)
      l
      (cons a l)))

(defun precedes-do (a vec)
  (do ((i 1 (+ i 1))
       (acc nil))
      ((>= i (length vec)) acc)
    (let ((prev (elt vec (- i 1)))
	  (cur  (elt vec i)))
      (if (equal cur a) (setf acc (cons-if-not-member prev acc))))))

(defun precedes-rec (a vec)
  (if (< (length vec) 2) nil
      (let ((p    (precedes-rec a (subseq vec 1)))
	    (prev (elt vec 0))
	    (cur  (elt vec 1)))
	(if (eql cur a) 
	    (cons-if-not-member prev p) 
	    p))))

(defun precedes-dotimes (a vec)
  (setf acc nil)
  (dotimes (i ( - (length vec) 1) acc)
    (if (eql (elt vec (+ i 1)) a)
	     (setf acc (cons-if-not-member (elt vec i) acc)))))

(precedes #\a "abracadabra")

 (trace precedes)

 (setf testlist '((#\a "abracadabra")(#\a (a b c))))
 (setf funlist (list #'precedes #'precedes-rec #'precedes-dotimes))


 (defun equality-as-lists (a b test)
   (cond ((and (null a) (null b)) t)
	 ((null a) nil)
	 ((null b) nil)
	 (t (and (funcall test (car a) (car b))
		 (equality-as-lists (cdr a) (cdr b) test)))))

 (equality-as-lists '(a b (1 2)) '(a b (1 2)) #'equal)
 (equality-as-lists '(a b (1 2)) '(a b (2 1)) #'equal)
 (equality-as-lists '(a b (1 2)) '(a b (1 2)) #'eql)

 (defun equality-as-lists-of-sets (a b)
   (equality-as-lists a b
		      (lambda (x y) (not (set-exclusive-or x y)))))

(equality-as-lists-of-sets '((#\d #\c #\r) NIL (a)) 
			    '((#\c #\d #\r) NIL (a)))

(equality-as-lists-of-sets '((#\d #\c #\r) NIL) 
			   '(NIL (#\c #\d #\r)))

(equallist? (mapcar 
	     (lambda (f) (mapcar (lambda (l) (apply f l)) testlist))
	     funlist)
	    #'equality-as-lists-of-sets)

(defun equallist? (l test)
  (if (null (cdr l)) t
      (and (funcall test (car l) (cadr l))
	   (equallist? (cdr l) test))))

(equallist? 
 '((b c a) (a c b) (a c b))
 #'(lambda (a b) (not (set-exclusive-or a b))))


(defun intersperse (a l)
  (cond ((null l) nil)
	((null (cdr l)) l)
	(t (cons (car l) (cons a (intersperse a (cdr l)))))))

(defun testfn (f list)
  (setf no.passed 0)
  (dolist (x list no.passed)
    (let* ((args (car x))
	   (vals (cdr x))
	   (results (apply f args)))
      (if (equal results vals)
	  (incf no.passed)
	  (progn
	    (format t 
		    "failure!!~% input ~A -> ~A != [~A]~%" 
		    args results vals)
	    (return nil))))))


(intersperse '- '(a b c d))
(intersperse '- '())
(intersperse '- '(a))

(equallist?
 (list
  (mapcar 
   (lambda (l) (intersperse '- l))
   '((a b c d) () (a)))
  '((A - B - C - D) NIL (A)))
 #'equal)

(defun testfn (f list)
  (setf no.passed 0)
  (dolist (x list no.passed)
    (let ((args (car x))
	  (vals (cdr x)))
      (let ((results (apply f args)))
	;(format t "~A -> ~A [~A]~%" args results vals)
	(if (equal results vals)
	    (incf no.passed)
	    (progn
	      (format t "failure!!~% input ~A -> ~A != [~A]~%" args results vals)
	      (return nil)))))))

(testfn 
 #'intersperse
 (list
  '((- (a b c d)) . (a - b - c - d))
  '((- ())        . ())
  '((- (a))       . (a))
  '((a (a b c))   . (a a b a c))
))

(defun intersperse (a l)
  (if (< (length l) 2) l
      (let ((acc (list (car l))))
	(dolist (x (cdr l))
	  (setf acc (append acc (list a x))))
	acc)))

(defun monotone-1 (l)
  (if (or (null l) (null (cdr l))) t
      (and 
       (equal (+ (car l) 1) (cadr l))
       (monotone-1 (cdr l)))))

(defun monotone-do (l)
  (dotimes (i (- (length l) 1) t)
    (if (not (equal (+ 1 (elt l i)) (elt l (+ i 1)))) (return '()))))

(defun monotone-mapc (l)
  (mapc (lambda (a b) 
	    (if (not (equal (- b a) 1)) (return-from monotone-mapc NIL)))
	  l (cdr l))
  t)

(monotone-mapc '(1 2 1 2))

(let ((flist   (list #'monotone-do #'monotone-1 #'monotone-mapc))
      (testset (list
		'((  (4 5 6 7 8) ) . t)
		'((  (1 2 3 4)   ) . t)
		'((  ()          ) . t)
		'((  (1)         ) . t)
		'((  (1 3 4)     ) . NIL)
		'((  (1 2 1 2)   ) . NIL)
		'((  (1 2 3 5 7) ) . NIL))))
  (mapcar (lambda (f) (testfn f testset)) flist))


(defun minmax (v)
  (if (null (cdr v)) (values (car v) (car v))
      (multiple-value-bind (mn mx) (minmax (cdr v))
	(values
	 (min (car v) mn)
	 (max (car v) mx)))))

(minmax '(1 2 2 0 -7 3 4))
	 
(testfn 
 #'(lambda (l) (multiple-value-list (minmax l)))
 (list
  '(( (0)              ) . (0 0)        )
  '(( (1 0)            ) . (0 1)        )
  '(( ()               ) . (NIL NIL)    )
  '(( (1 2 2 0 -7 3 4) ) . (-7 4)       )))





















































