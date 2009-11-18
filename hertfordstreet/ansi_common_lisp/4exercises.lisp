;;;;exercises for chapter 4 of ansi common lisp

;;;matrix quarter turn clockwise
(defun quarter-turn (a)
  (let ((dims (array-dimensions a)))
    (let ((xd (car dims))
	  (yd (cadr dims))
	  (na (make-array (reverse dims))))
      (dotimes (x yd (+ 1 x))
	(dotimes (y xd (+ 1 y))
	  (let ((ox (- (- xd 1) y))
		(oy x))
	    (setf (aref na x y) (aref a ox oy)))))
      na)))

(setf test-array #2A((1 2 3)( 4 5 6)(7 8 9)(10 11 12)))

(quarter-turn (quarter-turn (quarter-turn (quarter-turn test-array))))

(defun compose (a b)
  #'(lambda (x) (funcall a (funcall b x))))

(setf (symbol-function 'half-turn) (compose #'quarter-turn #'quarter-turn))
(defun three-quarter-turn (x) (half-turn (quarter-turn x)))
(defun identity (x) (half-turn (half-turn x)))

(let ((acc nil))
  (dolist (x '(identity quarter-turn half-turn three-quarter-turn))
    (setf acc (append acc (list (funcall x test-array)))))
  acc)

(funcall (compose #'quarter-turn #'quarter-turn) test-array)


(defun my-copy-list (x)
  (reduce #'cons x :from-end t :initial-value nil))

(defun my-reverse (x)
  (reduce (lambda (x y) (cons y x)) x :initial-value nil))

(my-copy-list '(1 2 3 4))
(my-reverse '(1 2 3 4))

(defstruct trinode
  elt
  c
  r
  l)

(setf x (make-trinode 
	 :elt 'a 
	 :r (make-trinode :elt 'b) 
	 :l (make-trinode :elt 'c) 
	 :r (make-trinode :elt 'd)))

(defun copytritree (x)
  (if (null x) nil
      (make-trinode
       :elt (trinode-elt x)
       :r   (copytritree (trinode-r   x))       
       :l   (copytritree (trinode-l   x))       
       :c   (copytritree (trinode-c   x)))))

(copytritree x)

(defun addtotritree(tree x)
  (let ((rand (random 3)))
    (let ((node (cond ((= rand 0) (trinode-r tree))
		      ((= rand 1) (trinode-c tree))
		      (t (trinode-l tree)))))
      (if (null node)
	  (let ((newnode (make-trinode :elt x)))
	    (cond ((= rand 0) (setf (trinode-r tree) newnode))
		  ((= rand 1) (setf (trinode-c tree) newnode))
		  (t (setf (trinode-l tree) newnode))))
	  (addtotritree node x)))))
	  

(progn
  (setf tree (make-trinode :elt 1))
  (mapcar (lambda (x) (addtotritree tree x)) (list 1 2 3 4 5)))

(defun search-tree (tree elt)
  (if (null tree) nil
      (if (eql (trinode-elt tree) elt) t
	  (or
	   (search-tree (trinode-l tree) elt)
	   (search-tree (trinode-r tree) elt)
	   (search-tree (trinode-c tree) elt)))))


(search-tree tree 'e)

(addtotritree tree 'e)

(defun map-tree (tree f)
  (if (null tree) nil
      (make-trinode :elt (funcall f (trinode-elt tree))
		    :r (map-tree (trinode-r tree) f)		  
		    :c (map-tree (trinode-c tree) f)		    
		    :l (map-tree (trinode-l tree) f))))

(progn
  (setf acc nil)
  (map-tree tree (lambda (x) (setf acc (cons x acc))))
  (sort acc #'<))

(setf ht (make-hash-table))
(mapcar (lambda (x y) (setf (gethash x ht) y)) '(1 2 3 4) '(a b c d))

(defun hash2alist (ht)
  (setf acc nil)
  (maphash (lambda (k v) (setf acc (cons (cons k v) acc))) ht)
  acc)
  
(hash2alist ht)

(defun alist2hash (al)
  (setf ht (make-hash-table))
  (mapcar (lambda (x) (setf (gethash (car x) ht) (cdr x))) al)
  ht)

(alist2hash '((1 . a) (2 . b) (3 . c) (4 . d)))





