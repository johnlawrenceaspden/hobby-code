;;;; Utilities for operations on sorted vectors

;;; Finds an element in a sorted vector
(DEFUN bin-search (obj vec)
  (let ((len (length vec)))
    ;; if it's a real vector, send it to finder
    (and (not (zerop len)) ; return nil if empty
	 (finder obj vec 0 (- len 1)))))

#|This extensive comment
is just for fun|#

(DEFUN finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start))
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (aref vec mid)))
	    (if (< obj obj2)
		(finder obj vec start (- mid 1))
		(if (> obj obj2)
		    (finder obj vec (+ mid 1) end)
		    obj)))))))



(defun thou (f)
    (dotimes (i 11)
	(princ (funcall f))))

(defun mirror? (s)
  (let ((len (length s)))
    (do ((forward 0 (+ forward 1))
	 (back (- len 1) (- back 1)))
	((or (> forward back)
	     (not (eql (elt s forward)
		       (elt s back))))
	 (> forward back)))))


(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
	  (parse-month   (second toks))
	  (parse-integer (third toks)))))

(defparameter month-names
  #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names :test #'string-equal)))
    (if p (+ p 1)
	nil)))

;(parse-date "12 Feb 1989")

(defun str-integer (str acc)
  (if (equal str "")
      acc
      (let ((c (digit-char-p (elt str 0))))
	(str-integer (subseq str 1) ( + c (* acc 10))))))


(defun str-integer2 (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
	(dotimes (pos (length str))
	  (setf accum (+ (* accum 10) (digit-char-p (elt str pos)))))
	accum)))

;(str-integer2 "123456")

;(defun block-height (b) (svref b 0))

;(block-height #(1 2 3))

;(defstruct point x y)

;(setf a (make-point :x 0 :y 0))
;(make-point)
;(make-point :x 1 :y 2)

;(setf (point-x a) 3)
;(point-p a)
;(format t "~A" a)
;(setf b (copy-point a))
;b

;(typep b 'point)
;(atom b)

(defstruct polemic
  (type (progn
	  (format t "What kind of polemic was it? ")
	  (read)))
  (effect nil))

;(setf p (make-polemic))

(defstruct (point (:conc-name p)
		  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))

;(setf a (make-point))
;a
;(format t "~A" a)
;(px a)
;(py a)

;(make-point)

;;;; 4.7 Example: Binary Search Trees

(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elt n)))))
  elt 
  (l nil) 
  (r nil))

(defparameter doomtree 
      (make-node :elt 'doom 
		 :l (make-node :elt 'horror) 
		 :r (make-node :elt 'treason)))

(node-l doomtree)
(node-r doomtree)

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-insert obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :l (node-l bst)
		 :r (bst-insert obj (node-r bst) <)))))))

(defparameter tree (make-node :elt 10))
;(setf tree (bst-insert 2 tree #'<))

(defun add-to-tree (n)
  (setf tree (bst-insert n tree #'<)))

;(mapcar #'add-to-tree '(2 10 4 5 6 7 8 13 15 18))

(defun tree-list (bst)
  (if (null bst) nil
      (list (node-elt bst) 
	    (list (tree-list (node-l bst)) 
		  (tree-list (node-r bst))))))

(defun stripped-tree-list (bst)
  (if (null bst) nil
      (let ((l (node-l bst))
	    (r (node-r bst))
	    (e (node-elt bst)))
	(cond ((and (null l) (null r)) (list e))
	      ((null l) (list e (stripped-tree-list r)))
	      ((null r) (list e (stripped-tree-list l)))
	      (t (list e (stripped-tree-list r) (stripped-tree-list l)))))))

;(stripped-tree-list tree)
;(tree-list tree)
;tree
;(node-l tree)
;(node-r tree)

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(bst-find obj (node-l bst) <)
		(bst-find obj (node-r bst) <))))))

;(bst-find 3 tree #'<)

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

;(bst-min tree)

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

;(bst-max tree)

#|
(setf nums nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))

(tree-list nums)

(bst-find 4 nums #'<)
(bst-min nums)
(bst-max nums)
|#

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    (percolate bst)
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-remove obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :r (bst-remove obj (node-r bst) <)
		 :l (node-l bst)))))))

(defun percolate (bst)
  (cond ((null (node-l bst))
	 (if (null (node-r bst))
	     nil
	     (rperc bst)))
	((null (node-r bst)) (lperc bst))
	(t (if (zerop (random 2))
	       (lperc bst)
	       (rperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
	     :l (node-l bst)
	     :r (percolate (node-r bst))))

(defun lperc (bst)
  (make-node :elt (node-elt (node-l bst))
	     :l (percolate (node-l bst))
	     :r (node-r bst)))
		

;(setf nums (bst-remove 5 nums #'<))
;(tree-list nums)

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

;(bst-traverse #'princ nums)

;;;; 4.8 hash tables

(setf bugs (make-hash-table))

(push "Doesn't take keyword arguments." (gethash #'tree-list bugs))
(push "Doesn't work." (gethash #'tree-list bugs))
(push "Offends all principles of good design." (gethash #'tree-list bugs))

(defparameter fruit (make-hash-table))

fruit
(setf (gethash 'apricot fruit) t)
(gethash 'apricot fruit)

(remhash 'apricot fruit)

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)

(setf acc nil)
(maphash #'(lambda (k v) 
	     (setf acc (cons (format nil "~A = ~A" k v) acc)))
	 ht)


(setf writers (make-hash-table :size 5 :test #'equal))
(setf (gethash '(ralph waldo emerson) writers) t)
(gethash '(ralph waldo emerson) writers)

