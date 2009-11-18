(defun list-duplicates (a)
  (let* ((b (remove-duplicates a))
	 (c (set-difference b a)))
    (remove-duplicates c)))


(defvar *testlist* '(a a b b b d d e f g h i i))

(remove-duplicates *testlist* :test #'string=)
(set-difference (remove-duplicates *testlist*) *testlist*)

(defvar *h* (make-hash-table))
(dolist (i *testlist*)
  (let ((a (gethash i *h*)))
    (if a (incf (gethash i *h*))
	(setf (gethash i *h*) 1))))

(let ((acc nil))
  (defun addtolistiflargerthanone(k v)
    (if (> v 1)
	(setf acc (cons k acc))))
  (defun getacc() acc))

(maphash #'addtolistiflargerthanone *h*)
(getacc)

(let (acc hashtable)
  (flet ((addiflargerthanone (k v)
	   (if (> v 1) (setf acc (cons k acc)))))
    (defun getduplicates (lst &key (test #'eql))
      (setf acc nil hashtable (make-hash-table :test test))
      (dolist (i lst)
	(let ((a (gethash i hashtable)))
	  (if a (incf (gethash i hashtable))
	      (setf (gethash i hashtable) 1))))
      (maphash #'addiflargerthanone hashtable)
      (reverse acc))))

(getduplicates '(a a b b "doom" c d 1 1 "doom" (1 2)(1 2)) :test #'equal)

(defun hashinc (i hash)
    (if (gethash i hash)
	(incf (gethash i hash))
	(setf (gethash i hash) 1)))

(defun getduplicates (lst &key (test #'eql))
  (let ((acc nil)
	(hash (make-hash-table :test test)))
    (dolist (i lst)
      (hashinc i hash))
    (maphash 
     #'(lambda (k v) (if (> v 1) (setf acc (cons k acc))))
     hash)
    (reverse acc)))   

(defmacro dohash ((k v hash) &body body)
  `(maphash (lambda (,k ,v) ,@body) ,hash))

(defun hash-string(h)
  (with-output-to-string (s)
    (dohash (k v h)
      (format s "~A,~A; " k v))))



;;;Hash tables become tedious.
;;;I wish to write:  *a*{"i"}=1 and *a*{"i"}+=1
;;;but will settle for: (hsetf *a* "i" 1)
;;;even sweeter would be
#|(hsetf *a*
	 "i" 10
	 "j" 20)

(setf (gethash "i" *a*) 10
      (gethash "j" *a*) 20)|#


(defmacro hsetf (hash &rest list)
  (labels ((helper (hash &rest list)
		  (if (null (cadr list)) '()
		      (append (list (list 'gethash (car list) hash) 
				    (cadr list))
			      (apply #'helper (cons hash (cddr list)))))))
    `(setf ,@(apply #'helper (cons hash list)))))

;;;now I can say

(defvar *b* (make-hash-table :test #'equal))
(hsetf *b* "1" 1 "2" 2 "2" 2 "3" 3)
(defvar *a* (make-hash-table))
(hsetf *a* 1 2 3 4 5 6)

(hash-string *a*)
(hash-string *b*)
;;;where were we?

(with-output-to-string(s)
  (dohash (k v *a*)
    (dohash (a b *b*)
      (format s "(~A,~A)->~A,~A;" k a v b))))



