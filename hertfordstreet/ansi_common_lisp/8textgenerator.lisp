(load "favourites.lisp")

;;;;functions which identify punctuation characters and transform them to appropriate symbols.
;;;;originally they look like:
;(defun punctuation-p (c)
;  (if (member c '(#\. #\, #\; #\? #\!)) t nil))
;(defun punc (c)
;  (case c
;    (#\. '|.|) (#\, '|,|) (#\; '|;|) (#\! '|!|) (#\? '|?|) ))
;obviously there's a bit of a redundancy, so there are macroized versions

(defconstant *punctuation-list* '(#\. #\, #\; #\? #\! #\: #\@))

(defun punctuation-p (c)
  (if (member c *punctuation-list*) t nil))

(defmacro punctuation-case-statement (c)  
  (let ((l (loop for a in *punctuation-list* collect `(,a (quote ,(intern (make-string 1 :initial-element a)))))))
    `(case ,c ,@l)))

(defmacro punc (c) 
  `(punctuation-case-statement ,c))

;(mapcar (lambda (x) (list (punc x)(punctuation-p x))) '(#\a #\b #\/ #\; #\. #\, #\; #\? #\@ #\:))


(defun on-each-word-of-stream (str fn &key 
			       (max-word-length 100) 
			       (word-char-p   #'(lambda(c) (or (alpha-char-p c)(eq c #\'))))
			       (punctuation-p #'punctuation-p)
			       (punctuation-fn fn))
"Calls fn on each word in the stream, and punctuation-fn on each punctuation character
word-char-p and punctuation-p are used to define what's what"
  (let ((buffer (make-string max-word-length))
	(pos 0))
    (do ((c (read-char str nil :eof)
	    (read-char str nil :eof)))
	((eql c :eof))
      (if (funcall word-char-p c)
	  (progn
	    (setf (aref buffer pos) c)
	    (incf pos))
	  (progn
	    (unless (zerop pos)
	      (funcall fn (subseq buffer 0 pos))
	      (setf pos 0))
	    (if (funcall punctuation-p c)
		(funcall punctuation-fn c)))))))

(defun make-hashtable-from-file (filename)
    (with-open-file (file filename :direction :input) 
      (make-hashtable-from-stream file)))


(defun print-hash-table (h)
  (do-hash (k v h) (format t "~A->~A~%" k v)))


(defun make-hashtable-from-stream (s &optional (hash-size 10000))
  (reset)
  (let ((hashtable (make-hash-table :test #'equal :size hash-size)))
    (on-each-word-of-stream s 
			    #'(lambda(x) (see  (intern (string-downcase x)) hashtable))
			    :punctuation-fn #'(lambda(x) (see (punc x) hashtable)))
			    hashtable))

(let ((prev nil)
      (preprev nil))
  (defun see (symb hashtable)
    (let* ((predec (list preprev prev))
	   (pair (assoc symb (gethash predec hashtable))))
      (if (null pair)
	  (push (cons symb 1) (gethash predec hashtable))
	  (incf (cdr pair))))
    (setf preprev prev)
    (setf prev symb))
  (defun reset ()
    (setf prev '|.|
	  preprev '|.|)))

(defun generate-text (hashtable n &optional (preprev '|.|) (prev '|.|))
  (if (zerop n)
      '()
      (let ((next (random-next hashtable preprev prev)))
	(cons next (generate-text hashtable (1- n) prev next)))))

(defun random-next (hashtable preprev prev)
  (let* ((choices (gethash (list preprev prev) hashtable))
	 (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
	  (return (car pair))))))

;;;;the exercise is to tell whether a henley file could have been generated from a given source
(defun write-random-text (filename hashtable &optional (wordcount 100))
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (let ((count 0)
	  (text (generate-text hashtable wordcount))) 
    (dolist (w text)
      (format file "~A " w)
      (incf count)
      (when (> count 10)
	(setf count 0)
	(terpri file)))
    (format nil "~{~A ~}" text))))

(defun sub-hash-tables (a b) ;a and b are hash-tables: is every association in a also in b?
  (block the-horror
    (do-hash (key avals a)
      (let ((bvals (gethash key b)))
	(format t "~A leads on to  ~A or ~A {" key avals bvals)
	(when (null bvals) (return-from the-horror 'keypair-not-present))
	(format t "~A}~%" (subalist avals bvals)) 
	(when (null (subalist avals bvals)) (return-from the-horror 'not-a-sublist))
	))'seem-to-have-made-it) )


(defun subalist (a b) ;a and b are assoc-lists. is every element of a present in b (counts not relevant)
   (reduce (lambda (a b) (and a b)) 
	   (mapcar (lambda (x) (not (not (member (car x) b :key #'car)))) a)))

(defun generate-henley (infilename outfilename)
  (let (table)
    (setf table (make-hashtable-from-file infilename))
    (write-random-text outfilename table)))

(defun generate-all-henleys()
  (generate-henley "milton.txt" "milton.henley")
  (generate-henley "gor.txt"    "gor.henley")
  (generate-henley "paradiselost.txt" "paradiselost.henley"))

(defvar *miltontxt*           (make-hashtable-from-file "milton.txt"))
(defvar *gortxt*              (make-hashtable-from-file "gor.txt"))
(defvar *paradiselosttxt*     (make-hashtable-from-file "paradiselost.txt"))
(defvar *paradiselost.henley* (make-hashtable-from-file "paradiselost.henley"))
(defvar *miltonhenley*        (make-hashtable-from-file "milton.henley"))
(defvar *gorhenley*           (make-hashtable-from-file "gor.henley"))



(sub-hash-tables *paradiselosttxt* *miltontxt*)





