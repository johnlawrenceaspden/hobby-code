;;;;1 & 2 Define a function that takes a filename 
;;;;and returns a list of strings representing each line in the file.
;;;;and returns a list of the expressions found in the file.

(defun readlist (filename reader)
  (let ((path (make-pathname :name filename)))
    (with-open-file (file path :direction :input)
      (do ((line 
	    (funcall reader file nil 'eof) 
	    (funcall reader file nil 'eof))
	   (acc nil (cons line acc)))
	  ((eql line 'eof) (nreverse acc))))))

(format t "~{~A~%~}" (readlist "5control.py" #'read-line))
(eval (caddr (readlist "5control.lisp" #'read)))

;;;;3 Suppose that in some format for text files, 
;;;; comments are indicated by a % character. 
;;;; Everything from this character to the end of the line is ignored. 
;;;; Define a function that takes two filenames, 
;;;; and writes to the second file a copy of the first, minus comments.

(defun file-stripcomment (infilename outfilename)
  (let ((inpath (make-pathname :name infilename))
	(outpath (make-pathname :name outfilename)))
    (with-open-file (in inpath :direction :input)
      (with-open-file (out outpath :direction :output
			   :if-exists :supersede)
	(stream-stripcomment in out)))))

(defun stream-stripcomment (in out)
  (do ((c (read-char in nil 'eof) (read-char in nil 'eof))
       (printing t))
      ((eql c 'eof) 'done)
    (if (eql c #\%)       (setf printing nil))
    (if (eql c #\Newline) (setf printing t))
    (if printing (princ c out))))

(file-stripcomment "myfile" "myfile.sans")
	
;;;;4 define a function that takes a two-dimensional array of floats
;;;; adn displays it in neat columns. Each element should be printed with
;;;; two digits after the decimal point, in a field 10 characters wide. 
;;;; (Assume all will fit.) You will need #'array-dimensions

(defun neatdisplay (farray)
  (let* ((ad (array-dimensions farray))
	 (rank (car ad))
	 (cols (cadr ad)))
    (dotimes (r rank)
      (dotimes (c cols)
	(format t "~10,2F "(aref farray r c)))
      (terpri))))

(neatdisplay #2A((1 2.598 3.1212 4) (3 4 5 2.3) (0 1 0 0)))


;;;;5 modify stream-subst to allow wildcards in the pattern.
;;;; if the character + occurs in old, it should match any character

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
	 (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
	       (mod n (length (buf-vec buf))))
	val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
	  (buf-new b)  (buf-end b))))
    
(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
	(buf-new b)  (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1
	(buf-used  b) -1
	(buf-new   b) -1
	(buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (stream-subst old new in out))))


(with-input-from-string (in "The struggle 1232 between Liberty and Authority is the most conspicuous feature in the portions of history with which we are earliest familiar, particularly in that of Greece, Rome, and England since 1972.")
  (with-output-to-string (out)
    (stream-subst (list :alpha #\Space :digit)  "***" in out)))

(defun match (c old pos)
  (let ((s (elt old pos)))
    (cond ((characterp s) (char= c s))
	  ((eq s :owt) t)
	  ((eq s :digit) (digit-char-p c))
	  ((eq s :alpha) (alpha-char-p c)))))
    
(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)
	    (or (setf from-buf (buf-next buf))
		(read-char in nil :eof))))
	((eql c :eof))
      (cond ((match c old pos)
	     (incf pos)
	     (cond ((= pos len)
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)
		    (buf-insert c buf))))
	    ((zerop pos)
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t
	     (unless from-buf
	       (buf-insert c buf))
	     (princ (buf-pop buf) out)
	     (buf-reset buf)
	     (setf pos 0))))
    (buf-flush buf out)))

(progn
  (setf source  (make-pathname :name "myfile"  )
	dest    (make-pathname :name "myfile.new"))

  (with-open-file (s source :direction :output :if-exists :supersede)
    (format s "The struggle between Liberty and Authority is the most conspicuous feature in the portions of history with which we are earliest familiar, particularly in that of Greece, Rome, and England.~%"))
  

  (file-subst "that of" "zzzzzz" source dest))







(with-output-to-string (str)
  (format str "yo")
  (format str " baby"))