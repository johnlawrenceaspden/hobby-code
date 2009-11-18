(setf path (make-pathname :name "myfile"))

(setf str (open path :direction :output
		     :if-exists :supersede))

(format str "something ~%")

(close str)


;(setf str (open path :direction :input))

;(read-line str)

;(close str)

(with-open-file (str path 
		     :direction :output
		     :if-exists :supersede)
  (format str "Something ~A~%" (random 100))
  (format str "Something ~A~%" (random 100))
  )

'(progn
  (format t "Please enter your name:")
  (read-line))

(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
	       (read-line str nil 'eof))
	 (acc nil))
	((eql line 'eof) (nreverse acc))
      (setf acc (cons line acc)))))

(pseudo-cat (make-pathname :name "5control.lisp"))


(defun lispy-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read str nil 'eof)
	       (read str nil 'eof))
	 (acc nil))
	((eql line 'eof) (nreverse acc))
      (setf acc (cons line acc)))))

(setf prog (lispy-cat (make-pathname :name "5control.lisp")))

(defmacro dbg (a)
  (let ((b (gensym)))
  `(let ((,b ,a))
    (format t "~A -> ~A~%"  (quote ,a) ,b)
    ,b)))

(read-from-string (read-line))


;;;;something about ring buffers and pattern matching

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

(progn
  (setf doom (new-buf 10))
  (buf-insert 'a doom)
  (buf-insert 'b doom)
  (buf-insert 'c doom)
  (buf-insert 'd doom)
  (dbg (buf-next doom))
  (dbg (buf-pop doom))
  (dbg (buf-next doom))
  (buf-flush doom t))


(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)
	    (or (setf from-buf (buf-next buf))
		(read-char in nil :eof))))
	((eql c :eof))
      (cond ((char= c (char old pos))
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

(setf source  (make-pathname :name "myfile"  )
      dest    (make-pathname :name "myfile.new"))

(with-open-file (s source :direction :output :if-exists :supersede)
  (format s "The struggle between Liberty and Authority is the most conspicuous feature in the portions of history with which we are earliest familiar, particularly in that of Greece, Rome, and England.~%"))


(file-subst "that of" "zzzzzz" source dest)

;;;;7.5 Macro Characters

(dolist (x (read-from-string "'a"))
  (princ x)
  (terpri))

(let ((*print-array* t))
   (read-from-string (dbg (format nil "~S" (vector 1 2 3)))))

