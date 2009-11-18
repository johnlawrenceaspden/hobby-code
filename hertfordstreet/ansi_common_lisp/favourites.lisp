;my debugging macro
(defmacro dbg (a)
  (let ((b (gensym)))
  `(let ((,b ,a))
    (format t "~A -> ~A~%"  (quote ,a) ,b)
    ,b)))

;the macro-writing macro from Practical Common Lisp
(defmacro pcl-once-only ((&rest names) &body body)
   (let ((gensyms (loop for n in names collect (gensym))))
     `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	 ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	       ,@body)))))