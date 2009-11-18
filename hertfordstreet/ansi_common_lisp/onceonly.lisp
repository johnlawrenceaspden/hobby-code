;;;;the macro writing macro from Practical Common Lisp
(defmacro pcl-once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	      ,@body)))))

;an example of its use
(defmacro square (x)
   (pcl-once-only (x)
     `(* ,x ,x)))

;this proves it works
(setf a 2)
(square (incf a))

;clues as to how it works
(macroexpand-1 '(pcl-once-only (x y z) `body))

(macroexpand-1 '(square a))