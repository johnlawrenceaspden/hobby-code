;;;;Here's a nice macro to do the hard work for me. Turns defclass back into defstruct!

(defun symbol-from-symbols(a b)
  (intern (concatenate 'string (symbol-name a) "-" (symbol-name b))))

(defun make-make-symbol (s)
  (intern (concatenate 'string "MAKE-" (symbol-name s))))

(defun slotgen (class slot)
  (let ()
    (if (atom slot)
	(list slot 
	      :accessor (symbol-from-symbols class slot)
	      :initarg  (intern (symbol-name slot) :keyword)
	      :initform nil)
	(list (car slot)
	      :accessor (symbol-from-symbols class (car slot)) 
	      :initarg  (intern (symbol-name (car slot)) :keyword)
	      :initform (cadr slot)))))

(defun slotsgen (class slots)
  (mapcar #'(lambda (x) (slotgen class x)) slots))

(defmacro define-class (class superclass slots)
  `(progn 
    (defclass ,class ,superclass ,(slotsgen class slots))
    (defun ,(make-make-symbol class) (&rest args) 
      (apply #'make-instance (cons ',class args)))))

(deftest
    (macroexpand-1 '(define-class circle () ((radius 1) (centre (cons 0 0)))))
    (progn 
      (defclass circle ()
	((radius :accessor circle-radius
		 :initarg :radius
		 :initform 1)
	 (centre :accessor circle-centre
		 :initarg :centre
		 :initform (cons 0 0))))
      (defun make-circle (&rest args) (apply #'make-instance (cons 'circle args)))))

